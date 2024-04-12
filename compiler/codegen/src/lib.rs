use std::cell::RefCell;

use arena::{dense::DenseMap, Ix};
use cranelift::{
	codegen::{
		ir::StackSlot,
		isa::{CallConv, TargetFrontendConfig},
	},
	prelude::*,
};
use cranelift_module::{DataDescription, DataId, FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use hir::{
	ident::{AbsPath, DebugAbsPath},
	LangItem,
};
use parking_lot::{RwLock, RwLockWriteGuard};
use rustc_hash::FxHashMap;
pub use target_lexicon as target;
use thir::Thir;
use verde::{Db, Id};

#[derive(Clone)]
pub struct CodegenOptions {
	pub name: String,
	pub target: target::Triple,
	pub verify_ir: bool,
	pub emit_ir: bool,
}

thread_local! {
	static CONTEXT: RefCell<codegen::Context> = RefCell::new(codegen::Context::new());
	static FN_BUILDER: RefCell<FunctionBuilderContext> = RefCell::new(FunctionBuilderContext::new());
}

#[derive(Copy, Clone)]
pub struct Layout {
	pub size: u64,
	pub align: u64,
}

pub struct StructLayout {
	pub layout: Layout,
	pub offsets: DenseMap<hir::Param, u64>,
}

pub enum Item {
	Struct(StructLayout),
	Enum,
	Fn(FuncId),
	TypeAlias,
	Static(DataId),
}

pub struct DeclaredPackage {
	pub items: FxHashMap<Id<AbsPath>, Item>,
	module: RwLock<ObjectModule>,
}

impl DeclaredPackage {
	pub fn finish(self) -> Vec<u8> { self.module.into_inner().finish().emit().unwrap() }
}

pub fn layout_of_type(
	db: &dyn Db, target: &target::Triple, thir: &Thir, tys: &FxHashMap<Id<AbsPath>, Item>, ty: Id<thir::Type>,
) -> (Layout, bool) {
	match *db.geti(ty) {
		thir::Type::Array(ref a) => {
			let (elem, _) = layout_of_type(db, target, thir, tys, a.ty);
			let size = elem.size * a.len;
			let align = elem.align;
			(Layout { size, align }, true)
		},
		thir::Type::Fn(_) | thir::Type::Ptr(_) => {
			let size = target.pointer_width().unwrap().bytes() as u64;
			(Layout { size, align: size }, false)
		},
		thir::Type::Struct(ref s) => match tys.get(s) {
			Some(&Item::Struct(ref s)) => (s.layout, true),
			_ => match db.get(thir.decls[s]).kind {
				thir::ItemDeclKind::Struct(ref s) => {
					let mut size = 0;
					let mut align = 0;
					for (_, &ty) in s.fields.iter() {
						let (ty, _) = layout_of_type(db, target, thir, tys, ty);
						size += ty.size;
						align = align.max(ty.align);
					}
					(Layout { size, align }, true)
				},
				_ => unreachable!(),
			},
		},
		thir::Type::Enum(ref e) => match db.get(thir.decls[e]).kind {
			thir::ItemDeclKind::Enum(ref e) => (layout_of_lang_item(e.repr), false),
			_ => unreachable!(),
		},
		thir::Type::LangItem(l) => (layout_of_lang_item(l), false),
		thir::Type::Void | thir::Type::Error => (Layout { size: 0, align: 0 }, false),
	}
}

pub fn layout_of_lang_item(item: LangItem) -> Layout {
	match item {
		LangItem::U8 | LangItem::I8 | LangItem::Bool | LangItem::Char => Layout { size: 1, align: 1 },
		LangItem::U16 | LangItem::I16 => Layout { size: 2, align: 2 },
		LangItem::U32 | LangItem::I32 | LangItem::F32 => Layout { size: 4, align: 4 },
		LangItem::U64 | LangItem::I64 | LangItem::F64 => Layout { size: 8, align: 8 },
		LangItem::U128 | LangItem::I128 => Layout { size: 16, align: 16 },
	}
}

pub fn callconv_of_abi(target: &target::Triple, abi: Option<&'static str>) -> CallConv {
	match abi {
		Some(abi) => match abi {
			"C" => CallConv::triple_default(target),
			"systemv" => CallConv::SystemV,
			"win64" => CallConv::WindowsFastcall,
			_ => unreachable!("unknown abi"),
		},
		None => CallConv::Fast,
	}
}

pub fn sig_of_type(
	db: &dyn Db, target: &target::Triple, thir: &Thir, items: &FxHashMap<Id<AbsPath>, Item>, ty: Id<thir::Type>,
) -> Signature {
	let t = db.geti(ty);
	let thir::Type::Fn(ref f) = *t else { unreachable!() };
	let mut sig = Signature::new(callconv_of_abi(target, f.abi));
	let is_extern = f.abi.is_some();
	for &ty in f.params.iter() {
		let (layout, compound) = layout_of_type(db, target, thir, items, ty);
		sig.params.push(AbiParam::new(if compound {
			if is_extern {
				unreachable!("cannot pass compound type to extern fn");
			}
			Type::triple_pointer_type(target)
		} else {
			Type::int_with_byte_size(layout.size as _).unwrap()
		}));
	}
	let (ret, compound) = layout_of_type(db, target, thir, items, f.ret);
	if compound {
		if is_extern {
			unreachable!("cannot return compound type from extern fn");
		}
		sig.params.push(AbiParam::new(Type::triple_pointer_type(target)));
	} else if ret.size > 0 {
		sig.returns
			.push(AbiParam::new(Type::int_with_byte_size(ret.size as _).unwrap()));
	}

	sig
}

pub fn codegen_declare(db: &dyn Db, options: &CodegenOptions, thir: &Thir) -> DeclaredPackage {
	let mut builder = settings::builder();
	builder.enable("is_pic").unwrap();
	builder.enable("enable_atomics").unwrap();
	builder.set("opt_level", "speed").unwrap();

	let flags = settings::Flags::new(builder);
	let isa = isa::lookup(options.target.clone()).unwrap().finish(flags).unwrap();
	let mut module = ObjectModule::new(
		ObjectBuilder::new(isa, options.name.clone(), cranelift_module::default_libcall_names()).unwrap(),
	);

	let mut items = FxHashMap::default();
	for (&path, &decl) in thir.decls.iter() {
		let item = match db.get(decl).kind {
			thir::ItemDeclKind::Struct(ref s) => {
				let mut size = 0;
				let mut align = 0;
				let mut offsets = DenseMap::default();

				for (p, &ty) in s.fields.iter() {
					let (ty, _) = layout_of_type(db, &options.target, thir, &items, ty);

					size = (size + (ty.align - 1)) & !(ty.align - 1);
					offsets.insert(p, size);

					size += ty.size;
					align = align.max(ty.align);
				}

				Item::Struct(StructLayout {
					layout: Layout { size, align },
					offsets,
				})
			},
			thir::ItemDeclKind::Enum(ref e) => Item::Enum,
			thir::ItemDeclKind::Fn(ref f) => {
				let hir = db.get(thir.hir[&path]);
				let hir = match hir.kind {
					hir::ItemKind::Fn(ref f) => f,
					_ => unreachable!(),
				};

				let sig = sig_of_type(db, &options.target, thir, &items, f.ty);
				let id = module
					.declare_function(
						&if sig.call_conv != CallConv::Fast {
							hir.name.name.as_str().to_string()
						} else {
							format!("{:?}", path.debug(db))
						},
						if hir.abi.is_some() {
							if hir.body.is_some() {
								Linkage::Export
							} else {
								Linkage::Import
							}
						} else {
							Linkage::Local
						},
						&sig,
					)
					.unwrap();
				Item::Fn(id)
			},
			thir::ItemDeclKind::TypeAlias(ref t) => Item::TypeAlias,
			thir::ItemDeclKind::Static(ref s) => {
				let id = module
					.declare_data(&format!("{:?}", path.debug(db)), Linkage::Local, true, true)
					.unwrap();
				Item::Static(id)
			},
		};

		items.insert(path, item);
	}

	DeclaredPackage {
		items,
		module: RwLock::new(module),
	}
}

pub fn codegen_item(
	db: &dyn Db, options: &CodegenOptions, decls: &DeclaredPackage, thir: &Thir, hir: Id<hir::Item>,
	ithir: Id<thir::Item>,
) {
	let hir = db.get(hir);
	let ithir = db.get(ithir);

	CONTEXT.with(|cranelift| {
		FN_BUILDER.with(|builder| {
			let mut cranelift = cranelift.borrow_mut();
			cranelift.clear();
			let mut builder = builder.borrow_mut();
			let mut codegen = Codegen {
				db,
				options,
				hir: &*hir,
				ithir: &*ithir,
				decls,
				thir,
				params: DenseMap::default(),
				locals: DenseMap::default(),
				frontend: decls.module.read().target_config(),
				entry: Block::new(0),
				loop_: None,
			};

			match hir.kind {
				hir::ItemKind::Fn(ref f) => codegen.fn_(&mut *cranelift, &mut *builder, f),
				hir::ItemKind::Static(ref s) => codegen.static_(s),
				_ => {},
			}
		})
	});
}

pub struct Codegen<'a> {
	db: &'a dyn Db,
	options: &'a CodegenOptions,
	hir: &'a hir::Item,
	ithir: &'a thir::Item,
	decls: &'a DeclaredPackage,
	thir: &'a Thir,
	params: DenseMap<hir::Param, StackSlot>,
	locals: DenseMap<hir::Local, StackSlot>,
	frontend: TargetFrontendConfig,
	entry: Block,
	loop_: Option<(Block, Block)>,
}

impl<'a> Codegen<'a> {
	pub fn fn_(
		&'a mut self, cranelift: &'a mut codegen::Context, builder: &'a mut FunctionBuilderContext, hir: &hir::Fn,
	) {
		if let Some(body) = hir.body.as_ref() {
			let decl = self.db.get(self.ithir.decl);
			let thir::ItemDeclKind::Fn(ref sig) = decl.kind else {
				unreachable!()
			};
			cranelift.func.signature = sig_of_type(self.db, &self.options.target, self.thir, &self.decls.items, sig.ty);

			let mut builder = FunctionBuilder::new(&mut cranelift.func, builder);
			let entry = builder.create_block();
			builder.append_block_params_for_function_params(entry);
			builder.switch_to_block(entry);
			builder.seal_block(entry);

			for (i, (param, &ty)) in sig.params.iter().enumerate() {
				let (layout, _) = layout_of_type(self.db, &self.options.target, self.thir, &self.decls.items, ty);
				let slot =
					builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, layout.size as _));
				self.params.insert(param, slot);
				let ptr = builder
					.ins()
					.stack_addr(Type::triple_pointer_type(&self.options.target), slot, 0);
				builder.emit_small_memory_copy(
					self.frontend,
					ptr,
					builder.block_params(entry)[i],
					layout.size as _,
					layout.align as _,
					1,
					true,
					MemFlags::new(),
				);
			}
			for (local, &ty) in self.ithir.locals.iter() {
				let (layout, _) = layout_of_type(self.db, &self.options.target, self.thir, &self.decls.items, ty);
				let slot =
					builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, layout.size as _));
				self.locals.insert(local, slot);
			}

			self.entry = entry;
			if let Some((ret, ty)) = self.block(&mut builder, body) {
				let (layout, compound) =
					layout_of_type(self.db, &self.options.target, self.thir, &self.decls.items, ty);
				if compound {
					let &v = builder.block_params(entry).last().unwrap();
					builder.emit_small_memory_copy(
						self.frontend,
						v,
						ret,
						layout.size as _,
						layout.align as _,
						layout.align as _,
						true,
						MemFlags::new(),
					);
					builder.ins().return_(&[]);
				} else {
					builder.ins().return_(&[ret]);
				}
			} else {
				builder.ins().return_(&[]);
			}
			builder.finalize();

			cranelift.optimize(self.decls.module.read().isa()).unwrap();

			if self.options.emit_ir {
				println!("{}", cranelift.func);
			}
			if self.options.verify_ir {
				cranelift.verify(self.decls.module.write().isa()).unwrap();
			}

			let Item::Fn(id) = self.decls.items[&self.ithir.path] else {
				unreachable!()
			};
			self.decls.module.write().define_function(id, cranelift).unwrap();
		}
	}

	pub fn static_(&mut self, _: &hir::Static) {
		let Item::Static(id) = self.decls.items[&self.ithir.path] else {
			unreachable!()
		};
		self.decls
			.module
			.write()
			.define_data(id, &DataDescription::new())
			.unwrap();
	}

	pub fn const_eval(&mut self, expr: Ix<hir::Expr>) {
		unreachable!("bruh const eval is so dumb");
	}

	pub fn expr_place(
		&mut self, builder: &mut FunctionBuilder, expr: Ix<hir::Expr>,
	) -> Option<(Value, Id<thir::Type>, bool)> {
		let ty = self.ithir.exprs[expr];
		let expr = &self.hir.exprs[expr];

		match expr.kind {
			hir::ExprKind::Continue => {
				let (start, _) = self.loop_.unwrap();
				builder.ins().jump(start, &[]);
				None
			},
			hir::ExprKind::Array(ref arr) => {
				let (arr_layout, _) = layout_of_type(self.db, &self.options.target, self.thir, &self.decls.items, ty);
				let elem_ty = self.ithir.exprs[arr.elems[0]];
				let (elem_layout, _) =
					layout_of_type(self.db, &self.options.target, self.thir, &self.decls.items, elem_ty);
				let storage = builder
					.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, arr_layout.size as _));
				if arr.repeat {
					let count = arr_layout.size / elem_layout.size;
					let elem = self.expr(builder, arr.elems[0]).unwrap().0;
					for i in 0..count {
						self.store(builder, elem, elem_ty, storage, i as i32 * elem_layout.size as i32);
					}
				} else {
					for (i, &elem) in arr.elems.iter().enumerate() {
						let elem = self.expr(builder, elem).unwrap().0;
						self.store(builder, elem, elem_ty, storage, i as i32 * elem_layout.size as i32);
					}
				}
				Some((
					builder
						.ins()
						.stack_addr(Type::triple_pointer_type(&self.options.target), storage, 0),
					ty,
					true,
				))
			},
			hir::ExprKind::Let(ref l) => {
				if let Some((value, ty)) = l.init.and_then(|x| self.expr(builder, x)) {
					self.store(builder, value, ty, self.locals[l.local], 0);
				} else {
					self.zero(builder, ty, self.locals[l.local], 0);
				}
				None
			},
			hir::ExprKind::Block(ref b) => self.block(builder, b).map(|(x, ty)| (x, ty, false)),
			hir::ExprKind::Infix(ref i) => {
				let rhs = self.expr(builder, i.rhs).unwrap().0;
				let t = self.db.geti(ty);
				let l = match *t {
					thir::Type::LangItem(l) => l,
					thir::Type::Ptr(_) => LangItem::U64,
					_ => unreachable!(),
				};
				let v = match i.op {
					hir::InfixOp::Or | hir::InfixOp::BitOr => {
						let (lhs, _) = self.expr(builder, i.lhs).unwrap();
						builder.ins().bor(lhs, rhs)
					},
					hir::InfixOp::And | hir::InfixOp::BitAnd => {
						let (lhs, _) = self.expr(builder, i.lhs).unwrap();
						builder.ins().band(lhs, rhs)
					},
					hir::InfixOp::Eq => {
						let (lhs, _) = self.expr(builder, i.lhs).unwrap();
						if matches!(l, LangItem::F32 | LangItem::F64) {
							builder.ins().fcmp(FloatCC::Equal, lhs, rhs)
						} else {
							builder.ins().icmp(IntCC::Equal, lhs, rhs)
						}
					},
					hir::InfixOp::NotEq => {
						let (lhs, _) = self.expr(builder, i.lhs).unwrap();
						if matches!(l, LangItem::F32 | LangItem::F64) {
							builder.ins().fcmp(FloatCC::NotEqual, lhs, rhs)
						} else {
							builder.ins().icmp(IntCC::NotEqual, lhs, rhs)
						}
					},
					hir::InfixOp::Lt => {
						let (lhs, _) = self.expr(builder, i.lhs).unwrap();
						if matches!(l, LangItem::F32 | LangItem::F64) {
							builder.ins().fcmp(FloatCC::LessThan, lhs, rhs)
						} else if matches!(
							l,
							LangItem::U8 | LangItem::U16 | LangItem::U32 | LangItem::U64 | LangItem::U128
						) {
							builder.ins().icmp(IntCC::UnsignedLessThan, lhs, rhs)
						} else {
							builder.ins().icmp(IntCC::SignedLessThan, lhs, rhs)
						}
					},
					hir::InfixOp::Leq => {
						let (lhs, _) = self.expr(builder, i.lhs).unwrap();
						if matches!(l, LangItem::F32 | LangItem::F64) {
							builder.ins().fcmp(FloatCC::LessThanOrEqual, lhs, rhs)
						} else if matches!(
							l,
							LangItem::U8 | LangItem::U16 | LangItem::U32 | LangItem::U64 | LangItem::U128
						) {
							builder.ins().icmp(IntCC::UnsignedLessThanOrEqual, lhs, rhs)
						} else {
							builder.ins().icmp(IntCC::SignedLessThanOrEqual, lhs, rhs)
						}
					},
					hir::InfixOp::Gt => {
						let (lhs, _) = self.expr(builder, i.lhs).unwrap();
						if matches!(l, LangItem::F32 | LangItem::F64) {
							builder.ins().fcmp(FloatCC::GreaterThan, lhs, rhs)
						} else if matches!(
							l,
							LangItem::U8 | LangItem::U16 | LangItem::U32 | LangItem::U64 | LangItem::U128
						) {
							builder.ins().icmp(IntCC::UnsignedGreaterThan, lhs, rhs)
						} else {
							builder.ins().icmp(IntCC::SignedGreaterThan, lhs, rhs)
						}
					},
					hir::InfixOp::Geq => {
						let (lhs, _) = self.expr(builder, i.lhs).unwrap();
						if matches!(l, LangItem::F32 | LangItem::F64) {
							builder.ins().fcmp(FloatCC::GreaterThanOrEqual, lhs, rhs)
						} else if matches!(
							l,
							LangItem::U8 | LangItem::U16 | LangItem::U32 | LangItem::U64 | LangItem::U128
						) {
							builder.ins().icmp(IntCC::UnsignedGreaterThanOrEqual, lhs, rhs)
						} else {
							builder.ins().icmp(IntCC::SignedGreaterThanOrEqual, lhs, rhs)
						}
					},
					hir::InfixOp::Add => {
						let (lhs, _) = self.expr(builder, i.lhs).unwrap();
						if matches!(l, LangItem::F32 | LangItem::F64) {
							builder.ins().fadd(lhs, rhs)
						} else {
							builder.ins().iadd(lhs, rhs)
						}
					},
					hir::InfixOp::Sub => {
						let (lhs, _) = self.expr(builder, i.lhs).unwrap();
						if matches!(l, LangItem::F32 | LangItem::F64) {
							builder.ins().fsub(lhs, rhs)
						} else {
							builder.ins().isub(lhs, rhs)
						}
					},
					hir::InfixOp::Mul => {
						let (lhs, _) = self.expr(builder, i.lhs).unwrap();
						if matches!(l, LangItem::F32 | LangItem::F64) {
							builder.ins().fmul(lhs, rhs)
						} else {
							builder.ins().imul(lhs, rhs)
						}
					},
					hir::InfixOp::Div => {
						let (lhs, _) = self.expr(builder, i.lhs).unwrap();
						if matches!(l, LangItem::F32 | LangItem::F64) {
							builder.ins().fdiv(lhs, rhs)
						} else {
							builder.ins().sdiv(lhs, rhs)
						}
					},
					hir::InfixOp::Mod => {
						let (lhs, _) = self.expr(builder, i.lhs).unwrap();
						builder.ins().srem(lhs, rhs)
					},
					hir::InfixOp::Shl => {
						let (lhs, _) = self.expr(builder, i.lhs).unwrap();
						builder.ins().ishl(lhs, rhs)
					},
					hir::InfixOp::Shr => {
						let (lhs, _) = self.expr(builder, i.lhs).unwrap();
						builder.ins().ushr(lhs, rhs)
					},
					hir::InfixOp::Xor => {
						let (lhs, _) = self.expr(builder, i.lhs).unwrap();
						builder.ins().bxor(lhs, rhs)
					},
					hir::InfixOp::Assign => {
						let (lhs, _, place) = self.expr_place(builder, i.lhs).unwrap();
						assert!(place, "cannot assign to non-place");
						self.val_store(builder, rhs, ty, lhs, 0);
						return None;
					},
					hir::InfixOp::AddAssign => {
						let (lhs, _, place) = self.expr_place(builder, i.lhs).unwrap();
						assert!(place, "cannot assign to non-place");
						let v = if matches!(l, LangItem::F32 | LangItem::F64) {
							builder.ins().fadd(lhs, rhs)
						} else {
							builder.ins().iadd(lhs, rhs)
						};
						self.val_store(builder, v, ty, lhs, 0);
						return None;
					},
					hir::InfixOp::SubAssign => {
						let (lhs, _, place) = self.expr_place(builder, i.lhs).unwrap();
						assert!(place, "cannot assign to non-place");
						let v = if matches!(l, LangItem::F32 | LangItem::F64) {
							builder.ins().fsub(lhs, rhs)
						} else {
							builder.ins().isub(lhs, rhs)
						};
						self.val_store(builder, v, ty, lhs, 0);
						return None;
					},
					hir::InfixOp::MulAssign => {
						let (lhs, _, place) = self.expr_place(builder, i.lhs).unwrap();
						assert!(place, "cannot assign to non-place");
						let v = if matches!(l, LangItem::F32 | LangItem::F64) {
							builder.ins().fmul(lhs, rhs)
						} else {
							builder.ins().imul(lhs, rhs)
						};
						self.val_store(builder, v, ty, lhs, 0);
						return None;
					},
					hir::InfixOp::DivAssign => {
						let (lhs, _, place) = self.expr_place(builder, i.lhs).unwrap();
						assert!(place, "cannot assign to non-place");
						let v = if matches!(l, LangItem::F32 | LangItem::F64) {
							builder.ins().fdiv(lhs, rhs)
						} else {
							builder.ins().sdiv(lhs, rhs)
						};
						self.val_store(builder, v, ty, lhs, 0);
						return None;
					},
					hir::InfixOp::ModAssign => {
						let (lhs, _, place) = self.expr_place(builder, i.lhs).unwrap();
						assert!(place, "cannot assign to non-place");
						let v = builder.ins().srem(lhs, rhs);
						self.val_store(builder, v, ty, lhs, 0);
						return None;
					},
					hir::InfixOp::ShlAssign => {
						let (lhs, _, place) = self.expr_place(builder, i.lhs).unwrap();
						assert!(place, "cannot assign to non-place");
						let v = builder.ins().ishl(lhs, rhs);
						self.val_store(builder, v, ty, lhs, 0);
						return None;
					},
					hir::InfixOp::ShrAssign => {
						let (lhs, _, place) = self.expr_place(builder, i.lhs).unwrap();
						assert!(place, "cannot assign to non-place");
						let v = builder.ins().ushr(lhs, rhs);
						self.val_store(builder, v, ty, lhs, 0);
						return None;
					},
					hir::InfixOp::XorAssign => {
						let (lhs, _, place) = self.expr_place(builder, i.lhs).unwrap();
						assert!(place, "cannot assign to non-place");
						let v = builder.ins().bxor(lhs, rhs);
						self.val_store(builder, v, ty, lhs, 0);
						return None;
					},
					hir::InfixOp::BitOrAssign => {
						let (lhs, _, place) = self.expr_place(builder, i.lhs).unwrap();
						assert!(place, "cannot assign to non-place");
						let v = builder.ins().bor(lhs, rhs);
						self.val_store(builder, v, ty, lhs, 0);
						return None;
					},
					hir::InfixOp::BitAndAssign => {
						let (lhs, _, place) = self.expr_place(builder, i.lhs).unwrap();
						assert!(place, "cannot assign to non-place");
						let v = builder.ins().band(lhs, rhs);
						self.val_store(builder, v, ty, lhs, 0);
						return None;
					},
					hir::InfixOp::Error => unreachable!(),
				};
				Some((v, ty, false))
			},
			hir::ExprKind::Break(ref b) => {
				let (_, end) = self.loop_.unwrap();
				if let &Some(b) = b {
					let (value, _) = self.expr(builder, b).unwrap();
					builder.ins().jump(end, &[value]);
				} else {
					builder.ins().jump(end, &[]);
				}
				None
			},
			hir::ExprKind::Call(ref c) => {
				let (callee, _) = self.expr(builder, c.callee).unwrap();
				let mut args = Vec::new();
				for &arg in c.args.iter() {
					let (arg, _) = self.expr(builder, arg).unwrap();
					args.push(arg);
				}

				let tt = self.ithir.exprs[c.callee];
				let t = self.db.geti(tt);
				let thir::Type::Fn(ref f) = *t else { unreachable!() };
				let (ret, compound) =
					layout_of_type(self.db, &self.options.target, self.thir, &self.decls.items, f.ret);
				let sig = sig_of_type(self.db, &self.options.target, self.thir, &self.decls.items, tt);
				let sig = builder.import_signature(sig);
				if compound {
					let ret =
						builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, ret.size as _));
					let ret = builder
						.ins()
						.stack_addr(Type::triple_pointer_type(&self.options.target), ret, 0);
					args.push(ret);
					builder.ins().call_indirect(sig, callee, &args);
					Some((ret, f.ret, false))
				} else if ret.size > 0 {
					let ret = builder.ins().call_indirect(sig, callee, &args);
					Some((builder.inst_results(ret)[0], f.ret, false))
				} else {
					None
				}
			},
			hir::ExprKind::Struct(ref s) => {
				let Item::Struct(ref l) = &self.decls.items[&s.struct_] else {
					unreachable!()
				};
				let storage = builder
					.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, l.layout.size as _));
				for ((_, &o), &v) in l.offsets.iter().zip(s.args.iter()) {
					let (v, ty) = self.expr(builder, v).unwrap();
					self.store(builder, v, ty, storage, o as _);
				}
				Some((
					builder
						.ins()
						.stack_addr(Type::triple_pointer_type(&self.options.target), storage, 0),
					ty,
					false,
				))
			},
			hir::ExprKind::Cast(_) => unreachable!(),
			hir::ExprKind::Field(ref f) => {
				let (v, sty, place) = self.expr_place(builder, f.expr).unwrap();
				let thir::Type::Struct(s) = *self.db.geti(sty) else {
					unreachable!()
				};
				let Item::Struct(ref l) = &self.decls.items[&s] else {
					unreachable!()
				};
				let h = self.db.get(self.thir.hir[&s]);
				let hir::ItemKind::Struct(ref s) = h.kind else {
					unreachable!()
				};
				let f = s
					.fields
					.ids_iter()
					.find(|(_, p)| p.name.name == f.field.name)
					.unwrap()
					.0;
				let off = l.offsets[f];
				let v = builder.ins().iadd_imm(v, off as i64);
				Some((v, ty, place))
			},
			hir::ExprKind::Index(ref i) => {
				let (v, ty, place) = self.expr_place(builder, i.expr).unwrap();
				let t = self.db.geti(ty);
				let thir::Type::Array(ref a) = *t else { unreachable!() };
				let (idx, _) = self.expr(builder, i.index).unwrap();
				let (elem, _) = layout_of_type(self.db, &self.options.target, self.thir, &self.decls.items, a.ty);
				let elem = builder.ins().imul_imm(idx, elem.size as i64);
				let v = builder.ins().iadd(v, elem);
				Some((v, ty, place))
			},
			hir::ExprKind::Literal(l) => match l {
				hir::Literal::Char(c) => {
					let v = builder.ins().iconst(Type::int_with_byte_size(1).unwrap(), c as i64);
					Some((v, ty, false))
				},
				hir::Literal::Int(i) => {
					let (ll, _) = layout_of_type(self.db, &self.options.target, self.thir, &self.decls.items, ty);
					let v = builder.ins().iconst(Type::int_with_byte_size(ll.size as _).unwrap(), i);
					Some((v, ty, false))
				},
				hir::Literal::Float(f) => {
					let (ll, _) = layout_of_type(self.db, &self.options.target, self.thir, &self.decls.items, ty);
					if ll.size == 4 {
						let v = builder.ins().f32const(f as f32);
						Some((v, ty, false))
					} else {
						let v = builder.ins().f64const(f);
						Some((v, ty, false))
					}
				},
				hir::Literal::Bool(b) => {
					let v = builder.ins().iconst(Type::int_with_byte_size(1).unwrap(), b as i64);
					Some((v, ty, false))
				},
				hir::Literal::String(s) => {
					let mut m = self.decls.module.write();
					let d = m.declare_anonymous_data(false, false).unwrap();

					let mut desc = DataDescription::new();
					let mut vec = Vec::with_capacity(s.len() + 1);
					vec.extend(s.bytes());
					vec.push(0);
					desc.define(vec.into());
					m.define_data(d, &desc).unwrap();

					let m = RwLockWriteGuard::downgrade(m);
					let v = m.declare_data_in_func(d, builder.func);
					let v = builder
						.ins()
						.global_value(Type::triple_pointer_type(&self.options.target), v);
					Some((v, ty, false))
				},
			},
			hir::ExprKind::Loop(_) => unreachable!(),
			hir::ExprKind::Match(_) => unreachable!(),
			hir::ExprKind::Fn(ref f) => {
				let Item::Fn(f) = self.decls.items[f] else {
					unreachable!()
				};
				let f = self.decls.module.write().declare_func_in_func(f, builder.func);
				let v = builder
					.ins()
					.func_addr(Type::triple_pointer_type(&self.options.target), f);
				Some((v, ty, false))
			},
			hir::ExprKind::Static(ref s) => {
				let Item::Static(s) = self.decls.items[s] else {
					unreachable!()
				};
				let v = self.decls.module.read().declare_data_in_func(s, builder.func);
				let v = builder
					.ins()
					.global_value(Type::triple_pointer_type(&self.options.target), v);
				Some((v, ty, true))
			},
			hir::ExprKind::Local(l) => {
				let v = builder
					.ins()
					.stack_addr(Type::triple_pointer_type(&self.options.target), self.locals[l], 0);
				Some((v, ty, true))
			},
			hir::ExprKind::Param(p) => {
				let v = builder
					.ins()
					.stack_addr(Type::triple_pointer_type(&self.options.target), self.params[p], 0);
				Some((v, ty, true))
			},
			hir::ExprKind::EnumVariant(ref e) => {
				let h = self.db.get(self.thir.hir[&e.path]);
				let hir::ItemKind::Enum(ref e) = h.kind else {
					unreachable!()
				};
				let tag = e.variants.ids_iter().find(|(_, v)| v.0.name == e.name.name).unwrap().0;
				let (layout, _) = layout_of_type(self.db, &self.options.target, self.thir, &self.decls.items, ty);
				let v = builder
					.ins()
					.iconst(Type::int_with_byte_size(layout.size as _).unwrap(), tag.index() as i64);
				Some((v, ty, false))
			},
			hir::ExprKind::Ref(ref f) => {
				let (v, ty, place) = self.expr_place(builder, f.expr).unwrap();
				let (_, compound) = layout_of_type(self.db, &self.options.target, self.thir, &self.decls.items, ty);
				assert!(place || compound, "cannot take reference of non-place");
				Some((v, ty, false))
			},
			hir::ExprKind::Prefix(ref p) => {
				let expr = self.expr(builder, p.expr).unwrap().0;
				match p.op {
					hir::PrefixOp::Deref => Some((expr, ty, true)),
					hir::PrefixOp::Neg => {
						let t = self.db.geti(ty);
						let thir::Type::LangItem(l) = *t else { unreachable!() };
						let v = if matches!(l, LangItem::F32 | LangItem::F64) {
							builder.ins().fneg(expr)
						} else {
							builder.ins().ineg(expr)
						};
						Some((v, ty, false))
					},
					hir::PrefixOp::Not => {
						let v = builder.ins().bnot(expr);
						Some((v, ty, false))
					},
					hir::PrefixOp::Error => unreachable!(),
				}
			},
			hir::ExprKind::Return(r) => {
				if let Some((ret, ty)) = r.and_then(|x| self.expr(builder, x)) {
					let (layout, compound) =
						layout_of_type(self.db, &self.options.target, self.thir, &self.decls.items, ty);
					if compound {
						let &v = builder.block_params(self.entry).last().unwrap();
						builder.emit_small_memory_copy(
							self.frontend,
							v,
							ret,
							layout.size as _,
							layout.align as _,
							layout.align as _,
							true,
							MemFlags::new(),
						);
						builder.ins().return_(&[]);
					} else {
						builder.ins().return_(&[ret]);
					}
				} else {
					builder.ins().return_(&[]);
				}
				None
			},
			hir::ExprKind::Error => unreachable!(),
		}
	}

	pub fn expr(&mut self, builder: &mut FunctionBuilder, expr: Ix<hir::Expr>) -> Option<(Value, Id<thir::Type>)> {
		let (v, ty, place) = self.expr_place(builder, expr)?;
		let (layout, compound) = layout_of_type(self.db, &self.options.target, self.thir, &self.decls.items, ty);
		if place && !compound {
			Some((
				builder.ins().load(
					Type::int_with_byte_size(layout.size as _).unwrap(),
					MemFlags::new(),
					v,
					0,
				),
				ty,
			))
		} else {
			Some((v, ty))
		}
	}

	pub fn block(&mut self, builder: &mut FunctionBuilder, block: &hir::Block) -> Option<(Value, Id<thir::Type>)> {
		for &expr in block.discard.iter() {
			self.expr(builder, expr);
		}
		block.value.and_then(|expr| self.expr(builder, expr))
	}

	pub fn store(
		&mut self, builder: &mut FunctionBuilder, value: Value, ty: Id<thir::Type>, slot: StackSlot, offset: i32,
	) {
		let (layout, compound) = layout_of_type(self.db, &self.options.target, self.thir, &self.decls.items, ty);
		if compound {
			let addr = builder
				.ins()
				.stack_addr(Type::triple_pointer_type(&self.options.target), slot, offset);
			builder.emit_small_memory_copy(
				self.frontend,
				addr,
				value,
				layout.size as _,
				layout.align as _,
				layout.align as _,
				true,
				MemFlags::new(),
			);
		} else {
			builder.ins().stack_store(value, slot, offset);
		}
	}

	pub fn val_store(
		&mut self, builder: &mut FunctionBuilder, value: Value, ty: Id<thir::Type>, slot: Value, offset: i32,
	) {
		let (layout, compound) = layout_of_type(self.db, &self.options.target, self.thir, &self.decls.items, ty);
		if compound {
			builder.emit_small_memory_copy(
				self.frontend,
				slot,
				value,
				layout.size as _,
				layout.align as _,
				layout.align as _,
				true,
				MemFlags::new(),
			);
		} else {
			builder.ins().store(MemFlags::new(), value, slot, offset);
		}
	}

	pub fn zero(&mut self, builder: &mut FunctionBuilder, ty: Id<thir::Type>, slot: StackSlot, offset: i32) {
		let (layout, _) = layout_of_type(self.db, &self.options.target, self.thir, &self.decls.items, ty);
		let addr = builder
			.ins()
			.stack_addr(Type::triple_pointer_type(&self.options.target), slot, offset);
		builder.emit_small_memset(
			self.frontend,
			addr,
			0,
			layout.size as _,
			layout.align as _,
			MemFlags::new(),
		);
	}
}
