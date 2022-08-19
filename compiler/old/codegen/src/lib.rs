use std::{collections::HashMap, io::Write};

use cranelift::{
	codegen::{
		ir::Function,
		settings::{Configurable, Flags},
		Context,
	},
	frontend::FunctionBuilderContext,
	prelude::{isa::CallConv, *},
};
use cranelift_module::{DataContext, FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use id::{DenseMap, Id, SparseMapBuilder};
use ssir::{
	pretty::SsirWriter,
	ssir::{
		Abi,
		BinOp,
		Block,
		Fn,
		FnSignature,
		Instr,
		LangItem,
		Lit,
		NonValueInstr,
		Ssir,
		Type,
		UnOp,
		ValDef,
		ValDefKind,
		ValRef,
		ValueInstr,
	},
};
use target_lexicon::Triple;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum OptimizeLevel {
	None,
	Basic,
	Full,
}

pub struct CodegenSettings<'a, W> {
	pub writer: &'a mut W,
	pub optimize: OptimizeLevel,
	pub triple: Triple,
}

pub fn native_triple() -> Triple { Triple::host() }

pub fn codegen<W: Write>(ssir: &Ssir, settings: CodegenSettings<W>) {
	let mut builder = cranelift::codegen::settings::builder();

	builder.enable("is_pic").unwrap();
	builder.enable("enable_simd").unwrap();
	builder.enable("enable_atomics").unwrap();
	builder.set("enable_heap_access_spectre_mitigation", "false").unwrap();
	builder.set("enable_table_access_spectre_mitigation", "false").unwrap();

	if settings.optimize == OptimizeLevel::Full {
		builder.set("opt_level", "speed").unwrap();
	}

	let flags = Flags::new(builder);
	let isa = isa::lookup(settings.triple).unwrap().finish(flags.clone()).unwrap();

	let module =
		ObjectModule::new(ObjectBuilder::new(isa, "yamout", cranelift_module::default_libcall_names()).unwrap());

	let mut codegen = Codegen {
		ssir,
		flags,
		ctx: module.make_context(),
		data: DataContext::new(),
		module,
		function_defs: HashMap::new(),
	};

	let mut fctx = FunctionBuilderContext::new();

	for (r, val) in ssir.values.iter() {
		codegen.decl_val(r, val);
	}

	for (r, val) in ssir.values.iter() {
		codegen.def_val(r, val, &mut fctx);
	}

	let output = codegen.module.finish();
	output.object.write_stream(settings.writer).unwrap();
}

pub struct Codegen<'a> {
	ssir: &'a Ssir,
	flags: Flags,
	ctx: Context,
	data: DataContext,
	module: ObjectModule,
	function_defs: HashMap<ValRef, (FuncId, Signature)>,
}

impl Codegen<'_> {
	fn decl_val(&mut self, r: ValRef, val: &ValDef) {
		let p = val
			.path
			.iter()
			.map(|x| self.ssir.resolve_intern(x.node))
			.collect::<Vec<_>>()
			.join(".");

		match &val.kind {
			ValDefKind::Fn(f) => {
				let mut sig = Signature::new(self.abi(&f.abi));
				for (_, arg) in f.blocks[Block::from_id(0)].args() {
					sig.params.push(AbiParam::new(
						self.ty_to_cranelift(arg).expect("invalid function arg type"),
					))
				}

				if let Some(ret) = self.ty_to_cranelift(&f.ret) {
					sig.returns.push(AbiParam::new(ret));
				};

				let id = self.module.declare_function(&p, Linkage::Export, &sig).unwrap();
				self.function_defs.insert(r, (id, sig));
			},
			ValDefKind::FnDecl(sig) => {
				let sig = self.sig(sig);

				let id = self.module.declare_function(&p, Linkage::Import, &sig).unwrap();
				self.function_defs.insert(r, (id, sig));
			},
		}
	}

	fn def_val(&mut self, r: ValRef, val: &ValDef, fctx: &mut FunctionBuilderContext) {
		match &val.kind {
			ValDefKind::Fn(f) => self.def_fn(r, f, fctx),
			ValDefKind::FnDecl(_) => {},
		}
	}

	fn def_fn(&mut self, r: ValRef, f: &Fn, fctx: &mut FunctionBuilderContext) {
		let (id, sig) = &self.function_defs[&r];

		let mut func = Function::with_name_signature(
			ExternalName::User {
				namespace: 0,
				index: r.id(),
			},
			sig.clone(),
		);
		let mut builder = FunctionBuilder::new(&mut func, fctx);

		let block_map = {
			let mut b = DenseMap::builder();
			for (id, _) in f.blocks.iter() {
				b.insert_at(id, builder.create_block());
			}

			b.build()
		};
		let mut value_map = DenseMap::builder();
		let mut sig_map = SparseMapBuilder::new();

		for (id, block) in f.blocks.iter() {
			let id = block_map[id];
			builder.switch_to_block(id);

			for (value, arg) in block.args() {
				let v = builder.append_block_param(id, self.ty_to_cranelift(arg).expect("invalid block arg type"));
				value_map.insert_at(value, v);
			}

			for (_, instr) in block.instrs() {
				match instr {
					Instr::Value { value, instr, ty } => {
						let ty = self.ty_to_cranelift(ty).unwrap_or(types::INVALID);

						let val = match instr {
							ValueInstr::Literal(l) => match l {
								Lit::Bool(b) => builder.ins().bconst(ty, *b),
								Lit::Char(c) => builder.ins().iconst(ty, *c as i64),
								Lit::Float(f) => builder.ins().f64const(*f),
								Lit::Int(i) => builder.ins().iconst(ty, *i as i64),
								Lit::String(s) => {
									let s = self.ssir.resolve_intern(*s);
									let id = self.module.declare_anonymous_data(false, false).unwrap();
									let mut null_terminated = Vec::with_capacity(s.len() + 1);
									null_terminated.extend_from_slice(s.as_bytes());
									null_terminated.push(0);
									self.data.define(null_terminated.into_boxed_slice());
									self.module.define_data(id, &self.data).unwrap();
									self.data.clear();
									let value = self.module.declare_data_in_func(id, &mut builder.func);
									builder.ins().global_value(ty, value)
								},
							},
							ValueInstr::Global(g) => {
								let (id, sig) = &self.function_defs[g];
								let r = self.module.declare_func_in_func(*id, &mut builder.func);
								let sig = builder.import_signature(sig.clone());
								sig_map.add(*value, sig);
								builder.ins().func_addr(ty, r)
							},
							ValueInstr::Call { target, args } => {
								let sig = sig_map[*target];
								let i = builder.ins().call_indirect(
									sig,
									value_map[*target],
									&args.iter().map(|x| value_map[*x]).collect::<Vec<_>>(),
								);
								if let Some(value) = builder.inst_results(i).get(0) {
									*value
								} else {
									continue;
								}
							},
							ValueInstr::Cast(c) => value_map[*c],
							ValueInstr::Unary { op, value } => match op {
								UnOp::Not => builder.ins().bnot(value_map[*value]),
								UnOp::Neg => {
									if ty.is_float() {
										builder.ins().fneg(value_map[*value])
									} else {
										builder.ins().ineg(value_map[*value])
									}
								},
								UnOp::Addr => unreachable!(""),
								UnOp::AddrMut => unreachable!(),
								UnOp::Deref => unreachable!(),
							},
							ValueInstr::Binary { left, op, right } => match op {
								BinOp::Add => {
									if ty.is_float() {
										builder.ins().fadd(value_map[*left], value_map[*right])
									} else {
										builder.ins().iadd(value_map[*left], value_map[*right])
									}
								},
								BinOp::Sub => {
									if ty.is_float() {
										builder.ins().fsub(value_map[*left], value_map[*right])
									} else {
										builder.ins().isub(value_map[*left], value_map[*right])
									}
								},
								BinOp::Mul => {
									if ty.is_float() {
										builder.ins().fmul(value_map[*left], value_map[*right])
									} else {
										builder.ins().imul(value_map[*left], value_map[*right])
									}
								},
								BinOp::Div => {
									if ty.is_float() {
										builder.ins().fdiv(value_map[*left], value_map[*right])
									} else {
										builder.ins().udiv(value_map[*left], value_map[*right])
									}
								},
								BinOp::Rem => {
									if ty.is_float() {
										unreachable!()
									} else {
										builder.ins().urem(value_map[*left], value_map[*right])
									}
								},
								BinOp::Shl => builder.ins().ishl(value_map[*left], value_map[*right]),
								BinOp::Shr => builder.ins().ushr(value_map[*left], value_map[*right]),
								BinOp::Lt => {
									if ty.is_float() {
										builder
											.ins()
											.fcmp(FloatCC::LessThan, value_map[*left], value_map[*right])
									} else {
										builder
											.ins()
											.icmp(IntCC::UnsignedLessThan, value_map[*left], value_map[*right])
									}
								},
								BinOp::Gt => {
									if ty.is_float() {
										builder
											.ins()
											.fcmp(FloatCC::GreaterThan, value_map[*left], value_map[*right])
									} else {
										builder.ins().icmp(
											IntCC::UnsignedGreaterThan,
											value_map[*left],
											value_map[*right],
										)
									}
								},
								BinOp::Leq => {
									if ty.is_float() {
										builder.ins().fcmp(
											FloatCC::LessThanOrEqual,
											value_map[*left],
											value_map[*right],
										)
									} else {
										builder.ins().icmp(
											IntCC::UnsignedLessThanOrEqual,
											value_map[*left],
											value_map[*right],
										)
									}
								},
								BinOp::Geq => {
									if ty.is_float() {
										builder.ins().fcmp(
											FloatCC::GreaterThanOrEqual,
											value_map[*left],
											value_map[*right],
										)
									} else {
										builder.ins().icmp(
											IntCC::UnsignedGreaterThanOrEqual,
											value_map[*left],
											value_map[*right],
										)
									}
								},
								BinOp::Eq => {
									if ty.is_float() {
										builder.ins().fcmp(FloatCC::Equal, value_map[*left], value_map[*right])
									} else {
										builder.ins().icmp(IntCC::Equal, value_map[*left], value_map[*right])
									}
								},
								BinOp::Neq => {
									if ty.is_float() {
										builder
											.ins()
											.fcmp(FloatCC::NotEqual, value_map[*left], value_map[*right])
									} else {
										builder.ins().icmp(IntCC::NotEqual, value_map[*left], value_map[*right])
									}
								},
								BinOp::BitAnd => builder.ins().band(value_map[*left], value_map[*right]),
								BinOp::BitOr => builder.ins().bor(value_map[*left], value_map[*right]),
								BinOp::BitXor => builder.ins().bxor(value_map[*left], value_map[*right]),
								BinOp::And => builder.ins().band(value_map[*left], value_map[*right]),
								BinOp::Or => builder.ins().bor(value_map[*left], value_map[*right]),
							},
						};

						value_map.insert_at(*value, val);
					},
					Instr::NonValue(instr) => match instr {
						NonValueInstr::Jump { to, args } => {
							builder
								.ins()
								.jump(block_map[*to], &args.iter().map(|x| value_map[*x]).collect::<Vec<_>>());
						},
						NonValueInstr::JumpIf { cond, to, args } => {
							builder.ins().brnz(
								value_map[*cond],
								block_map[*to],
								&args.iter().map(|x| value_map[*x]).collect::<Vec<_>>(),
							);
						},
						NonValueInstr::Ret(ret) => {
							if let Some(ret) = ret {
								builder.ins().return_(&[value_map[*ret]]);
							} else {
								builder.ins().return_(&[]);
							}
						},
					},
				}
			}
		}

		builder.seal_all_blocks();
		builder.finalize();

		if cfg!(debug_assertions) {
			if let Err(e) = codegen::verify_function(&func, &self.flags) {
				println!("codegen error");

				let mut s = String::new();

				SsirWriter::new(self.ssir, &mut s)
					.fn_(&self.ssir.values[r].path, f)
					.unwrap();
				println!("SSIR:\n{}", s);

				s.clear();

				codegen::write_function(&mut s, &func).unwrap();
				println!("CLIF:\n{}", s);

				panic!("errors: {:#?}", e);
			}
		}

		self.ctx.func = func;
		self.module.define_function(*id, &mut self.ctx).unwrap();
		self.ctx.clear();
	}

	fn sig(&mut self, sig: &FnSignature) -> Signature {
		let mut s = Signature::new(self.abi(&sig.abi));

		for arg in sig.args.iter() {
			s.params.push(AbiParam::new(
				self.ty_to_cranelift(&arg.ty.node.ty)
					.expect("invalid function arg type"),
			))
		}

		if let Some(ret) = self.ty_to_cranelift(&sig.ret) {
			s.returns.push(AbiParam::new(ret));
		};

		s
	}

	fn abi(&mut self, abi: &Abi) -> CallConv {
		match abi {
			Abi::None => CallConv::Fast,
			Abi::Extern => CallConv::Fast,
			Abi::Abi(abi) => {
				let s = self.ssir.resolve_intern(abi.node);
				match s {
					"C" => self.module.target_config().default_call_conv,
					"systemv" => CallConv::SystemV,
					"mac" => CallConv::AppleAarch64,
					"fastcall" => CallConv::WindowsFastcall,
					_ => unreachable!("unknown ABI: {}", s),
				}
			},
		}
	}

	fn ty_to_cranelift(&self, ty: &Type) -> Option<cranelift::prelude::Type> {
		match ty {
			Type::Void => None,
			Type::Never => None,
			Type::Type => None,
			Type::Tuple(_) => None,
			Type::Fn { .. } | Type::Ptr { .. } => Some(self.module.target_config().pointer_type()),
			Type::Ty(ty) => match self.ssir.inv_lang_item.get(*ty) {
				Some(x) => Some(match *x {
					LangItem::U8 | LangItem::I8 => types::I8,
					LangItem::U16 | LangItem::I16 => types::I16,
					LangItem::U32 | LangItem::I32 => types::I32,
					LangItem::U64 | LangItem::I64 => types::I64,
					LangItem::Usize | LangItem::Isize => self.module.target_config().pointer_type(),
					LangItem::F32 => types::F32,
					LangItem::F64 => types::F64,
					LangItem::Bool => types::B1,
					LangItem::Void => return None,
				}),
				None => unreachable!("structs unsupported"),
			},
			Type::Unresolved(_) | Type::Unknown | Type::Err => unreachable!("shouldn't be here"),
		}
	}
}
