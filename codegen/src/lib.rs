use std::collections::HashMap;

use cfg::{BinOp, Ctx, Fn, InbuiltType, InstrKind, Lit, Ty, TyRef, Val, ValRef};
use cranelift::{
	codegen::ir::{types, Function},
	prelude::*,
};
use target_lexicon::Triple;

pub fn codegen(ctx: Ctx) {
	let triple = Triple::host();
	let builder = settings::builder();
	let flags = settings::Flags::new(builder);
	let isa = isa::lookup(triple.clone()).unwrap().finish(flags.clone()).unwrap();

	let mut codegen = Codegen {
		triple,
		flags,
		isa: &*isa,
		globals: &ctx.globals,
		types: &ctx.types,
	};

	let mut cctx = codegen::Context::new();
	let mut fctx = FunctionBuilderContext::new();

	for (v, val) in ctx.globals.iter() {
		let f = codegen.gen_val(&mut fctx, v.0, val);
		cctx.func = f;
		let info = cctx.compile(&*isa).unwrap();
	}
}

struct Codegen<'a> {
	triple: Triple,
	flags: settings::Flags,
	isa: &'a dyn isa::TargetIsa,
	globals: &'a HashMap<ValRef, Val>,
	types: &'a HashMap<TyRef, Ty>,
}

impl Codegen<'_> {
	fn gen_val(&mut self, ctx: &mut FunctionBuilderContext, index: u32, val: &Val) -> Function {
		match val {
			Val::Fn(f) => self.gen_fn(ctx, index, f),
		}
	}

	fn gen_fn(&mut self, ctx: &mut FunctionBuilderContext, index: u32, f: &Fn) -> Function {
		let mut sig = Signature::new(isa::CallConv::Fast);
		for arg in f.blocks[0].args.iter() {
			sig.params.push(AbiParam::new(self.ty_to_cranelift(&arg.ty).0));
		}
		sig.returns.push(AbiParam::new(self.ty_to_cranelift(&f.ret).0));

		let mut func = Function::with_name_signature(ExternalName::user(0, index), sig);
		let mut builder = FunctionBuilder::new(&mut func, ctx);

		let blocks: Vec<_> = f.blocks.iter().map(|_| builder.create_block()).collect();
		for (i, block) in f.blocks.iter().enumerate() {
			let b = blocks[i];
			for arg in block.args.iter() {
				builder.append_block_param(b, self.ty_to_cranelift(&arg.ty).0);
			}

			builder.switch_to_block(b);

			let mut values = Vec::with_capacity(block.instrs.len());
			for instr in block.instrs.iter() {
				let ins = builder.ins();
				let (ty, signed) = self.ty_to_cranelift(&instr.ty);

				let value = match instr.kind {
					InstrKind::Void => ins.null(ty),
					InstrKind::Literal(lit) => match lit {
						Lit::Bool(b) => ins.bconst(ty, b),
						Lit::Char(c) => ins.iconst(ty, c as i64),
						Lit::Float(f) => {
							if ty == types::F32 {
								ins.f32const(f as f32)
							} else {
								ins.f64const(f)
							}
						},
						Lit::Int(i) => ins.iconst(self.ty_to_cranelift(&instr.ty).0, i as i64),
						Lit::String(_) => unreachable!(),
					},
					InstrKind::Binary { left, op, right } => {
						let lhs = values[left.0 as usize];
						let rhs = values[right.0 as usize];
						match op {
							BinOp::Add => if ty.is_int() {
								ins.iadd(lhs, rhs)
							} else {
								ins.fadd(lhs, rhs)
							},
							BinOp::Sub => if ty.is_int() {
								ins.isub(lhs, rhs)
							} else {
								ins.fsub(lhs, rhs)
							},
							BinOp::Mul => if ty.is_int() {
								ins.imul(lhs, rhs)
							} else {
								ins.fmul(lhs, rhs)
							},
							BinOp::Div => if ty.is_int() {
								if signed {
									ins.sdiv(lhs, rhs)
								} else {
									ins.udiv(lhs, rhs)
								}
							} else {
								ins.fdiv(lhs, rhs)
							},
							BinOp::Rem => if ty.is_int() {
								if signed {
									ins.srem(lhs, rhs)
								} else {
									ins.urem(lhs, rhs)
								}
							} else {
								unreachable!()
							},
							BinOp::Shl => ins.ishl(lhs, rhs),
							BinOp::Shr => if signed {
								ins.sshr(lhs, rhs)
							} else {
								ins.ushr(lhs, rhs)
							},
							BinOp::Lt |
							BinOp::Gt |
							BinOp::Leq |
							BinOp::Geq |
							BinOp::Eq |
							BinOp::Neq => unreachable!(),
							BinOp::BitAnd => ins.band(lhs, rhs),
							BinOp::BitOr => ins.bor(lhs, rhs),
							BinOp::BitXor => ins.bxor(lhs, rhs),
							BinOp::And |
							BinOp::Or => unreachable!(),
							BinOp::Assign |
							BinOp::AddAssign |
							BinOp::SubAssign |
							BinOp::MulAssign |
							BinOp::DivAssign |
							BinOp::RemAssign |
							BinOp::BitAndAssign |
							BinOp::BitOrAssign |
							BinOp::BitXorAssign |
							BinOp::ShlAssign |
							BinOp::ShrAssign |
							BinOp::PlaceConstruct => unreachable!()
						}
					},
					InstrKind::Arg(id) => builder.block_params(b)[id.0 as usize],
					InstrKind::Ret(id) => {
						if let Some(id) = id {
							ins.return_(&[values[id.0 as usize]])
						} else {
							ins.return_(&[])
						};

						continue
					},
					_ => unreachable!()
					// InstrKind::Global(_) => {},
					// InstrKind::Call { .. } => {},
					// InstrKind::Cast(_) => {},
					// InstrKind::Unary { .. } => {},,
					// InstrKind::Jmp { .. } => {},
					// InstrKind::CondJmp { .. } => {},
				};

				values.push(value);
			}
		}

		builder.seal_all_blocks();
		builder.finalize();

		let mut s = String::new();
		codegen::write_function(&mut s, &func);
		println!("{}", s);

		codegen::verify_function(&func, &self.flags).expect("generated invalid function");

		func
	}

	fn ty_to_cranelift(&mut self, ty: &cfg::Type) -> (Type, bool) {
		match ty {
			cfg::Type::Void => unreachable!("void type"),
			cfg::Type::Never => unreachable!("never type"),
			cfg::Type::Fn { .. } => (Type::triple_pointer_type(&self.triple), false),
			cfg::Type::TyRef(t) => match self.types[t] {
				Ty::Struct(_) => unreachable!("no"),
				Ty::Inbuilt(i) => match i {
					InbuiltType::Int(x) => {
						let x = if x == 0 {
							self.triple.architecture.pointer_width().unwrap().bits()
						} else {
							x
						};
						(Type::int(x as _).unwrap(), true)
					},
					InbuiltType::Uint(x) => {
						let x = if x == 0 {
							self.triple.architecture.pointer_width().unwrap().bits()
						} else {
							x
						};
						(Type::int(x as _).unwrap(), false)
					},
					InbuiltType::Bool => (Type::int(1).unwrap().as_bool(), false),
					InbuiltType::Float(x) => {
						if x == 32 {
							(types::F32, false)
						} else {
							(types::F64, false)
						}
					},
				},
			},
			cfg::Type::Ptr { .. } => (Type::triple_pointer_type(&self.triple), false),
			cfg::Type::Err => (types::INVALID, false),
		}
	}
}
