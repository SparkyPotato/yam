use hir::{
	ctx::Hir,
	hir::{BinOp, Block, ExprData, ExprKind, Fn, PatKind, StmtKind, UnOp, ValDefKind},
	types::Type,
};
use id::SparseMapBuilder;

use crate::{
	builder::FnBuilder,
	ssir::{InstrKind, Ssir, Value},
};

mod builder;
pub mod pretty;
pub mod ssir;

pub fn lower(hir: Hir) -> Ssir {
	let mut values = SparseMapBuilder::new();
	let mut tys = SparseMapBuilder::new();

	let mut lowerer = Lowerer {
		builder: FnBuilder::new(),
		loop_start: None,
		loop_end: None,
	};

	for (r, val) in hir.globals.into_iter() {
		if let Some(lang) = hir.val_to_lang_item.get(r) {
			tys.add(
				r,
				ssir::TyDef {
					path: val.path,
					kind: ssir::TyDefKind::LangItem(*lang),
					span: val.span,
				},
			);
		} else {
			match val.kind {
				ValDefKind::Static(_) => unreachable!("statics are not supported"),
				ValDefKind::Const(_) => unreachable!("consts are not supported"),
				ValDefKind::Fn(f) => {
					values.add(
						r,
						ssir::ValDef {
							path: val.path,
							kind: ssir::ValDefKind::Fn(lowerer.lower_fn(f)),
							span: val.span,
						},
					);
				},
				ValDefKind::FnDecl(sig) => {
					values.add(
						r,
						ssir::ValDef {
							path: val.path,
							kind: ssir::ValDefKind::FnDecl(sig),
							span: val.span,
						},
					);
				},
				ValDefKind::Struct(s) => {
					tys.add(
						r,
						ssir::TyDef {
							path: val.path,
							kind: ssir::TyDefKind::Struct(s),
							span: val.span,
						},
					);
				},
			}
		}
	}

	Ssir {
		rodeo: hir.rodeo,
		values: values.build(),
		tys: tys.build(),
	}
}

pub struct Lowerer {
	builder: FnBuilder,
	loop_start: Option<ssir::Block>,
	loop_end: Option<ssir::Block>,
}

impl Lowerer {
	pub fn lower_fn(&mut self, f: Fn) -> ssir::Fn {
		let block = self.builder.fn_init_block(f.sig.args);
		self.builder.set_block(block);

		let ret = self.lower_block(f.block);
		self.builder.instr(InstrKind::Ret(ret), Type::Void);

		let ret = ssir::Fn {
			abi: f.sig.abi,
			ret: f.sig.ret,
			blocks: self.builder.build(),
		};

		self.builder.reset();

		ret
	}

	pub fn lower_block(&mut self, block: Block) -> Value {
		let mut last_val = None;

		for stmt in block.stmts {
			match stmt.node {
				StmtKind::Expr(expr) => last_val = Some(self.lower_expr(expr)),
				StmtKind::Semi(expr) => {
					self.lower_expr(expr);
				},
				StmtKind::Err => unreachable!("error statement was found but compilation wasn't aborted before"),
			}
		}

		if let Some(last_val) = last_val {
			last_val
		} else {
			self.builder.instr(InstrKind::Void, Type::Void)
		}
	}

	pub fn lower_expr(&mut self, expr: ExprData) -> Value {
		let kind = match expr.kind {
			ExprKind::ValRef(v) => InstrKind::Global(v),
			ExprKind::LocalRef(l) => return self.builder.get_var(l),
			ExprKind::Never | ExprKind::Type | ExprKind::TypeOf(_) | ExprKind::Ptr(_) | ExprKind::Infer => {
				unreachable!("expected only value expressions")
			},
			ExprKind::Tuple(_) => unreachable!("tuples unsupported"),
			ExprKind::Lit(l) => InstrKind::Literal(l),
			ExprKind::Block(b) => return self.lower_block(b),
			ExprKind::Let(l) => {
				let r = match l.pat.node {
					PatKind::Binding(b) => b.binding,
				};
				let value = self.lower_expr(l.expr.expect("expected initializer").node);
				self.builder.add_var(r, l.ty, value);

				InstrKind::Void
			},
			ExprKind::List(_) | ExprKind::Array(_) => unreachable!("arrays unsupported"),
			ExprKind::Cast(c) => InstrKind::Cast(self.lower_expr(c.expr.node)),
			ExprKind::Fn(_) => unreachable!("closures unsupported"),
			ExprKind::Call(c) => {
				let target = self.lower_expr(c.target.node);
				let args = c.args.into_iter().map(|e| self.lower_expr(e.node)).collect();
				InstrKind::Call { target, args }
			},
			ExprKind::Index(_) => unreachable!("indexing unsupported"),
			ExprKind::Access(_) => unreachable!("field access unsupported"),
			ExprKind::Unary(u) => {
				let e = self.lower_expr(u.expr.node);
				let op = match u.op {
					UnOp::Not => ssir::UnOp::Not,
					UnOp::Neg => ssir::UnOp::Neg,
					UnOp::Addr => ssir::UnOp::Addr,
					UnOp::DoubleAddr => {
						let first = self.builder.instr(
							InstrKind::Unary {
								op: ssir::UnOp::Addr,
								value: e,
							},
							Self::deref(&expr.ty),
						);

						return self.builder.instr(
							InstrKind::Unary {
								op: ssir::UnOp::Addr,
								value: first,
							},
							expr.ty,
						);
					},
					UnOp::AddrMut => ssir::UnOp::AddrMut,
					UnOp::DoubleAddrMut => {
						let first = self.builder.instr(
							InstrKind::Unary {
								op: ssir::UnOp::AddrMut,
								value: e,
							},
							Self::deref(&expr.ty),
						);

						return self.builder.instr(
							InstrKind::Unary {
								op: ssir::UnOp::Addr,
								value: first,
							},
							expr.ty,
						);
					},
					UnOp::Deref => ssir::UnOp::Deref,
				};

				InstrKind::Unary { op, value: e }
			},
			ExprKind::Binary(b) => {
				let op = match b.op {
					BinOp::Add => ssir::BinOp::Add,
					BinOp::Sub => ssir::BinOp::Sub,
					BinOp::Mul => ssir::BinOp::Mul,
					BinOp::Div => ssir::BinOp::Div,
					BinOp::Rem => ssir::BinOp::Rem,
					BinOp::Shl => ssir::BinOp::Shl,
					BinOp::Shr => ssir::BinOp::Shr,
					BinOp::Lt => ssir::BinOp::Lt,
					BinOp::Gt => ssir::BinOp::Gt,
					BinOp::Leq => ssir::BinOp::Leq,
					BinOp::Geq => ssir::BinOp::Geq,
					BinOp::Eq => ssir::BinOp::Eq,
					BinOp::Neq => ssir::BinOp::Neq,
					BinOp::BitAnd => ssir::BinOp::BitAnd,
					BinOp::BitOr => ssir::BinOp::BitOr,
					BinOp::BitXor => ssir::BinOp::BitXor,
					BinOp::And => ssir::BinOp::And,
					BinOp::Or => ssir::BinOp::Or,
					BinOp::Assign => {
						let l = match b.lhs.node.kind {
							ExprKind::LocalRef(l) => l,
							_ => unreachable!("unsupported left side of assignment"),
						};

						let rhs = self.lower_expr(b.rhs.node);
						self.builder.mutate_var(l, rhs);

						return self.builder.instr(InstrKind::Void, Type::Void);
					},
					BinOp::AddAssign
					| BinOp::SubAssign
					| BinOp::MulAssign
					| BinOp::DivAssign
					| BinOp::RemAssign
					| BinOp::BitAndAssign
					| BinOp::BitOrAssign
					| BinOp::BitXorAssign
					| BinOp::ShlAssign
					| BinOp::ShrAssign
					| BinOp::PlaceConstruct => unreachable!("unsupported ops"),
				};

				let left = self.lower_expr(b.lhs.node);
				let right = self.lower_expr(b.rhs.node);

				InstrKind::Binary { op, left, right }
			},
			ExprKind::Break(b) => {
				let loop_block = self.loop_end.unwrap();
				let value = b
					.map(|x| self.lower_expr(x.node))
					.unwrap_or_else(|| self.builder.instr(InstrKind::Void, Type::Void));
				InstrKind::Jump {
					to: loop_block,
					args: vec![value],
				}
			},
			ExprKind::Continue => InstrKind::Jump {
				to: self.loop_start.unwrap(),
				args: Vec::new(),
			},
			ExprKind::Return(r) => {
				let ret = r
					.map(|x| self.lower_expr(x.node))
					.unwrap_or_else(|| self.builder.instr(InstrKind::Void, Type::Void));
				InstrKind::Ret(ret)
			},
			ExprKind::If(if_) => {
				let cond = self.lower_expr(if_.cond.node);
				let if_block = self.builder.add_block();
				let else_block = self.builder.add_block();
				let end = self.builder.add_block();

				self.builder.instr(
					InstrKind::JumpIf {
						cond,
						to: if_block,
						args: Vec::new(),
					},
					Type::Void,
				);
				self.builder.instr(
					InstrKind::Jump {
						to: else_block,
						args: Vec::new(),
					},
					Type::Void,
				);

				self.builder.set_block(if_block);
				let value = self.lower_block(if_.then);
				self.builder.instr(
					InstrKind::Jump {
						to: end,
						args: vec![value],
					},
					Type::Void,
				);

				self.builder.set_block(else_block);
				let value = if let Some(else_) = if_.else_ {
					self.lower_expr(else_.node)
				} else {
					self.builder.instr(InstrKind::Void, Type::Void)
				};
				self.builder.instr(
					InstrKind::Jump {
						to: end,
						args: vec![value],
					},
					Type::Void,
				);

				self.builder.set_block(end);
				return self.builder.add_arg(expr.ty);
			},
			ExprKind::Loop(l) => {
				let loop_block = self.builder.add_block();
				let end_block = self.builder.add_block();

				self.builder.instr(
					InstrKind::Jump {
						to: loop_block,
						args: Vec::new(),
					},
					Type::Void,
				);

				self.builder.set_block(loop_block);
				self.loop_start = Some(loop_block);
				self.loop_end = Some(end_block);
				self.lower_block(l.block);

				if let Some(w) = l.while_ {
					let cond = self.lower_expr(w.node);
					self.builder.instr(
						InstrKind::JumpIf {
							cond,
							to: loop_block,
							args: Vec::new(),
						},
						Type::Void,
					);
					self.builder.instr(
						InstrKind::Jump {
							to: end_block,
							args: Vec::new(),
						},
						Type::Void,
					);
				} else {
					self.builder.instr(
						InstrKind::Jump {
							to: loop_block,
							args: Vec::new(),
						},
						Type::Void,
					);
				}

				self.builder.set_block(end_block);
				return self.builder.add_arg(expr.ty);
			},
			ExprKind::While(w) => {
				let cond_block = self.builder.add_block();
				let loop_block = self.builder.add_block();
				let end_block = self.builder.add_block();

				self.builder.instr(
					InstrKind::Jump {
						to: cond_block,
						args: Vec::new(),
					},
					Type::Void,
				);

				self.builder.set_block(cond_block);
				let cond = self.lower_expr(w.cond.node);
				self.builder.instr(
					InstrKind::JumpIf {
						cond,
						to: loop_block,
						args: Vec::new(),
					},
					Type::Void,
				);
				self.builder.instr(
					InstrKind::Jump {
						to: end_block,
						args: Vec::new(),
					},
					Type::Void,
				);

				self.builder.set_block(loop_block);
				self.loop_start = Some(loop_block);
				self.loop_end = Some(end_block);
				self.lower_block(w.block);
				self.builder.instr(
					InstrKind::Jump {
						to: cond_block,
						args: Vec::new(),
					},
					Type::Void,
				);

				self.builder.set_block(end_block);
				return self.builder.instr(InstrKind::Void, Type::Void);
			},
			ExprKind::For(_) => unreachable!("for loops are not supported"),
			ExprKind::Err => unreachable!("error expression found but compilation wasn't aborted before"),
		};

		self.builder.instr(kind, expr.ty)
	}

	fn deref(ty: &Type) -> Type {
		match ty {
			Type::Ptr { to, .. } => Type::clone(to),
			_ => unreachable!("expected pointer type"),
		}
	}
}
