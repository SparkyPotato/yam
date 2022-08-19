use hir::{
	ctx::Hir,
	hir::{BinOp, Block, ExprData, ExprKind, Fn, PatKind, StmtKind, UnOp, ValDefKind},
	types::Type,
};
use id::SparseMapBuilder;

use crate::{
	builder::FnBuilder,
	ssir::{Ssir, Value},
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
		inv_lang_item: hir.val_to_lang_item,
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
		self.builder.ret(ret);

		let ret = ssir::Fn {
			abi: f.sig.abi,
			ret: f.sig.ret,
			blocks: self.builder.build(),
		};

		self.builder.reset();

		ret
	}

	pub fn lower_block(&mut self, block: Block) -> Option<Value> {
		let mut last_val = None;

		for stmt in block.stmts {
			match stmt.node {
				StmtKind::Expr(expr) => last_val = self.lower_expr(expr),
				StmtKind::Semi(expr) => {
					self.lower_expr(expr);
					last_val = None;
				},
				StmtKind::Err => unreachable!("error statement was found but compilation wasn't aborted before"),
			}
		}

		last_val
	}

	pub fn lower_expr(&mut self, expr: ExprData) -> Option<Value> {
		Some(match expr.kind {
			ExprKind::ValRef(v) => self.builder.global(v, expr.ty),
			ExprKind::LocalRef(l) => return self.builder.get_var(l),
			ExprKind::Never | ExprKind::Type | ExprKind::TypeOf(_) | ExprKind::Ptr(_) | ExprKind::Infer => {
				unreachable!("expected only value expressions")
			},
			ExprKind::Tuple(_) => unreachable!("tuples unsupported"),
			ExprKind::Lit(l) => self.builder.lit(l, expr.ty),
			ExprKind::Block(b) => return self.lower_block(b),
			ExprKind::Let(l) => {
				let r = match l.pat.node {
					PatKind::Binding(b) => b.binding,
				};
				self.builder.add_var(r, l.ty);

				if let Some(expr) = l.expr {
					let value = self.lower_expr(expr.node);
					self.builder.set_var(r, value);
				}

				return None;
			},
			ExprKind::List(_) | ExprKind::Array(_) => unreachable!("arrays unsupported"),
			ExprKind::Cast(c) => {
				let value = self.lower_expr(c.expr.node).expect("cannot cast void");
				self.builder.cast(value, expr.ty)
			},
			ExprKind::Fn(_) => unreachable!("closures unsupported"),
			ExprKind::Call(c) => {
				let target = self.lower_expr(c.target.node).expect("cannot call void");
				let args = c
					.args
					.into_iter()
					.map(|e| self.lower_expr(e.node).expect("cannot pass void params"))
					.collect();
				self.builder.call(target, args, expr.ty)
			},
			ExprKind::Index(_) => unreachable!("indexing unsupported"),
			ExprKind::Access(_) => unreachable!("field access unsupported"),
			ExprKind::Unary(u) => {
				let e = self
					.lower_expr(u.expr.node)
					.expect("cannot apply unary operator to void");
				let op = match u.op {
					UnOp::Not => ssir::UnOp::Not,
					UnOp::Neg => ssir::UnOp::Neg,
					UnOp::Addr => ssir::UnOp::Addr,
					UnOp::DoubleAddr => {
						let first = self.builder.unary(ssir::UnOp::Addr, e, Self::deref(&expr.ty));
						return Some(self.builder.unary(ssir::UnOp::Addr, first, expr.ty));
					},
					UnOp::AddrMut => ssir::UnOp::AddrMut,
					UnOp::DoubleAddrMut => {
						let first = self.builder.unary(ssir::UnOp::AddrMut, e, Self::deref(&expr.ty));
						return Some(self.builder.unary(ssir::UnOp::Addr, first, expr.ty));
					},
					UnOp::Deref => ssir::UnOp::Deref,
				};

				self.builder.unary(op, e, expr.ty)
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
						let rhs = self.lower_expr(b.rhs.node);

						let l = match b.lhs.node.kind {
							ExprKind::LocalRef(l) => l,
							_ => unreachable!("unsupported left side of assignment"),
						};

						self.builder.set_var(l, rhs);

						return None;
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

				let left = self
					.lower_expr(b.lhs.node)
					.expect("cannot apply binary operator to void");
				let right = self
					.lower_expr(b.rhs.node)
					.expect("cannot apply binary operator to void");

				self.builder.binary(left, op, right, expr.ty)
			},
			ExprKind::Break(b) => {
				let loop_block = self.loop_end.unwrap();
				let value = b.map(|x| self.lower_expr(x.node)).flatten();

				if let Some(value) = value {
					self.builder.jump(loop_block, vec![value]);
				} else {
					self.builder.jump(loop_block, Vec::new());
				}

				return None;
			},
			ExprKind::Continue => {
				self.builder.jump(self.loop_start.unwrap(), Vec::new());
				return None;
			},
			ExprKind::Return(r) => {
				let ret = r.map(|x| self.lower_expr(x.node)).flatten();
				self.builder.ret(ret);
				return None;
			},
			ExprKind::If(if_) => {
				let cond = self.lower_expr(if_.cond.node).expect("cannot apply if to void");
				let if_block = self.builder.add_block();
				let else_block = self.builder.add_block();
				let end = self.builder.add_block();

				self.builder.jump_if(cond, if_block, Vec::new());
				self.builder.jump(else_block, Vec::new());

				self.builder.set_block(if_block);
				if let Some(value) = self.lower_block(if_.then) {
					self.builder.jump(end, vec![value]);
				} else {
					self.builder.jump(end, Vec::new());
				}

				self.builder.set_block(else_block);
				if let Some(value) = if_.else_.map(|x| self.lower_expr(x.node)).flatten() {
					self.builder.jump(end, vec![value]);
				} else {
					self.builder.jump(end, Vec::new());
				}

				self.builder.set_block(end);
				return if !matches!(expr.ty, Type::Void) {
					Some(self.builder.add_arg(expr.ty))
				} else {
					None
				};
			},
			ExprKind::Loop(l) => {
				let loop_block = self.builder.add_block();
				let end_block = self.builder.add_block();

				self.builder.jump(loop_block, Vec::new());

				self.builder.set_block(loop_block);
				self.loop_start = Some(loop_block);
				self.loop_end = Some(end_block);
				self.lower_block(l.block);

				if let Some(w) = l.while_ {
					let cond = self.lower_expr(w.node).expect("cannot apply while to void");
					self.builder.jump_if(cond, loop_block, Vec::new());
					self.builder.jump(end_block, Vec::new());
				} else {
					self.builder.jump(loop_block, Vec::new());
				}

				self.builder.set_block(end_block);
				return if !matches!(expr.ty, Type::Void) {
					Some(self.builder.add_arg(expr.ty))
				} else {
					None
				};
			},
			ExprKind::While(w) => {
				let cond_block = self.builder.add_block();
				let loop_block = self.builder.add_block();
				let end_block = self.builder.add_block();

				self.builder.jump(cond_block, Vec::new());

				self.builder.set_block(cond_block);
				let cond = self.lower_expr(w.cond.node).expect("cannot apply while to void");
				self.builder.jump_if(cond, loop_block, Vec::new());
				self.builder.jump(end_block, Vec::new());

				self.builder.set_block(loop_block);
				self.loop_start = Some(loop_block);
				self.loop_end = Some(end_block);
				self.lower_block(w.block);
				self.builder.jump(cond_block, Vec::new());

				self.builder.set_block(end_block);
				return None;
			},
			ExprKind::For(_) => unreachable!("for loops are not supported"),
			ExprKind::Err => unreachable!("error expression found but compilation wasn't aborted before"),
		})
	}

	fn deref(ty: &Type) -> Type {
		match ty {
			Type::Ptr { to, .. } => Type::clone(to),
			_ => unreachable!("expected pointer type"),
		}
	}
}
