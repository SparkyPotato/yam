use std::collections::HashMap;

use diag::{
	ariadne::{Label, Report, ReportKind},
	Span,
};
use name_resolve::{
	resolved,
	resolved::{InbuiltType, Lit, LocalRef, Spur, ValExprKind},
};

use crate::{
	types,
	types::{TypeEngine, TypeInfo},
	Arg,
	BasicBlock,
	BinOp,
	Ctx,
	Field,
	Fn,
	Rodeo,
	Struct,
	Ty,
	TyRef,
	Type,
	TypeId,
	UnOp,
	Val,
	ValRef,
};

pub fn lower_to_cfg(ctx: resolved::Ctx, rodeo: &Rodeo, diagnostics: &mut Vec<Report<Span>>) -> Ctx {
	let mut lowerer = CfgLower {
		engine: TypeEngine::new(&ctx.types, &ctx.inbuilt_types, rodeo),
		types: &ctx.types,
		globals: &ctx.globals,
		inbuilts: &ctx.inbuilt_types,
		rodeo,
		diagnostics,
		locals: HashMap::new(),
	};

	Ctx {
		types: ctx.types.iter().map(|(r, ty)| (*r, lowerer.lower_ty(ty))).collect(),
		globals: ctx
			.globals
			.iter()
			.map(|(r, val)| (*r, lowerer.lower_val(val)))
			.collect(),
		inbuilt_types: ctx.inbuilt_types,
	}
}

struct CfgLower<'a> {
	engine: TypeEngine<'a>,
	types: &'a HashMap<TyRef, resolved::Ty>,
	globals: &'a HashMap<ValRef, resolved::Val>,
	inbuilts: &'a HashMap<InbuiltType, TyRef>,
	rodeo: &'a Rodeo,
	diagnostics: &'a mut Vec<Report<Span>>,
	locals: HashMap<LocalRef, TypeId>,
}

impl CfgLower<'_> {
	fn lower_ty(&mut self, ty: &resolved::Ty) -> Ty {
		match ty {
			resolved::Ty::Inbuilt(i) => Ty::Inbuilt(*i),
			resolved::Ty::Struct(s) => Ty::Struct(Struct {
				path: s.path.clone(),
				fields: s
					.fields
					.iter()
					.map(|x| Field {
						name: x.name,
						ty: self.lower_ty_expr(&x.ty),
						span: x.span,
					})
					.collect(),
				span: s.span,
			}),
		}
	}

	fn lower_val(&mut self, val: &resolved::Val) -> Val {
		match val {
			resolved::Val::Const(_) => unreachable!("consts are not supported"),
			resolved::Val::Static(_) => unreachable!("statics are not supported"),
			resolved::Val::Fn(f) => Val::Fn(self.lower_fn(f)),
		}
	}

	fn lower_fn(&mut self, f: &resolved::Fn) -> Fn {
		let ret = f.ret.as_ref().map(|x| self.lower_ty_expr(&x)).unwrap_or(Type::Void);
		let args: Vec<_> = f
			.args
			.iter()
			.map(|x| match &x.pat.node {
				resolved::PatKind::Binding(binding) => (
					Arg {
						ident: x.span,
						ty: self.lower_ty_expr(&x.ty),
					},
					binding.binding,
				),
			})
			.collect();

		let block = self.infer_fn(
			&args,
			&ret,
			f.ret.as_ref().map(|x| x.span).unwrap_or(Self::useless_span()),
			&f.block,
		);

		let mut blocks = Vec::new();
		self.lower_block(block, &mut blocks);

		Fn {
			path: f.path.clone(),
			ret,
			blocks,
			span: f.span,
		}
	}

	fn lower_ty_expr(&mut self, ty: &resolved::TyExpr) -> Type {
		match &ty.node {
			resolved::TyExprKind::Err => Type::Err,
			resolved::TyExprKind::TyRef(r) => Type::TyRef(*r),
			resolved::TyExprKind::Type => unreachable!("`type` is not supported"),
			resolved::TyExprKind::TypeOf(_) => unreachable!("typeof is not supported"),
			resolved::TyExprKind::Ptr(ptr) => Type::Ptr {
				mutable: ptr.mutability,
				to: Box::new(self.lower_ty_expr(&ptr.to)),
			},
			resolved::TyExprKind::Tuple(_) => unreachable!("tuples are not supported"),
		}
	}

	fn lower_block(&mut self, block: types::Block, blocks: &mut Vec<BasicBlock>) {}

	fn infer_fn(
		&mut self, args: &[(Arg, LocalRef)], ret: &Type, ret_span: Span, block: &resolved::Block,
	) -> types::Block {
		let (block, span) = self.infer_block(block);

		for (arg, r) in args {
			let id = self.insert_ty(&arg.ty);
			self.locals.insert(*r, id);
		}

		let ret = self.insert_ty(ret);
		self.engine.unify(ret, ret_span, block.ty, span, &mut self.diagnostics);

		self.engine.reset();
		self.locals.clear();

		block
	}

	fn infer_block(&mut self, block: &resolved::Block) -> (types::Block, Span) {
		self.engine.push_scope();

		let mut exprs = Vec::with_capacity(block.stmts.len());
		let mut ty = None;
		let mut last_span = Self::useless_span();

		let last_expr = block.stmts.len().wrapping_sub(1);
		for (i, stmt) in block.stmts.iter().enumerate() {
			let (expr, last) = match &stmt.node {
				resolved::StmtKind::Expr(expr) => {
					if i != last_expr {
						self.diagnostics.push(
							stmt.span
								.report(ReportKind::Error)
								.with_message("expected `;` after statement")
								.with_label(Label::new(stmt.span).with_message("consider putting a `;` after this"))
								.finish(),
						);

						(expr, false)
					} else {
						(expr, true)
					}
				},
				resolved::StmtKind::Semi(expr) => (expr, false),
				resolved::StmtKind::Err => continue,
			};

			let expr = self.infer_expr(expr, stmt.span);

			if last {
				ty = Some(expr.ty);
				last_span = stmt.span;
			}

			exprs.push(expr);
		}

		self.engine.pop_scope();

		(
			types::Block {
				exprs,
				ty: ty.unwrap(),
				span: block.span,
			},
			last_span,
		)
	}

	fn infer_expr(&mut self, expr: &ValExprKind, span: Span) -> types::Expr {
		let (kind, ty) = match expr {
			ValExprKind::Lit(lit) => (types::ExprKind::Lit(*lit), self.infer_lit(lit)),
			ValExprKind::Block(block) => {
				let (block, _) = self.infer_block(block);
				let ty = block.ty;
				(types::ExprKind::Block(block), ty)
			},
			ValExprKind::ValRef(val) => (types::ExprKind::ValRef(*val), self.infer_val(val)),
			ValExprKind::LocalRef(local) => (types::ExprKind::LocalRef(*local), self.locals[local]),
			ValExprKind::Let(l) => {
				let init_ty = l.expr.as_ref().map(|x| (self.infer_expr(&x.node, x.span).ty, x.span));
				let given_ty = l.ty.as_ref().map(|x| {
					let ty = self.lower_ty_expr(&x);
					(self.insert_ty(&ty), x.span)
				});

				let ty = match (init_ty, given_ty) {
					(Some((rhs, rhs_span)), Some((lhs, lhs_span))) => {
						self.engine.unify(lhs, lhs_span, rhs, rhs_span, &mut self.diagnostics);
						lhs
					},
					(Some((ty, _)), None) | (None, Some((ty, _))) => ty,
					(None, None) => self.engine.insert(TypeInfo::Unknown),
				};

				match l.pat.node {
					resolved::PatKind::Binding(binding) => self.locals.insert(binding.binding, ty),
				};

				(
					types::ExprKind::Let(types::Let {
						pat: l.pat,
						expr: l.expr.as_ref().map(|x| Box::new(self.infer_expr(&x.node, x.span))),
						span: l.span,
					}),
					self.engine.insert(TypeInfo::Void),
				)
			},
			ValExprKind::List(_) => unreachable!("arrays are not supported"),
			ValExprKind::Array(_) => unreachable!("arrays are not supported"),
			ValExprKind::Cast(cast) => {
				let ty = self.lower_ty_expr(&cast.ty);
				let ty = self.insert_ty(&ty);
				(
					types::ExprKind::Cast(types::Cast {
						expr: Box::new(self.infer_expr(&cast.expr.node, cast.expr.span)),
						ty,
					}),
					ty,
				)
			},
			ValExprKind::Fn(_) => unreachable!("closures are not supported"),
			ValExprKind::MacroRef(_) => unreachable!("macros are not supported"),
			ValExprKind::Call(call) => {
				let target = self.infer_expr(&call.target.node, call.target.span);
				let ty = target.ty;
				(
					types::ExprKind::Call(types::Call {
						target: Box::new(target),
						args: call
							.args
							.iter()
							.map(|x| self.infer_expr(Self::expr_to_val(&x.node), x.span))
							.collect(),
					}),
					self.engine.insert(TypeInfo::FnRet(ty)),
				)
			},
			ValExprKind::Index(_) => unreachable!("indexing is not supported"),
			ValExprKind::Access(_) => unreachable!("field access is not supported"),
			ValExprKind::Unary(unary) => {
				let expr = self.infer_expr(&unary.expr.node, unary.expr.span);
				let ty = match unary.op {
					UnOp::Not => self.engine.insert(TypeInfo::Ref(expr.ty)),
					UnOp::Neg => self.engine.insert(TypeInfo::Ref(expr.ty)),
					UnOp::Addr => self.engine.insert(TypeInfo::Ptr {
						mutable: false,
						to: expr.ty,
					}),
					UnOp::DoubleAddr => {
						let to = self.engine.insert(TypeInfo::Ptr {
							mutable: false,
							to: expr.ty,
						});
						self.engine.insert(TypeInfo::Ptr { mutable: false, to })
					},
					UnOp::AddrMut => self.engine.insert(TypeInfo::Ptr {
						mutable: true,
						to: expr.ty,
					}),
					UnOp::DoubleAddrMut => {
						let to = self.engine.insert(TypeInfo::Ptr {
							mutable: true,
							to: expr.ty,
						});
						self.engine.insert(TypeInfo::Ptr { mutable: false, to })
					},
					UnOp::Deref => self.engine.insert(TypeInfo::Deref(expr.ty)),
				};
				(
					types::ExprKind::Unary(types::Unary {
						op: unary.op,
						expr: Box::new(expr),
					}),
					ty,
				)
			},
			ValExprKind::Binary(binary) => {
				let lhs = self.infer_expr(&binary.lhs.node, binary.lhs.span);
				let rhs = self.infer_expr(&binary.rhs.node, binary.rhs.span);
				let ty = match binary.op {
					BinOp::Add
					| BinOp::Sub
					| BinOp::Mul
					| BinOp::Div
					| BinOp::Rem
					| BinOp::Shl
					| BinOp::Shr
					| BinOp::BitAnd
					| BinOp::BitOr
					| BinOp::BitXor => self.engine.insert(TypeInfo::Ref(lhs.ty)),
					BinOp::Lt
					| BinOp::Gt
					| BinOp::Leq
					| BinOp::Geq
					| BinOp::Eq
					| BinOp::Neq
					| BinOp::And
					| BinOp::Or => self.engine.insert(TypeInfo::Ty(self.inbuilts[&InbuiltType::Bool])),
					BinOp::Assign
					| BinOp::AddAssign
					| BinOp::SubAssign
					| BinOp::MulAssign
					| BinOp::DivAssign
					| BinOp::RemAssign
					| BinOp::BitAndAssign
					| BinOp::BitOrAssign
					| BinOp::BitXorAssign
					| BinOp::ShlAssign
					| BinOp::ShrAssign => {
						self.engine
							.unify(lhs.ty, lhs.span, rhs.ty, rhs.span, &mut self.diagnostics);
						self.engine.insert(TypeInfo::Void)
					},
					BinOp::PlaceConstruct => unreachable!("placement construction is not supported"),
				};
				(
					types::ExprKind::Binary(types::Binary {
						op: binary.op,
						lhs: Box::new(lhs),
						rhs: Box::new(rhs),
					}),
					ty,
				)
			},
			ValExprKind::Break(br) => (
				types::ExprKind::Break(
					br.as_ref()
						.map(|x| Box::new(self.infer_expr(Self::expr_to_val(&x.node), x.span))),
				),
				self.engine.insert(TypeInfo::Never),
			),
			ValExprKind::Continue(c) => (
				types::ExprKind::Continue(
					c.as_ref()
						.map(|x| Box::new(self.infer_expr(Self::expr_to_val(&x.node), x.span))),
				),
				self.engine.insert(TypeInfo::Never),
			),
			ValExprKind::Return(ret) => (
				types::ExprKind::Return(
					ret.as_ref()
						.map(|x| Box::new(self.infer_expr(Self::expr_to_val(&x.node), x.span))),
				),
				self.engine.insert(TypeInfo::Never),
			),
			ValExprKind::If(if_) => {
				let cond = Box::new(self.infer_expr(&if_.cond.node, if_.cond.span));
				let (block, span) = self.infer_block(&if_.then);
				let els = if_
					.else_
					.as_ref()
					.map(|x| self.infer_expr(Self::expr_to_val(&x.node), x.span));

				let ty = if let Some(els) = &els {
					self.engine
						.unify(block.ty, span, els.ty, els.span, &mut self.diagnostics);
					block.ty
				} else {
					self.engine.insert(TypeInfo::Void)
				};

				(
					types::ExprKind::If(types::If {
						cond,
						then: block,
						else_: els.map(Box::new),
					}),
					ty,
				)
			},
			ValExprKind::Loop(loop_) => {
				let (block, _) = self.infer_block(&loop_.block);
				(
					types::ExprKind::Loop(types::Loop {
						block,
						while_: loop_
							.while_
							.as_ref()
							.map(|x| Box::new(self.infer_expr(&x.node, x.span))),
					}),
					self.engine.insert(TypeInfo::Void),
				)
			},
			ValExprKind::While(w) => {
				let (block, _) = self.infer_block(&w.block);
				(
					types::ExprKind::While(types::While {
						cond: Box::new(self.infer_expr(&w.cond.node, w.cond.span)),
						block,
					}),
					self.engine.insert(TypeInfo::Void),
				)
			},
			ValExprKind::For(_) => unreachable!("for loops are not supported"),
			ValExprKind::Err => unreachable!("already filtered out"),
		};

		types::Expr { kind, ty, span }
	}

	fn infer_lit(&mut self, lit: &Lit) -> TypeId {
		let ty = match lit {
			Lit::Int(_) => TypeInfo::Int,
			Lit::Float(_) => TypeInfo::Float,
			Lit::Bool(_) => TypeInfo::Ty(self.inbuilts[&InbuiltType::Bool]),
			Lit::Char(_) => TypeInfo::Ty(self.inbuilts[&InbuiltType::Uint(32)]),
			Lit::String(_) => {
				let r = self.inbuilts[&InbuiltType::Uint(8)];
				TypeInfo::Ptr {
					mutable: false,
					to: self.engine.insert(TypeInfo::Ty(r)),
				}
			},
		};
		self.engine.insert(ty)
	}

	fn infer_val(&mut self, val: &ValRef) -> TypeId {
		match &self.globals[val] {
			resolved::Val::Fn(f) => {
				let args = f
					.args
					.iter()
					.map(|x| {
						let ty = self.lower_ty_expr(&x.ty);
						self.insert_ty(&ty)
					})
					.collect();
				let ret = f
					.ret
					.as_ref()
					.map(|x| {
						let ty = self.lower_ty_expr(&x);
						self.insert_ty(&ty)
					})
					.unwrap_or(self.engine.insert(TypeInfo::Void));
				self.engine.insert(TypeInfo::Fn { args, ret })
			},
			resolved::Val::Const(_) => unreachable!("consts are not supported"),
			resolved::Val::Static(_) => unreachable!("statics are not supported"),
		}
	}

	fn insert_ty(&mut self, ty: &Type) -> TypeId {
		let ty = match ty {
			Type::Void => TypeInfo::Void,
			Type::Never => TypeInfo::Never,
			Type::Fn { args, ret } => TypeInfo::Fn {
				args: args.iter().map(|x| self.insert_ty(x)).collect(),
				ret: self.insert_ty(&ret),
			},
			Type::TyRef(r) => TypeInfo::Ty(*r),
			Type::Ptr { mutable, to } => TypeInfo::Ptr {
				mutable: *mutable,
				to: self.insert_ty(&to),
			},
			Type::Err => TypeInfo::Unknown,
		};
		self.engine.insert(ty)
	}

	fn expr_to_val(expr: &resolved::ExprKind) -> &ValExprKind {
		match expr {
			resolved::ExprKind::Val(val) => val,
			resolved::ExprKind::Err => &resolved::ValExprKind::Err,
			_ => unreachable!("type exprs are not supported in this position"),
		}
	}

	fn useless_span() -> Span {
		Span {
			start: 0,
			end: 0,
			file: Spur::default(),
		}
	}
}
