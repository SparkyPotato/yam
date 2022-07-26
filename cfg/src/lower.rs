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
	Field,
	Fn,
	Ident,
	Instr,
	InstrKind,
	RCtx,
	Rodeo,
	Struct,
	Ty,
	TyRef,
	Type,
	TypeId,
	Val,
	ValRef,
};

pub fn lower_to_cfg(ctx: resolved::Ctx, rodeo: &Rodeo, diagnostics: &mut Vec<Report<Span>>) -> RCtx {
	let mut lowerer = CfgLower {
		engine: TypeEngine::new(&ctx.types, &ctx.inbuilt_types, rodeo),
		types: &ctx.types,
		globals: &ctx.globals,
		inbuilts: &ctx.inbuilt_types,
		rodeo,
		diagnostics,
		locals: HashMap::new(),
	};

	RCtx {
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

		let block = self.tyck_fn(
			&args,
			&ret,
			f.ret.as_ref().map(|x| x.span).unwrap_or(Self::useless_span()),
			&f.block,
		);

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

	fn tyck_fn(
		&mut self, args: &[(Arg, LocalRef)], ret: &Type, ret_span: Span, block: &resolved::Block,
	) -> types::Block {
		let (block, span) = self.tyck_block(block);

		for (arg, r) in args {
			let id = self.insert_ty(&arg.ty);
			self.locals.insert(*r, id);
		}

		let ret = self.insert_ty(ret);
		self.engine.assign(ret, ret_span, block.ty, span, &mut self.diagnostics);

		self.engine.reset();
		self.locals.clear();

		block
	}

	fn tyck_block(&mut self, block: &resolved::Block) -> (types::Block, Span) {
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

			let expr = self.tyck_expr(expr, stmt.span);

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

	fn tyck_expr(&mut self, expr: &resolved::ValExprKind, span: Span) -> types::Expr {
		let (kind, ty) = match expr {
			ValExprKind::Lit(lit) => (types::ExprKind::Lit(*lit), self.tyck_lit(lit)),
			ValExprKind::Block(block) => {
				let (block, _) = self.tyck_block(block);
				let ty = block.ty;
				(types::ExprKind::Block(block), ty)
			},
			ValExprKind::ValRef(val) => (types::ExprKind::ValRef(*val), self.tyck_val(val)),
			ValExprKind::LocalRef(local) => (types::ExprKind::LocalRef(*local), self.locals[local]),
			ValExprKind::Let(l) => {
				let init_ty = l.expr.as_ref().map(|x| (self.tyck_expr(&x.node, x.span).ty, x.span));
				let given_ty = l.ty.as_ref().map(|x| (self.insert_ty(&self.lower_ty_expr(&x)), x.span));

				let ty = match (init_ty, given_ty) {
					(Some((rhs, rhs_span)), Some((lhs, lhs_span))) => {
						self.engine.assign(lhs, lhs_span, rhs, rhs_span, &mut self.diagnostics);
						lhs
					},
					(Some((ty, _)), None) | (None, Some((ty, _))) => ty,
					(None, None) => self.engine.insert(TypeInfo::Unknown),
				};

				match l.pat.node {
					resolved::PatKind::Binding(binding) => self.locals.insert(binding.binding, ty),
				}

				(
					types::ExprKind::Let(types::Let {
						pat: l.pat,
						expr: l.expr.map(|x| Box::new(self.tyck_expr(&x.node, x.span))),
						span: l.span,
					}),
					self.engine.insert(TypeInfo::Void),
				)
			},
			ValExprKind::List(_) => unreachable!("arrays are not supported"),
			ValExprKind::Array(_) => unreachable!("arrays are not supported"),
			ValExprKind::Cast(cast) => {
				let ty = self.insert_ty(&self.lower_ty_expr(&cast.ty));
				(
					types::Cast {
						expr: Box::new(self.tyck_expr(&cast.expr.node, cast.expr.span)),
						ty,
					},
					ty,
				)
			},
			ValExprKind::Fn(_) => unreachable!("closures are not supported"),
			ValExprKind::MacroRef(_) => unreachable!("macros are not supported"),
			ValExprKind::Call(_) => {},
			ValExprKind::Index(_) => {},
			ValExprKind::Access(_) => {},
			ValExprKind::Unary(_) => {},
			ValExprKind::Binary(_) => {},
			ValExprKind::Break(_) => {},
			ValExprKind::Continue(_) => {},
			ValExprKind::Return(_) => {},
			ValExprKind::If(_) => {},
			ValExprKind::Loop(_) => {},
			ValExprKind::While(_) => {},
			ValExprKind::For(_) => {},
			ValExprKind::Err => {},
		};

		types::Expr { kind, ty, span }
	}

	fn tyck_lit(&mut self, lit: &resolved::Lit) -> TypeId {
		self.engine.insert(match lit {
			resolved::Lit::Int(_) => TypeInfo::Int,
			resolved::Lit::Float(_) => TypeInfo::Float,
			resolved::Lit::Bool(_) => TypeInfo::Ty(self.inbuilts[&InbuiltType::Bool]),
			resolved::Lit::Char(_) => TypeInfo::Ty(self.inbuilts[&InbuiltType::Uint(32)]),
			resolved::Lit::String(_) => TypeInfo::Ptr {
				mutable: false,
				to: self.engine.insert(TypeInfo::Ty(self.inbuilts[&InbuiltType::Uint(8)])),
			},
		})
	}

	fn tyck_val(&mut self, val: &resolved::ValRef) -> TypeId {
		match &self.globals[val] {
			resolved::Val::Fn(f) => {
				let args = f
					.args
					.iter()
					.map(|x| self.insert_ty(&self.lower_ty_expr(&x.ty)))
					.collect();
				let ret = f
					.ret
					.as_ref()
					.map(|x| self.insert_ty(&self.lower_ty_expr(&x)))
					.unwrap_or(self.engine.insert(TypeInfo::Void));
				self.engine.insert(TypeInfo::Fn { args, ret })
			},
			resolved::Val::Const(_) => unreachable!("consts are not supported"),
			resolved::Val::Static(_) => unreachable!("statics are not supported"),
		}
	}

	fn insert_ty(&mut self, ty: &Type) -> TypeId {
		self.engine.insert(match ty {
			Type::Void => TypeInfo::Void,
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
		})
	}

	fn useless_span() -> Span {
		Span {
			start: 0,
			end: 0,
			file: Spur::default(),
		}
	}
}
