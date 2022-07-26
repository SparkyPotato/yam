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
			ValExprKind::Block(_) => {},
			ValExprKind::ValRef(_) => {},
			ValExprKind::LocalRef(_) => {},
			ValExprKind::Let(_) => {},
			ValExprKind::List(_) => {},
			ValExprKind::Array(_) => {},
			ValExprKind::Cast(_) => {},
			ValExprKind::Fn(_) => {},
			ValExprKind::MacroRef(_) => {},
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

	fn tyck_lit(&mut self, lit: &resolved::Lit) -> TypeId {}

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
