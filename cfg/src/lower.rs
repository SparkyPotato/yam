use std::collections::HashMap;

use diag::{
	ariadne::{Label, Report, ReportKind},
	Span,
};
use name_resolve::{
	resolved,
	resolved::{InbuiltType, Lit, Spur, ValExprKind},
};

use crate::{
	types::{TypeEngine, TypeInfo},
	Arg,
	BasicBlock,
	Field,
	Fn,
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
	diagnostics: &'a mut Vec<Report<Span>>,
	rodeo: &'a Rodeo,
}

impl CfgLower<'_> {
	fn lower_ty(&mut self, ty: &resolved::Ty) -> Ty<Type> {
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

	fn lower_val(&mut self, val: &resolved::Val) -> Val<Type> {
		match val {
			resolved::Val::Const(_) => unreachable!("consts are not supported"),
			resolved::Val::Static(_) => unreachable!("statics are not supported"),
			resolved::Val::Fn(f) => Val::Fn(self.lower_fn(f)),
		}
	}

	fn lower_fn(&mut self, f: &resolved::Fn) -> Fn<Type> {
		let ret = f.ret.as_ref().map(|x| self.lower_ty_expr(&x)).unwrap_or(Type::Void);

		self.engine.push_scope();

		let mut blocks = Vec::new();
		self.lower_block(&f.block, &mut blocks);
		let blocks = blocks
			.into_iter()
			.map(|block| self.resolve_basic_block(block))
			.collect();

		self.engine.pop_scope();
		self.engine.reset();

		Fn {
			path: f.path.clone(),
			ret: Box::new(ret),
			blocks,
			span: f.span,
		}
	}

	fn lower_block(&mut self, block: &resolved::Block, blocks: &mut Vec<BasicBlock<TypeId>>) {
		self.engine.push_scope();

		let mut curr_block = BasicBlock {
			args: Vec::new(),
			instrs: Vec::new(),
		};

		let last_expr = block.stmts.len().wrapping_sub(1);
		for (i, stmt) in block.stmts.iter().enumerate() {
			let expr = match &stmt.node {
				resolved::StmtKind::Expr(expr) => {
					if i != last_expr {
						self.diagnostics.push(
							stmt.span
								.report(ReportKind::Error)
								.with_message("expected `;` after statement")
								.with_label(Label::new(stmt.span).with_message("consider putting a `;` after this"))
								.finish(),
						)
					}
					expr
				},
				resolved::StmtKind::Semi(expr) => expr,
				resolved::StmtKind::Err => continue,
			};

			match expr {
				ValExprKind::Lit(lit) => {
					let ty = match lit {
						Lit::Bool(_) => TypeInfo::Ty(self.inbuilts[&InbuiltType::Bool]),
						Lit::Char(_) => TypeInfo::Ty(self.inbuilts[&InbuiltType::Uint(32)]),
						Lit::Float(_) => TypeInfo::Float,
						Lit::Int(_) => TypeInfo::Int,
						Lit::String(_) => TypeInfo::Ptr {
							mutable: false,
							to: self.engine.insert(TypeInfo::Ty(self.inbuilts[&InbuiltType::Uint(8)])),
						},
					};
					let ty = self.engine.insert(ty);

					curr_block.instrs.push(Instr {
						kind: InstrKind::Literal(*lit),
						ty,
						span: stmt.span,
					});
				},
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
			}
		}

		blocks.push(curr_block);

		self.engine.pop_scope();
	}

	fn resolve_basic_block(&mut self, block: BasicBlock<TypeId>) -> BasicBlock<Type> {
		BasicBlock {
			args: block
				.args
				.into_iter()
				.map(|x| Arg {
					ident: x.ident,
					ty: self.engine.reconstruct(
						x.ty,
						x.ident.map(|x| x.span).unwrap_or(Self::useless_span()),
						&mut self.diagnostics,
					),
				})
				.collect(),
			instrs: block
				.instrs
				.into_iter()
				.map(|x| Instr {
					kind: x.kind,
					span: x.span,
					ty: self.engine.reconstruct(x.ty, x.span, &mut self.diagnostics),
				})
				.collect(),
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

	fn useless_span() -> Span {
		Span {
			start: 0,
			end: 0,
			file: Spur::default(),
		}
	}
}
