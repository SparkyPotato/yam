use arena::{Arena, Ix};
use diagnostics::Span;
use hir::{ident::AbsPath, lang_item::LangItemMap, LangItem};
use rustc_hash::FxHashMap;
use verde::{Ctx, Id};

pub struct HirReader<'a> {
	pub types: &'a Arena<hir::Type>,
	pub exprs: &'a Arena<hir::Expr>,
	pub locals: &'a Arena<hir::Local>,
	pub lang_items: &'a LangItemMap,
	pub items: &'a FxHashMap<Id<AbsPath>, Id<hir::Item>>,
}

impl<'a> HirReader<'a> {
	pub fn new(
		types: &'a Arena<hir::Type>, exprs: &'a Arena<hir::Expr>, locals: &'a Arena<hir::Local>,
		lang_items: &'a LangItemMap, items: &'a FxHashMap<Id<AbsPath>, Id<hir::Item>>,
	) -> Self {
		Self {
			types,
			exprs,
			locals,
			lang_items,
			items,
		}
	}

	pub fn req_type(&self, ctx: &Ctx, ty: Ix<hir::Type>, error: bool) -> Id<thir::Type> {
		let ty = &self.types[ty];
		let ty = match ty.kind {
			hir::TypeKind::Array(ref a) => thir::Type::Array(thir::ArrayType {
				ty: self.req_type(ctx, a.ty, error),
				len: self.array_len(ctx, a.len),
			}),
			hir::TypeKind::Fn(ref f) => thir::Type::Fn(thir::FnType {
				params: f.params.iter().map(|&ty| self.req_type(ctx, ty, error)).collect(),
				ret: f
					.ret
					.map(|ty| self.req_type(ctx, ty, error))
					.unwrap_or_else(|| ctx.add(thir::Type::Void)),
			}),
			hir::TypeKind::Infer => {
				if error {
					let span = ty.id.erased();
					ctx.push(
						span.error("expected type, found `_`")
							.label(span.label("cannot infer type here")),
					);
				}
				thir::Type::Error
			},
			hir::TypeKind::Struct(s) => thir::Type::Struct(s),
			hir::TypeKind::Enum(e) => thir::Type::Enum(e),
			hir::TypeKind::Alias(p) => self.ty_alias(p),
			hir::TypeKind::Ptr(p) => thir::Type::Ptr(thir::PtrType {
				mutable: p.mutable,
				ty: self.req_type(ctx, p.ty, error),
			}),
		};
		ctx.add(ty)
	}

	fn ty_alias(&self, target: Id<AbsPath>) -> thir::Type {
		match self.lang_items.get_lang_item_of(target) {
			Some(x) => thir::Type::LangItem(x),
			None => todo!(),
		}
	}

	pub fn array_len(&self, ctx: &Ctx, len: Ix<hir::Expr>) -> u64 {
		let expr = &self.exprs[len];
		let span = expr.id.map(|x| x.erased());
		match expr.kind {
			hir::ExprKind::Literal(ref l) => match l.kind {
				hir::LiteralKind::Int => return l.value.as_str().parse().expect("invalid integer literal"),
				_ => span.map(|x| {
					ctx.push(
						x.error("expected `{int}`")
							.label(x.label("array lengths must be integers")),
					)
				}),
			},
			_ => span.map(|x| {
				ctx.push(
					x.error("expected `{int}`")
						.label(x.label("array lengths must be literals")),
				)
			}),
		};

		0
	}
}
