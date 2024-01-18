use arena::{Arena, Ix};
use diagnostics::Span;
use hir::{ident::AbsPath, lang_item::LangItemMap};
use rustc_hash::FxHashMap;
use verde::{Ctx, Id};

pub struct HirReader<'a> {
	pub path: Id<AbsPath>,
	pub types: &'a Arena<hir::Type>,
	pub exprs: &'a Arena<hir::Expr>,
	pub locals: &'a Arena<hir::Local>,
	pub lang_items: &'a LangItemMap,
	pub items: &'a FxHashMap<Id<AbsPath>, Id<hir::Item>>,
}

impl<'a> HirReader<'a> {
	pub fn new(
		item: &'a hir::Item, lang_items: &'a LangItemMap, items: &'a FxHashMap<Id<AbsPath>, Id<hir::Item>>,
	) -> Self {
		Self {
			path: item.path,
			types: &item.types,
			exprs: &item.exprs,
			locals: &item.locals,
			lang_items,
			items,
		}
	}

	pub fn req_type(&self, ctx: &Ctx, ty: Ix<hir::Type>) -> Id<thir::Type> {
		let ty = &self.types[ty];
		let ty = match ty.kind {
			hir::TypeKind::Array(ref a) => thir::Type::Array(thir::ArrayType {
				ty: self.req_type(ctx, a.ty),
				len: self.array_len(ctx, a.len),
			}),
			hir::TypeKind::Fn(ref f) => thir::Type::Fn(thir::FnType {
				abi: f.abi.as_ref().map(|x| x.abi.as_ref().map(|x| x.abi).unwrap_or("C")),
				params: f.params.iter().map(|&ty| self.req_type(ctx, ty)).collect(),
				ret: f
					.ret
					.map(|ty| self.req_type(ctx, ty))
					.unwrap_or_else(|| ctx.add(thir::Type::Void)),
			}),
			hir::TypeKind::Infer => {
				let span = ty.id.erased();
				ctx.push(
					span.error("expected type, found `_`")
						.label(span.label("cannot infer type here")),
				);
				thir::Type::Error
			},
			hir::TypeKind::Struct(s) => thir::Type::Struct(s),
			hir::TypeKind::Enum(e) => thir::Type::Enum(e),
			hir::TypeKind::Alias(p) => self.type_alias(p),
			hir::TypeKind::Ptr(p) => thir::Type::Ptr(thir::PtrType {
				mutable: p.mutable,
				ty: self.req_type(ctx, p.ty),
			}),
			hir::TypeKind::Error => thir::Type::Error,
		};
		ctx.add(ty)
	}

	pub fn type_alias(&self, target: Id<AbsPath>) -> thir::Type {
		match self.lang_items.get_lang_item_of(target) {
			Some(x) => thir::Type::LangItem(x),
			None => todo!(),
		}
	}

	pub fn array_len(&self, ctx: &Ctx, len: Ix<hir::Expr>) -> u64 {
		let expr = &self.exprs[len];
		let span = expr.id.erased();
		match expr.kind {
			hir::ExprKind::Literal(l) => match l {
				hir::Literal::Int(i) => return i as _,
				_ => ctx.push(
					span.error("expected `{int}`")
						.label(span.label("array lengths must be integers")),
				),
			},
			_ => ctx.push(
				span.error("expected `{int}`")
					.label(span.label("array lengths must be literals")),
			),
		};

		0
	}
}

