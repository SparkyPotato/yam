use arena::{dense::DenseMap, Arena};
use hir::{
	ident::{AbsPath, DebugAbsPath},
	lang_item::LangItemMap,
	LangItem,
};
use rustc_hash::FxHashMap;
use tracing::{span, Level};
use verde::{query, Ctx, Id};

use crate::reader::HirReader;

#[query]
pub fn type_decl(
	ctx: &Ctx, item: Id<hir::Item>, lang_items: Id<LangItemMap>,
	#[ignore] items: &FxHashMap<Id<AbsPath>, Id<hir::Item>>,
) -> thir::ItemDecl {
	let item = ctx.get(item);

	let s = span!(Level::DEBUG, "type decl", path=?item.path.debug(ctx));
	let _e = s.enter();

	let lang_items = ctx.get(lang_items);
	let reader = HirReader::new(&item, &lang_items, items);

	let kind = match item.kind {
		hir::ItemKind::Struct(ref s) => thir::ItemDeclKind::Struct(struct_(ctx, &reader, s)),
		hir::ItemKind::Enum(ref e) => thir::ItemDeclKind::Enum(enum_(ctx, &reader, e)),
		hir::ItemKind::Fn(ref f) => thir::ItemDeclKind::Fn(fn_(ctx, &reader, f)),
		hir::ItemKind::TypeAlias(ref t) => thir::ItemDeclKind::TypeAlias(type_alias(ctx, &reader, t)),
		hir::ItemKind::Static(ref s) => thir::ItemDeclKind::Static(static_(ctx, &reader, s)),
	};

	thir::ItemDecl { path: item.path, kind }
}

fn struct_(ctx: &Ctx, reader: &HirReader, s: &hir::Struct) -> thir::StructDecl {
	thir::StructDecl {
		fields: params(ctx, reader, &s.fields),
		ty: ctx.add(thir::Type::Struct(reader.path)),
	}
}

fn enum_(ctx: &Ctx, reader: &HirReader, e: &hir::Enum) -> thir::EnumDecl {
	let var = e.variants.len();
	let variants = if var == 0 { 1 } else { var.ilog2() + 1 };
	let bits = ((variants + 7) / 8) * 8;
	thir::EnumDecl {
		repr: match bits {
			8 => LangItem::U8,
			16 => LangItem::U16,
			32 => LangItem::U32,
			64 => LangItem::U64,
			_ => unreachable!(),
		},
		ty: ctx.add(thir::Type::Enum(reader.path)),
	}
}

fn fn_(ctx: &Ctx, reader: &HirReader, f: &hir::Fn) -> thir::FnDecl {
	let ret = f
		.ret
		.map(|ty| reader.req_type(ctx, ty))
		.unwrap_or_else(|| ctx.add(thir::Type::Void));
	let params = params(ctx, reader, &f.params);
	let ty = ctx.add(thir::Type::Fn(thir::FnType {
		abi: f.abi.as_ref().map(|x| x.abi.as_ref().map(|x| x.abi).unwrap_or("C")),
		params: params.iter().map(|(_, &x)| x).collect(),
		ret,
	}));
	thir::FnDecl { params, ret, ty }
}

fn type_alias(ctx: &Ctx, reader: &HirReader, t: &hir::TypeAlias) -> thir::TypeAliasDecl {
	thir::TypeAliasDecl {
		ty: reader.req_type(ctx, t.ty),
	}
}

fn static_(ctx: &Ctx, reader: &HirReader, s: &hir::Static) -> thir::StaticDecl {
	thir::StaticDecl {
		ty: reader.req_type(ctx, s.ty),
	}
}

fn params(ctx: &Ctx, reader: &HirReader, params: &Arena<hir::Param>) -> DenseMap<hir::Param, Id<thir::Type>> {
	let mut map = DenseMap::with_capacity(params.len());
	for (id, param) in params.ids_iter() {
		map.insert(id, reader.req_type(ctx, param.ty));
	}
	map
}
