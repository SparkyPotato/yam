use arena::{Arena, Ix};
use diagnostics::{FilePath, RawSpan, Span};
use hir::ident::{AbsPath, InnerPath, PackageId};
use rustc_hash::FxHashMap;
use syntax::{ast, token, AstElement, AstToken, SyntaxElement};
use text::Text;
use tracing::{span, Level};
use verde::{query, Ctx, Db, Id, Tracked};

use crate::{
	index::name_of_item,
	module::{ItemBuilder, Module, ModuleMap, PackageTree},
};

pub fn build_hir_sea(db: &dyn Db, modules: impl IntoIterator<Item = Id<LoweredModule>>) -> Vec<Id<hir::Item>> {
	let mut v = Vec::new();
	for module in modules {
		let module = db.get(module);
		for &item in module.items.iter() {
			v.push(item);
		}
	}
	v
}

#[derive(Tracked)]
pub struct LoweredModule {
	#[id]
	module: Id<AbsPath>,
	items: Vec<Id<hir::Item>>,
}

impl Eq for LoweredModule {}
impl PartialEq for LoweredModule {
	fn eq(&self, _: &Self) -> bool {
		// We don't care about actually comparing each item, since this struct is just a dummy struct to generate the
		// real HIR sea.
		false
	}
}

/// Lower a module to HIR.
///
/// The `ModuleMap` should be the same one that was used to generate the index.
#[query]
pub fn lower_to_hir(
	ctx: &Ctx, module: Id<Module>, packages: Id<VisibilePackages>, tree: Id<PackageTree>, #[ignore] map: &mut ModuleMap,
) -> LoweredModule {
	let module = ctx.get(module);

	let s = span!(Level::DEBUG, "lower to HIR", path =%module.file);
	let _e = s.enter();

	let items = module
		.ast
		.items()
		.into_iter()
		.flat_map(|x| {
			name_of_item(&x).and_then(|x| x.text()).and_then(|name| {
				let builder = map.define(name).unwrap();

				let path = ctx.geti(module.path);
				let package = path.package;
				let prec = path.path;
				let path = ctx.add(InnerPath { prec, name });
				let path = ctx.add(AbsPath {
					package,
					path: Some(path),
				});

				let item = lower_item(ctx, path, module.file, x, packages, tree, builder);
				item.map(|x| ctx.insert(x))
			})
		})
		.collect();

	LoweredModule {
		module: module.path,
		items,
	}
}

fn lower_item(
	ctx: &Ctx, path: Id<AbsPath>, file: FilePath, item: ast::Item, packages: Id<VisibilePackages>,
	tree: Id<PackageTree>, builder: ItemBuilder,
) -> Option<hir::Item> {
	let mut lowerer = Lowerer {
		ctx,
		packages,
		tree,
		builder,
		exprs: Arena::new(),
		types: Arena::new(),
		locals: Arena::new(),
		file,
	};

	let attrs = item
		.attributes()
		.into_iter()
		.filter_map(|x| lowerer.attrib(x))
		.collect();

	let kind = match item.item_kind() {
		Some(ast::ItemKind::Fn(f)) => hir::ItemKind::Fn(lowerer.fn_(f)?),
		Some(ast::ItemKind::Struct(s)) => hir::ItemKind::Struct(lowerer.struct_(s)?),
		Some(ast::ItemKind::Enum(e)) => hir::ItemKind::Enum(lowerer.enum_(e)?),
		Some(ast::ItemKind::TypeAlias(t)) => hir::ItemKind::TypeAlias(lowerer.type_alias(t)?),
		Some(ast::ItemKind::Static(s)) => hir::ItemKind::Static(lowerer.static_(s)?),
		Some(ast::ItemKind::Import(_)) | None => return None,
	};

	Some(hir::Item {
		path,
		attrs,
		exprs: lowerer.exprs,
		types: lowerer.types,
		locals: lowerer.locals,
		kind,
	})
}

struct Lowerer<'a> {
	ctx: &'a Ctx<'a>,
	packages: Id<VisibilePackages>,
	tree: Id<PackageTree>,
	builder: ItemBuilder<'a>,
	exprs: Arena<hir::Expr>,
	types: Arena<hir::Type>,
	locals: Arena<hir::Local>,
	file: FilePath,
}

impl Lowerer<'_> {
	fn attrib(&mut self, a: ast::Attribute) -> Option<hir::Attr> {
		let name = a.name()?;
		let text = name.text()?;
		if text == Text::new("lang") {
			let tt = a.token_tree()?;
			let mut tokens = tt.tokens();

			let Some(value) = tokens.next() else {
				let span = tt.span().with(self.file);
				self.ctx.push(span.error("expected lang item type").label(span.mark()));
				return None;
			};
			let Some(ident) = token::Ident::cast(SyntaxElement::Token(value.clone())) else {
				let span = value.text_range();
				let span = RawSpan {
					start: span.start().into(),
					end: span.end().into(),
					relative: self.file,
				};
				self.ctx
					.push(span.error("lang item type must be an `ident`").label(span.mark()));
				return None;
			};

			let mut span = None;
			for token in tokens {
				let range = token.text_range();
				let span = span.get_or_insert(RawSpan {
					start: range.start().into(),
					end: range.end().into(),
					relative: self.file,
				});
				span.end = range.end().into();
			}

			if let Some(span) = span {
				self.ctx.push(
					span.error("unexpected tokens in lang item attribute")
						.label(span.mark()),
				);
			}

			let lang = match ident.text().as_str() {
				"u8" => hir::LangItem::U8,
				"u16" => hir::LangItem::U16,
				"u32" => hir::LangItem::U32,
				"u64" => hir::LangItem::U64,
				"u128" => hir::LangItem::U128,
				"i8" => hir::LangItem::I8,
				"i16" => hir::LangItem::I16,
				"i32" => hir::LangItem::I32,
				"i64" => hir::LangItem::I64,
				"i128" => hir::LangItem::I128,
				"bool" => hir::LangItem::Bool,
				"char" => hir::LangItem::Char,
				"f32" => hir::LangItem::F32,
				"f64" => hir::LangItem::F64,
				_ => {
					let span = ident.span().with(self.file);
					self.ctx.push(span.error("unknown lang item").label(span.mark()));
					return None;
				},
			};
			Some(hir::Attr::LangItem(lang))
		} else {
			let span = name.span().with(self.file);
			self.ctx.push(span.error("unknown attribute").label(span.mark()));
			None
		}
	}

	fn fn_(&mut self, f: ast::Fn) -> Option<hir::Fn> { None }

	fn struct_(&mut self, s: ast::Struct) -> Option<hir::Struct> {
		let name = self.name(s.name()?)?;
		let fields = s.fields().into_iter().filter_map(|x| self.param(x)).collect();

		Some(hir::Struct { name, fields })
	}

	fn enum_(&mut self, e: ast::Enum) -> Option<hir::Enum> {
		let name = self.name(e.name()?)?;
		let variants = e
			.variant_list()
			.iter()
			.flat_map(|x| x.variants())
			.filter_map(|x| self.name(x).map(|x| hir::Variant(x)))
			.collect();

		Some(hir::Enum { name, variants })
	}

	fn type_alias(&mut self, t: ast::TypeAlias) -> Option<hir::TypeAlias> {
		let name = self.name(t.name()?)?;
		let ty = self.ty(t.type_()?)?;

		Some(hir::TypeAlias { name, ty })
	}

	fn static_(&mut self, s: ast::Static) -> Option<hir::Static> {
		let name = self.name(s.name()?)?;
		let ty = self.ty(s.type_()?)?;
		let init = self.expr(s.init()?)?;

		Some(hir::Static { name, ty, init })
	}

	fn param(&mut self, p: ast::Param) -> Option<hir::Param> {
		let name = self.name(p.name()?)?;
		let ty = self.ty(p.type_()?)?;
		let id = self.builder.add(p);

		Some(hir::Param { name, ty, id })
	}

	fn ty(&mut self, t: ast::Type) -> Option<Ix<hir::Type>> { None }

	fn expr(&mut self, e: ast::Expr) -> Option<Ix<hir::Expr>> { None }

	fn name(&mut self, name: ast::Name) -> Option<hir::Name> {
		let ident = name.ident()?;
		let name = ident.text();
		let id = self.builder.add(ident);
		Some(hir::Name { name, id })
	}
}

/// The packages visible to a package.
#[derive(Tracked, Eq, PartialEq)]
pub struct VisibilePackages {
	#[id]
	pub package: PackageId,
	pub packages: FxHashMap<Text, PackageId>,
}
