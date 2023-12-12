use diagnostics::Span;
use rustc_hash::FxHashMap;
use syntax::ast;
use verde::{query, Ctx, Id, Tracked};

use crate::{ast::AstId, ident::AbsPath, AttrKind, Item, ItemKind, TypeKind};

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum LangItem {
	// Primitives.
	U8,
	U16,
	U32,
	U64,
	U128,
	I8,
	I16,
	I32,
	I64,
	I128,
	Bool,
	Char,
	F32,
	F64,
}

const COUNT: usize = LangItem::F64 as usize + 1;

#[derive(Tracked, Default, Eq, PartialEq)]
pub struct LangItemMap {
	#[id]
	id: (),
	items: [Option<Id<AbsPath>>; COUNT],
	reverse: FxHashMap<Id<AbsPath>, LangItem>,
}

impl LangItemMap {
	pub fn get(&self, item: LangItem) -> Option<Id<AbsPath>> { self.items[item as usize] }

	pub fn insert(&mut self, item: LangItem, path: Id<AbsPath>) -> Option<Id<AbsPath>> {
		let prev = self.items[item as usize];
		self.items[item as usize] = Some(path);
		self.reverse.insert(path, item);
		prev
	}

	pub fn get_lang_item_of(&self, path: Id<AbsPath>) -> Option<LangItem> { self.reverse.get(&path).copied() }
}

#[query]
pub fn build_lang_item_map(ctx: &Ctx, #[ignore] items: &FxHashMap<Id<AbsPath>, Id<Item>>) -> LangItemMap {
	let mut map = LangItemMap::default();
	for (&id, &item) in items.iter() {
		let mut prev: Option<AstId<ast::Attribute>> = None;
		let item = ctx.get(item);
		for attr in item.attrs.iter() {
			if let Some(prev) = prev {
				let prev = prev.erased();
				ctx.push(
					prev.error("cannot have multiple lang items attributes on one item")
						.label(prev.label("previous attribute"))
						.label(attr.id.erased().label("other lang item attribute")),
				);
			}

			let AttrKind::LangItem(l) = attr.kind;
			map.insert(l, id);
			prev = Some(attr.id);

			verify_lang_item_shape(ctx, l, &item);
		}
	}
	map
}

fn verify_lang_item_shape(ctx: &Ctx, l: LangItem, item: &Item) {
	use LangItem::*;
	match l {
		U8 | U16 | U32 | U64 | U128 | I8 | I16 | I32 | I64 | I128 | Bool | Char | F32 | F64 => {
			let ItemKind::TypeAlias(alias) = &item.kind else {
				let span = item.name.id.erased();
				ctx.push(
					span.error("primitive lang item must be a type alias to itself")
						.label(span.label(format!("found {:?}", item.kind))),
				);
				return;
			};
			let ty = &item.types[alias.ty];
			let TypeKind::Alias(path) = &ty.kind else {
				let span = ty.id.erased();
				ctx.push(
					span.error("primitive lang item must be a type alias to itself")
						.label(span.label(format!("found {:?}", ty.kind))),
				);
				return;
			};
			if *path != item.path {
				let span = ty.id.erased();
				let alias = match *ctx.geti(*path) {
					AbsPath::Name { name, .. } => name,
					_ => unreachable!("found type alias to package?"),
				};
				ctx.push(
					span.error("primitive lang item must be a type alias to itself")
						.label(span.label(format!("aliases `{}`", alias.as_str()))),
				);
			}
		},
	}
}
