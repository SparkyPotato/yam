use std::{collections::HashMap, ops::Index};

use diag::{DiagKind, Diagnostic, Diagnostics, Span};
use id::{DenseMap, DenseMapBuilder, Id, IdGen, SparseMap, SparseMapBuilder};
use parse::ast::Spanned;

use crate::{hir::Path, lang_item::LangItem, Rodeo, Spur, ValDef};

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct ValRef(u32);

impl Id for ValRef {
	fn from_id(id: u32) -> Self { Self(id) }

	fn id(self) -> u32 { self.0 }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct LocalRef(u32);

impl Id for LocalRef {
	fn from_id(id: u32) -> Self { Self(id) }

	fn id(self) -> u32 { self.0 }
}

#[derive(Debug)]
pub struct Hir {
	pub rodeo: Rodeo,
	pub globals: DenseMap<ValRef, ValDef>,
	pub lang_items: DenseMap<LangItem, ValRef>,
	pub val_to_lang_item: SparseMap<ValRef, LangItem>,
}

impl Hir {
	pub fn rodeo(&self) -> &Rodeo { &self.rodeo }

	pub fn resolve_intern(&self, spur: Spur) -> &str { self.rodeo.resolve(&spur) }

	pub fn lang_item_of(&self, val: ValRef) -> Option<LangItem> { self.val_to_lang_item.get(val).copied() }

	pub fn lang_item(&self, item: LangItem) -> ValRef { self.lang_items[item] }
}

impl Index<ValRef> for Hir {
	type Output = ValDef;

	fn index(&self, index: ValRef) -> &Self::Output { &self.globals[index] }
}

impl Index<LangItem> for Hir {
	type Output = ValRef;

	fn index(&self, index: LangItem) -> &Self::Output { &self.lang_items[index] }
}

pub struct HirBuilder {
	globals: DenseMapBuilder<ValRef, ValDef>,
	lang_items: DenseMapBuilder<LangItem, ValRef>,
	val_to_lang_item: SparseMapBuilder<ValRef, LangItem>,
	item_map: HashMap<Path, Spanned<ValRef>>,
}

impl Default for HirBuilder {
	fn default() -> Self {
		Self {
			globals: DenseMap::builder(),
			lang_items: DenseMap::builder(),
			val_to_lang_item: SparseMap::builder(),
			item_map: HashMap::new(),
		}
	}
}

impl HirBuilder {
	pub fn new() -> Self { Self::default() }

	pub fn decl_val(&mut self, path: Path, span: Span, diags: &mut Diagnostics) -> ValRef {
		let val = self.globals.reserve();

		if let Some(existing) = self.item_map.insert(path, Spanned { node: val, span }) {
			diags.push(
				existing
					.span
					.error("duplicate definition")
					.label(span.label("already defined here"))
					.label(existing.span.label("redefined here")),
			);
		}

		val
	}

	pub fn define_val(&mut self, val: ValRef, def: ValDef) { self.globals.insert_at(val, def); }

	pub fn resolve(&self, path: &Path) -> Option<ValRef> { self.item_map.get(path).map(|x| x.node) }

	pub fn define_lang_item(&mut self, item: LangItem, def: ValRef, diags: &mut Diagnostics) {
		if self.lang_items.already_inserted(item) {
			let orig_span = self.globals[self.lang_items[item]].span;
			let this_span = self.globals[def].span;

			diags.push(
				this_span
					.error("lang item already defined")
					.label(orig_span.label("already defined here"))
					.label(this_span.label("redefined here")),
			);
		} else {
			self.lang_items.insert_at(item, def);
			self.val_to_lang_item.add(def, item);
		}
	}

	pub fn finish(self, rodeo: Rodeo, diags: &mut Diagnostics, source_id: Spur) -> Hir {
		for item in LangItem::all() {
			if !self.lang_items.already_inserted(*item) {
				diags.push(Diagnostic::source(
					DiagKind::Error,
					format!("lang item `{}` is not defined", item),
					source_id,
				));
			}
		}

		Hir {
			rodeo,
			globals: self.globals.build(),
			lang_items: self.lang_items.build(),
			val_to_lang_item: self.val_to_lang_item.build(),
		}
	}
}

#[derive(Default)]
pub struct LocalBuilder {
	gen: IdGen<LocalRef>,
	scopes: Vec<HashMap<Spur, LocalRef>>,
}

impl LocalBuilder {
	pub fn new() -> Self { Self::default() }

	pub fn declare(&mut self, spur: Spur) -> LocalRef {
		let local = self.next();
		self.scopes
			.last_mut()
			.expect("declare without any scope")
			.insert(spur, local);
		local
	}

	pub fn resolve(&mut self, ident: Spur) -> Option<LocalRef> {
		for scope in self.scopes.iter().rev() {
			if let Some(r) = scope.get(&ident) {
				return Some(*r);
			}
		}

		None
	}

	pub fn push_scope(&mut self) { self.scopes.push(HashMap::new()); }

	pub fn pop_scope(&mut self) { self.scopes.pop(); }

	pub fn reset(&mut self) { self.gen = IdGen::new(); }

	fn next(&mut self) -> LocalRef { self.gen.next() }
}
