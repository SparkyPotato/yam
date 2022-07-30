use std::{collections::HashMap, convert::identity, ops::Index};

use diag::{DiagKind, Diagnostic, Diagnostics, Span};
use parse::ast::Spanned;

use crate::{
	hir::{Expr, ExprKind, Path},
	lang_item::LangItem,
	GlobalLet,
	Rodeo,
	Spur,
	Type,
	ValDef,
	ValDefKind,
};

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct ValRef(u32);
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct LocalRef(u32);

#[derive(Debug)]
pub struct Hir {
	rodeo: Rodeo,
	globals: Vec<ValDef>,
	lang_items: Vec<ValRef>,
	val_to_lang_item: HashMap<ValRef, LangItem>,
}

impl Hir {
	pub fn rodeo(&self) -> &Rodeo { &self.rodeo }

	pub fn resolve_intern(&self, spur: Spur) -> &str { self.rodeo.resolve(&spur) }

	pub fn lang_item(&self, val: ValRef) -> Option<LangItem> { self.val_to_lang_item.get(&val).copied() }
}

impl Index<ValRef> for Hir {
	type Output = ValDef;

	fn index(&self, index: ValRef) -> &Self::Output { &self.globals[index.0 as usize] }
}

impl Index<LangItem> for Hir {
	type Output = ValRef;

	fn index(&self, index: LangItem) -> &Self::Output { &self.lang_items[index as usize] }
}

pub struct HirBuilder {
	globals: Vec<ValDef>,
	inserted: Vec<bool>,
	lang_items: Vec<ValRef>,
	inserted_lang_items: Vec<bool>,
	val_to_lang_item: HashMap<ValRef, LangItem>,
	item_map: HashMap<Path, Spanned<ValRef>>,
}

impl Default for HirBuilder {
	fn default() -> Self {
		Self {
			globals: Vec::new(),
			inserted: Vec::new(),
			lang_items: vec![ValRef(0); LangItem::all().len()],
			inserted_lang_items: vec![false; LangItem::all().len()],
			val_to_lang_item: HashMap::new(),
			item_map: HashMap::new(),
		}
	}
}

impl HirBuilder {
	pub fn new() -> Self { Self::default() }

	pub fn decl_val(&mut self, path: Path, span: Span, diags: &mut Diagnostics) -> ValRef {
		let index = self.globals.len();

		self.globals.push(ValDef {
			path: Path::default(),
			kind: ValDefKind::Const(GlobalLet {
				ty: Type::Void,
				ty_expr: None,
				expr: Expr {
					kind: ExprKind::Err,
					ty: Type::Void,
					span: Span::default(),
				},
			}),
			span: Span::default(),
		});
		self.inserted.push(false);

		let val = ValRef(index as u32);
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

	pub fn define_val(&mut self, val: ValRef, def: ValDef) {
		self.globals[val.0 as usize] = def;
		self.inserted[val.0 as usize] = true;
	}

	pub fn resolve(&self, path: &Path) -> Option<ValRef> { self.item_map.get(path).map(|x| x.node) }

	pub fn define_lang_item(&mut self, item: LangItem, def: ValRef, diags: &mut Diagnostics) {
		if self.inserted_lang_items[item as usize] {
			let orig_span = self.globals[self.lang_items[item as usize].0 as usize].span;
			let this_span = self.globals[def.0 as usize].span;

			diags.push(
				this_span
					.error("lang item already defined")
					.label(orig_span.label("already defined here"))
					.label(this_span.label("redefined here")),
			);
		} else {
			self.lang_items[item as usize] = def;
			self.inserted_lang_items[item as usize] = true;
			self.val_to_lang_item.insert(def, item);
		}
	}

	pub fn finish(self, rodeo: Rodeo, diags: &mut Diagnostics, source_id: Spur) -> Hir {
		assert!(self.inserted.into_iter().all(identity));

		for item in LangItem::all() {
			if !self.inserted_lang_items[*item as usize] {
				diags.push(Diagnostic::source(
					DiagKind::Error,
					format!("lang item `{}` is not defined", item),
					source_id,
				));
			}
		}

		Hir {
			rodeo,
			globals: self.globals,
			lang_items: self.lang_items,
			val_to_lang_item: self.val_to_lang_item,
		}
	}
}

#[derive(Default)]
pub struct LocalBuilder {
	counter: u32,
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

	pub fn reset(&mut self) {
		assert!(self.scopes.is_empty());
		self.counter = 0;
	}

	fn next(&mut self) -> LocalRef {
		let counter = self.counter;
		self.counter += 1;
		LocalRef(counter)
	}
}
