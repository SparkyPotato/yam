use std::{collections::HashMap, convert::identity, ops::Index};

use diag::{ariadne::Label, Span};
use parse::ast::Spanned;

use crate::{
	hir::{Expr, ExprKind, Path},
	lang_item::LangItem,
	GlobalLet,
	Report,
	ReportKind,
	Rodeo,
	Spur,
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

	pub fn decl_val(&mut self, path: Path, span: Span, diags: &mut Vec<Report<Span>>) -> ValRef {
		let index = self.globals.len();

		self.globals.push(ValDef {
			path: Path::default(),
			kind: ValDefKind::Const(GlobalLet {
				ty: None,
				expr: Expr {
					node: ExprKind::Err,
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
					.report(ReportKind::Error)
					.with_message("duplicate definition")
					.with_label(Label::new(span).with_message("already defined here"))
					.with_label(Label::new(existing.span).with_message("redefined here"))
					.finish(),
			);
		}

		val
	}

	pub fn define_val(&mut self, val: ValRef, def: ValDef) {
		self.globals[val.0 as usize] = def;
		self.inserted[val.0 as usize] = true;
	}

	pub fn resolve(&self, path: &Path) -> Option<ValRef> { self.item_map.get(path).map(|x| x.node) }

	pub fn define_lang_item(&mut self, item: LangItem, def: ValRef, diags: &mut Vec<Report<Span>>) {
		if self.inserted_lang_items[item as usize] {
			let orig_span = self.globals[self.lang_items[item as usize].0 as usize].span;
			let this_span = self.globals[def.0 as usize].span;

			diags.push(
				this_span
					.report(ReportKind::Error)
					.with_message("lang item already defined")
					.with_label(Label::new(orig_span).with_message("already defined here"))
					.with_label(Label::new(this_span).with_message("redefined here"))
					.finish(),
			);
		} else {
			self.lang_items[item as usize] = def;
			self.inserted_lang_items[item as usize] = true;
			self.val_to_lang_item.insert(def, item);
		}
	}

	pub fn finish(self, rodeo: Rodeo, diags: &mut Vec<Report<Span>>, source_id: Spur) -> Hir {
		assert!(self.inserted.into_iter().all(identity));

		for item in LangItem::all() {
			if !self.inserted_lang_items[*item as usize] {
				diags.push(
					Report::build(ReportKind::Error, source_id, 0)
						.with_message(format!("lang item `{}` is not defined", item))
						.finish(),
				);
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
