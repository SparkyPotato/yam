use std::{collections::HashMap, convert::identity};

use diag::Span;

use crate::{
	resolved::{Expr, ExprKind, Path},
	GlobalLet,
	Spur,
	ValDef,
	ValDefKind,
};

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct ValRef(u32);
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct LocalRef(u32);

#[derive(Debug)]
pub struct ResolveCtx {
	globals: Vec<ValDef>,
}

impl std::ops::Index<ValRef> for ResolveCtx {
	type Output = ValDef;

	fn index(&self, index: ValRef) -> &Self::Output { &self.globals[index.0 as usize] }
}

#[derive(Default)]
pub struct ResolveCtxBuilder {
	globals: Vec<ValDef>,
	inserted: Vec<bool>,
}

impl ResolveCtxBuilder {
	pub fn new() -> Self { Self::default() }

	pub fn decl_val(&mut self) -> ValRef {
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
		ValRef(index as u32)
	}

	pub fn define_val(&mut self, val: ValRef, def: ValDef) {
		self.globals[val.0 as usize] = def;
		self.inserted[val.0 as usize] = true;
	}

	pub fn finish(self) -> ResolveCtx {
		assert!(self.inserted.into_iter().all(identity));
		ResolveCtx { globals: self.globals }
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
