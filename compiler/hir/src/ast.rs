use std::{hash::Hash, marker::PhantomData};

use diagnostics::{FilePath, RawSpan, Span};
use rustc_hash::FxHashMap;
use syntax::{ast::Item, AstElement, SyntaxElement};
use verde::Id;

use crate::RawPath;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct ErasedAstId {
	item: Id<RawPath>,
	index: u32,
}

impl Span for ErasedAstId {
	type Ctx = AstMap;
	type Relative = FilePath;

	fn to_raw(self, ctx: &Self::Ctx) -> RawSpan<Self::Relative> {
		let item = ctx.items.get(&self.item).unwrap();
		let span = item.sub[self.index as usize].text_range();
		RawSpan {
			start: span.start().into(),
			end: span.end().into(),
			relative: item.file,
		}
	}
}

pub struct AstId<T>(ErasedAstId, PhantomData<fn() -> T>);

pub struct AstMap {
	items: FxHashMap<Id<RawPath>, ItemData>,
}

struct ItemData {
	node: Item,
	file: FilePath,
	sub: Vec<SyntaxElement>,
}

impl AstMap {
	pub fn new() -> Self {
		Self {
			items: FxHashMap::default(),
		}
	}

	pub fn get<T: AstElement>(&self, id: AstId<T>) -> T {
		let item = self.items.get(&id.0.item).unwrap();
		let node = item.sub[id.0.index as usize].clone();
		T::cast(node).expect("invalid AstId")
	}
}

impl Default for AstMap {
	fn default() -> Self { Self::new() }
}

impl<T> Clone for AstId<T> {
	fn clone(&self) -> Self { *self }
}
impl<T> Copy for AstId<T> {}

impl<T> PartialEq for AstId<T> {
	fn eq(&self, other: &Self) -> bool { self.0 == other.0 }
}
impl<T> Eq for AstId<T> {}

impl<T> Hash for AstId<T> {
	fn hash<H: std::hash::Hasher>(&self, state: &mut H) { self.0.hash(state) }
}

impl<T> From<AstId<T>> for ErasedAstId {
	fn from(id: AstId<T>) -> Self { id.0 }
}
