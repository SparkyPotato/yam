use std::{fmt::Debug, hash::Hash, marker::PhantomData};

use diagnostics::{FilePath, FullSpan, RawSpan, Span};
use rustc_hash::FxHashMap;
use syntax::{ast::Item, AstElement, SyntaxElement};
use verde::Id;

use crate::Path;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct ErasedAstId {
	pub item: Id<Path>,
	pub index: u32,
}
impl Span for ErasedAstId {
	type Ctx = AstMap;

	fn to_raw(self, ctx: &Self::Ctx) -> FullSpan {
		let item = ctx.items.get(&self.item).unwrap();
		let span = item.sub[self.index as usize].text_range();
		RawSpan {
			start: span.start().into(),
			end: span.end().into(),
			relative: item.file,
		}
	}
}

pub struct AstId<T>(pub ErasedAstId, pub PhantomData<fn() -> T>);
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
impl<T> Debug for AstId<T> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(
			f,
			"AstId<{}>({:?}, {})",
			std::any::type_name::<T>(),
			self.0.item,
			self.0.index
		)
	}
}
impl<T> AstId<T> {
	pub fn erased(self) -> ErasedAstId { self.0 }
}

#[derive(Debug)]
pub struct ItemData {
	pub item: Item,
	pub file: FilePath,
	pub path: Id<Path>,
	pub sub: Vec<SyntaxElement>,
}

#[derive(Debug, Default)]
pub struct AstMap {
	items: FxHashMap<Id<Path>, ItemData>,
}

impl AstMap {
	pub fn new(items: impl IntoIterator<Item = ItemData>) -> Self {
		Self {
			items: items.into_iter().map(|x| (x.path, x)).collect(),
		}
	}

	pub fn get<T: AstElement>(&self, id: AstId<T>) -> T {
		let item = self.items.get(&id.0.item).unwrap();
		let node = item.sub[id.0.index as usize].clone();
		T::cast(node).expect("invalid AstId")
	}
}
