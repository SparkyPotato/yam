use std::{fmt::Debug, hash::Hash, marker::PhantomData};

use diagnostics::{FilePath, FullSpan, Span};
use rustc_hash::FxHashMap;
use syntax::{ast::Item, AstElement, SyntaxElement, TextRange};
use verde::Id;

use crate::ident::AbsPath;

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct ErasedAstId {
	pub item: Id<AbsPath>,
	pub index: u32,
}
impl Span for ErasedAstId {
	type Ctx = AstMap;

	fn to_raw(self, ctx: &Self::Ctx) -> FullSpan {
		let item = ctx.items.get(&self.item).unwrap();
		let span = match item.sub[self.index as usize] {
			ItemElement::Concrete(ref n) => n.text_range(),
			ItemElement::Error => {
				let start = ErasedAstId {
					item: self.item,
					index: self.index - 1,
				}
				.to_raw(ctx);
				let end = ErasedAstId {
					item: self.item,
					index: self.index + 1,
				}
				.to_raw(ctx);
				let start = if start.end + 1 == end.start {
					end.start
				} else {
					start.end + 1
				};
				TextRange::new(start.into(), end.start.into())
			},
		};
		FullSpan {
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

#[derive(Debug, Clone)]
pub enum ItemElement {
	Concrete(SyntaxElement),
	Error,
}

#[derive(Debug, Clone)]
pub struct ItemData {
	pub item: Item,
	pub file: FilePath,
	pub path: Id<AbsPath>,
	pub sub: Vec<ItemElement>,
}

#[derive(Debug, Default)]
pub struct AstMap {
	items: FxHashMap<Id<AbsPath>, ItemData>,
}

impl AstMap {
	pub fn new(items: impl IntoIterator<Item = ItemData>) -> Self {
		Self {
			items: items.into_iter().map(|x| (x.path, x)).collect(),
		}
	}

	pub fn get<T: AstElement>(&self, id: AstId<T>) -> Option<T> {
		let item = self.items.get(&id.0.item).unwrap();
		let node = item.sub[id.0.index as usize].clone();
		match node {
			ItemElement::Concrete(n) => Some(T::cast(n).expect("invalid AstId")),
			ItemElement::Error => None,
		}
	}
}
