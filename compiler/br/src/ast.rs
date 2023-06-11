use std::{fmt::Debug, hash::Hash, marker::PhantomData};

use diagnostics::{FilePath, FullSpan, RawSpan, Span};
use rustc_hash::FxHashMap;
use syntax::{
	ast::{File, Item},
	AstElement,
	SyntaxElement,
};
use text::Text;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct ErasedAstId {
	item: Text,
	index: u32,
}
impl Span for ErasedAstId {
	type Ctx = ModuleMap;

	fn to_raw(self, ctx: &Self::Ctx) -> FullSpan {
		let item = ctx.items.get(&self.item).unwrap();
		let span = item.sub[self.index as usize].text_range();
		RawSpan {
			start: span.start().into(),
			end: span.end().into(),
			relative: ctx.file,
		}
	}
}

pub struct AstId<T>(ErasedAstId, PhantomData<fn() -> T>);
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
struct ItemData {
	node: Item,
	sub: Vec<SyntaxElement>,
}

#[derive(Debug)]
pub struct ModuleMap {
	ast: File,
	file: FilePath,
	items: FxHashMap<Text, ItemData>,
}

impl ModuleMap {
	pub fn new(ast: File, file: FilePath) -> Self {
		Self {
			ast,
			file,
			items: FxHashMap::default(),
		}
	}

	pub fn ast(&self) -> &File { &self.ast }

	pub fn get<T: AstElement>(&self, id: AstId<T>) -> T {
		let item = self.items.get(&id.0.item).unwrap();
		let node = item.sub[id.0.index as usize].clone();
		T::cast(node).expect("invalid AstId")
	}

	pub fn add(&mut self, name: Text, item: Item) -> ItemBuilder {
		let map = self.items.entry(name).or_insert(ItemData {
			node: item,
			sub: Vec::new(),
		});
		map.sub.clear();
		ItemBuilder { map, item: name }
	}
}

pub struct ItemBuilder<'a> {
	map: &'a mut ItemData,
	item: Text,
}

impl ItemBuilder<'_> {
	pub fn add<T: AstElement>(&mut self, node: T) -> AstId<T> {
		let index = self.map.sub.len() as u32;
		self.map.sub.push(node.inner());
		AstId(ErasedAstId { item: self.item, index }, PhantomData)
	}
}
