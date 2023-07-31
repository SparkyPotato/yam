use std::{fmt::Debug, hash::Hash};

use diagnostics::{FilePath, FullSpan, Span};
use hir::{
	ast::{AstId, ErasedAstId, ItemData},
	ident::AbsPath,
	AstMap,
};
use rustc_hash::FxHashMap;
use syntax::{ast, AstElement, SyntaxElement};
use text::Text;
use verde::{Db, Id};

use self::local::name_of_item;

pub mod canonical;
pub mod local;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct ErasedTempId {
	module: Id<AbsPath>,
	index: u32,
}
impl Span for ErasedTempId {
	type Ctx = TempMap;

	fn to_raw(self, ctx: &Self::Ctx) -> FullSpan {
		let data = ctx.modules.get(&self.module).unwrap();
		let span = data.sub[self.index as usize].text_range();
		FullSpan {
			start: span.start().into(),
			end: span.end().into(),
			relative: data.file,
		}
	}
}

pub struct TempId<T>(ErasedTempId, std::marker::PhantomData<fn() -> T>);
impl<T> Clone for TempId<T> {
	fn clone(&self) -> Self { *self }
}
impl<T> Copy for TempId<T> {}
impl<T> PartialEq for TempId<T> {
	fn eq(&self, other: &Self) -> bool { self.0 == other.0 }
}
impl<T> Eq for TempId<T> {}
impl<T> Hash for TempId<T> {
	fn hash<H: std::hash::Hasher>(&self, state: &mut H) { self.0.hash(state) }
}
impl<T> From<TempId<T>> for ErasedTempId {
	fn from(id: TempId<T>) -> Self { id.0 }
}
impl<T> Debug for TempId<T> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(
			f,
			"TempId<{}>({:?}, {})",
			std::any::type_name::<T>(),
			self.0.module,
			self.0.index
		)
	}
}
impl<T> TempId<T> {
	pub fn erased(self) -> ErasedTempId { self.0 }
}

pub struct TempMap {
	modules: FxHashMap<Id<AbsPath>, Data>,
}

struct Data {
	file: FilePath,
	sub: Vec<SyntaxElement>,
}

/// A map from names in a module to their declarations.
pub struct ModuleMap {
	module: Id<AbsPath>,
	pub(crate) file: FilePath,
	items: FxHashMap<Text, ItemData>,
	sub: Vec<SyntaxElement>,
}

impl ModuleMap {
	pub fn new(module: Id<AbsPath>, file: FilePath) -> Self {
		Self {
			module,
			file,
			items: FxHashMap::default(),
			sub: Vec::new(),
		}
	}

	pub fn add_temp<T: AstElement>(&mut self, value: T) -> TempId<T> {
		let index = self.sub.len() as u32;
		self.sub.push(value.inner());
		TempId(
			ErasedTempId {
				module: self.module,
				index,
			},
			std::marker::PhantomData,
		)
	}

	pub fn span(&self, id: TempId<impl AstElement>) -> FullSpan {
		let element = self.sub[id.0.index as usize].clone();
		let span = element.text_range();
		FullSpan {
			start: span.start().into(),
			end: span.end().into(),
			relative: self.file,
		}
	}

	pub fn item_span(&self, name: Text) -> Option<FullSpan> {
		let item = self.items.get(&name)?;
		let (name, _) = name_of_item(&item.item)?;
		Some(name.span().with(self.file))
	}

	pub fn declare(&mut self, db: &dyn Db, name: Text, item: ast::Item) {
		let path = db.add(AbsPath::Name {
			prec: self.module,
			name,
		});

		self.items.insert(
			name,
			ItemData {
				item,
				file: self.file,
				path,
				sub: Vec::new(),
			},
		);
	}

	pub fn define(&mut self, name: Text) -> ItemBuilder {
		let item = self.items.get_mut(&name).expect("Item should have been declared");
		ItemBuilder { item }
	}
}

pub struct ItemBuilder<'a> {
	item: &'a mut ItemData,
}

impl ItemBuilder<'_> {
	pub fn add<T: AstElement>(&mut self, node: T) -> AstId<T> {
		let index = self.item.sub.len() as u32;
		self.item.sub.push(node.inner());
		AstId(
			ErasedAstId {
				item: self.item.path,
				index,
			},
			std::marker::PhantomData,
		)
	}

	pub fn cast<T: AstElement, U: AstElement>(&self, id: AstId<T>) -> AstId<U> {
		let elem = self.item.sub[id.0.index as usize].clone();
		if U::cast(elem).is_some() {
			AstId(
				ErasedAstId {
					item: self.item.path,
					index: id.0.index,
				},
				std::marker::PhantomData,
			)
		} else {
			panic!(
				"Cannot cast from `{}` to `{}`",
				std::any::type_name::<T>(),
				std::any::type_name::<U>()
			);
		}
	}
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum NameTy {
	Fn,
	Struct,
	Enum,
	TypeAlias,
	Static,
}

impl Debug for NameTy {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			NameTy::Fn => write!(f, "fn"),
			NameTy::Struct => write!(f, "struct"),
			NameTy::Enum => write!(f, "enum"),
			NameTy::TypeAlias => write!(f, "type alias"),
			NameTy::Static => write!(f, "static"),
		}
	}
}

pub fn build_ast_map(modules: impl IntoIterator<Item = ModuleMap>) -> (AstMap, TempMap) {
	let mut items = Vec::new();
	let mut mods = FxHashMap::default();

	for m in modules {
		items.extend(m.items.into_values());
		mods.insert(
			m.module,
			Data {
				file: m.file,
				sub: m.sub,
			},
		);
	}

	let temp = TempMap { modules: mods };
	let ast = AstMap::new(items);
	(ast, temp)
}
