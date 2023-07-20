use diagnostics::{FilePath, FullSpan, Span};
use hir::{
	ast::{AstId, ErasedAstId, ItemData},
	ident::AbsPath,
	AstMap,
};
use rustc_hash::FxHashMap;
use syntax::{ast, token, AstElement, SyntaxElement};
use text::Text;
use verde::{Ctx, Db, Id};

use self::local::name_of_item;

pub mod local;

/// A relative path.
#[derive(Clone, Default)]
pub struct RelPath {
	dot: Option<AstId<token::Dot>>,
	names: Vec<hir::Name>,
}

impl RelPath {
	pub fn from_ast(ctx: &Ctx, prefix: Option<RelPath>, path: ast::Path, map: &mut ModuleMap) -> Self {
		let mut this = prefix.unwrap_or_default();
		let mut prec_dot_allowed = this.dot.is_some();

		for segment in path.path_segments() {
			match segment {
				ast::PathSegment::Dot(dot) => {
					if !prec_dot_allowed {
						ctx.push(
							dot.span()
								.error("global qualifier is only allowed at the start of paths"),
						);
					} else if this.dot.is_none() {
						this.dot = Some(map.add(dot));
					}
				},
				ast::PathSegment::Name(n) => {
					if let Some(name) = n.text() {
						this.names.push(hir::Name { name, id: map.add(n) });
					}
				},
			}
		}

		this
	}
}

pub enum Declaration {
	Item(ItemData),
	Import { path: RelPath, rename: Option<hir::Name> },
}

/// A map from names in a module to their declarations.
pub struct ModuleMap {
	module: Id<AbsPath>,
	pub(crate) file: FilePath,
	items: FxHashMap<Text, Declaration>,
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

	pub fn get(&mut self, name: Text) -> Option<&Declaration> { self.items.get(&name) }

	pub fn add<T: AstElement>(&mut self, value: T) -> AstId<T> {
		let index = self.sub.len() as u32;
		self.sub.push(value.inner());
		AstId(
			ErasedAstId {
				item: self.module,
				index,
			},
			std::marker::PhantomData,
		)
	}

	pub fn span(&self, id: AstId<impl AstElement>) -> FullSpan {
		let element = self.sub[id.0.index as usize];
		let span = element.text_range();
		FullSpan {
			start: span.start().into(),
			end: span.end().into(),
			relative: self.file,
		}
	}

	pub fn import(
		&mut self, db: &dyn Db, name: Text, path: RelPath, rename: Option<hir::Name>,
	) -> Option<(FullSpan, bool)> {
		let old = self.items.insert(name, Declaration::Import { path, rename });
		self.decl_to_span(old)
	}

	pub fn declare(&mut self, db: &dyn Db, name: Text, item: ast::Item) -> Option<(FullSpan, bool)> {
		let path = db.add(AbsPath::Module {
			prec: self.module,
			name,
		});

		let old = self.items.insert(
			name,
			Declaration::Item(ItemData {
				item,
				file: self.file,
				path,
				sub: Vec::new(),
			}),
		);

		self.decl_to_span(old)
	}

	pub fn define(&mut self, name: Text) -> ItemBuilder {
		let decl = self
			.items
			.get_mut(&name)
			.expect("Tried to define item that wasn't previously declared");
		match decl {
			Declaration::Item(item) => ItemBuilder { item },
			Declaration::Import { .. } => panic!("Tried to define an import"),
		}
	}

	fn decl_to_span(&self, decl: Option<Declaration>) -> Option<(FullSpan, bool)> {
		decl.and_then(|old| match old {
			Declaration::Item(i) => name_of_item(&i.item).map(|x| (x.span().with(self.file), true)),
			Declaration::Import { rename, path } => rename
				.or_else(|| path.names.into_iter().last())
				.map(|x| (self.span(x.id), false)),
		})
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
}

pub fn build_ast_map(modules: impl IntoIterator<Item = ModuleMap>) -> AstMap {
	let items = modules.into_iter().flat_map(|x| {
		x.items.into_iter().flat_map(|(_, x)| match x {
			Declaration::Item(i) => Some(i),
			Declaration::Import { .. } => None,
		})
	});
	AstMap::new(items)
}
