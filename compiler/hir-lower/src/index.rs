use std::{fmt::Debug, hash::Hash};

use diagnostics::{FilePath, FullSpan, RawSpan, Span};
use hir::{
	ast::{AstId, AstMap, ErasedAstId, ItemData, ItemElement},
	ident::{AbsPath, PackageId},
};
use rustc_hash::{FxHashMap, FxHashSet};
use syntax::{ast, token, AstElement, SyntaxElement};
use text::Text;
use tracing::{span, Level};
use verde::{query, Ctx, Db, Id, Tracked};

use crate::Module;

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

	pub fn declare(&mut self, db: &dyn Db, name: Text, item: ast::Item) -> Id<AbsPath> {
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

		path
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
	pub fn add<T: AstElement>(&mut self, node: Option<T>) -> AstId<T> {
		match node {
			Some(t) => self.add_conc(t),
			None => self.add_errored(),
		}
	}

	pub fn add_conc<T: AstElement>(&mut self, node: T) -> AstId<T> {
		let index = self.item.sub.len() as u32;
		self.item.sub.push(ItemElement::Concrete(node.inner()));
		AstId(
			ErasedAstId {
				item: self.item.path,
				index,
			},
			std::marker::PhantomData,
		)
	}

	pub fn add_errored<T: AstElement>(&mut self) -> AstId<T> {
		let index = self.item.sub.len() as u32;
		self.item.sub.push(ItemElement::Error);
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
		let valid = match elem {
			ItemElement::Concrete(elem) => U::cast(elem).is_some(),
			ItemElement::Error => true,
		};
		if valid {
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

/// Generate a local index for the given module. This should be rerun on every reparse.
///
/// The `ModuleMap` is a stable side-channel used to allow mapping from items to spans without invalidating
/// everything if a span changes.
#[query]
pub fn generate_index(ctx: &Ctx, module: Id<Module>, #[ignore] map: &mut ModuleMap) -> Index {
	let module = ctx.get(module);

	let s = span!(Level::DEBUG, "generate index", path = %module.file);
	let _e = s.enter();

	let mut gen = IndexGen::new(ctx, map, module.path);

	for item in module.ast.items() {
		match item.item_kind() {
			Some(ast::ItemKind::Import(i)) => {
				let is_prelude = is_prelude(ctx, item.attributes());
				let public = item.visibility().is_some();
				if let Some(tree) = i.import_tree() {
					gen.import(public, tree, is_prelude, RelPath::default());
				}
			},
			Some(_) => gen.item(item),
			None => {},
		}
	}

	gen.finish()
}

#[query]
pub fn build_package_tree(db: &Ctx, indices: &[Id<Index>]) -> PackageTree {
	let s = span!(Level::DEBUG, "generate package tree");
	let _e = s.enter();

	let mut packages = FxHashMap::default();

	for &index in indices {
		let path = db.get(index).path;
		let mut p = *db.geti(path);
		let package = loop {
			match p {
				AbsPath::Package(p) => break p,
				AbsPath::Name { prec, .. } => p = *db.geti(prec),
			}
		};

		let tree = packages.entry(package).or_insert_with(|| TempTree {
			path: db.add(package.into()),
			index: None,
			children: FxHashMap::default(),
		});

		fn make_tree<'a>(db: &dyn Db, tree: &'a mut TempTree, path: Id<AbsPath>) -> &'a mut TempTree {
			match *db.geti(path) {
				AbsPath::Package(_) => tree,
				AbsPath::Name { prec, name } => {
					let prec = make_tree(db, tree, prec);
					prec.children.entry(name).or_insert_with(|| TempTree {
						path,
						index: None,
						children: FxHashMap::default(),
					})
				},
			}
		}

		make_tree(db, tree, path).index = Some(index);
	}

	fn realify(ctx: &Ctx, temp: TempTree) -> Id<ModuleTree> {
		let children: FxHashMap<_, _> = temp.children.into_iter().map(|(k, v)| (k, realify(ctx, v))).collect();
		if let Some(index) = temp.index {
			let index = ctx.get(index);
			let public = ctx.get(index.public);
			for (name, decl) in index.decls.iter() {
				if children.contains_key(name) && public.names.contains(&name) {
					let id = match decl {
						Declaration::Name { id, .. } => id.erased(),
						Declaration::Import { path, rename, .. } => {
							// Re-exporting child modules is fine
							if path.names.len() == 1 && path.dot.is_none() && path.names[0].name == *name {
								continue;
							}
							rename.map(|x| x.erased()).unwrap_or_else(|| path.span())
						},
					};
					ctx.push(id.error("child module shadows this item").label(id.mark()));
				}
			}
		}
		let real = ModuleTree {
			path: temp.path,
			index: temp.index,
			children,
		};
		ctx.insert(real)
	}

	PackageTree {
		id: (),
		packages: packages.into_iter().map(|(k, v)| (k, realify(db, v))).collect(),
	}
}

fn is_prelude(ctx: &Ctx, attribs: impl Iterator<Item = ast::Attribute>) -> bool {
	let mut found: Option<RawSpan<()>> = None;
	for ((name, span), tt) in attribs.filter_map(|a| {
		a.name()
			.and_then(|x| x.text().map(|y| (y, x.span())))
			.map(|n| (n, a.token_tree()))
	}) {
		if name.as_str() == "prelude" {
			if let Some(old) = found {
				ctx.push(
					span.error("repeated `@prelude` attributes have no effect")
						.label(span.label("repeated here"))
						.label(old.label("previously here")),
				);
			}
			if let Some(tt) = tt {
				ctx.push(
					tt.span()
						.error("`@prelude` cannot have an associated token tree")
						.label(tt.span().mark()),
				);
			}

			found = Some(span);
		}
	}

	found.is_some()
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct TempName {
	pub name: Text,
	pub id: TempId<ast::Name>,
}

#[derive(Clone, Default, Eq, PartialEq, Debug)]
pub struct RelPath {
	pub dot: Option<TempId<token::Dot>>,
	pub names: Vec<TempName>,
}

impl RelPath {
	pub fn from_ast(ctx: &Ctx, prefix: RelPath, path: ast::Path, map: &mut ModuleMap) -> Self {
		let mut this = prefix;
		let prec_dot_allowed = this.dot.is_none() && this.names.is_empty();
		let mut should_push_dot = true;

		for segment in path.path_segments() {
			match segment {
				ast::PathSegment::Dot(dot) => {
					if should_push_dot {
						if !prec_dot_allowed {
							let span = dot.span().with(map.file);
							ctx.push(
								span.error("global qualifier is only allowed at the start of paths")
									.label(span.mark()),
							);
						}

						this.dot = Some(map.add_temp(dot));
						should_push_dot = false;
					}
				},
				ast::PathSegment::Name(n) => {
					if let Some(name) = n.text() {
						this.names.push(TempName {
							name,
							id: map.add_temp(n),
						});
					}
					should_push_dot = false;
				},
			}
		}

		this
	}

	pub fn span(&self) -> ErasedTempId {
		self.names
			.last()
			.map(|x| x.id.erased())
			.unwrap_or_else(|| self.dot.unwrap().erased())
	}
}

#[derive(Eq, PartialEq)]
pub enum Declaration {
	Name {
		path: Id<AbsPath>,
		ty: NameTy,
		id: TempId<ast::Name>,
	},
	Import {
		path: RelPath,
		is_prelude: bool,
		rename: Option<TempId<ast::Name>>,
	},
}

#[derive(Tracked, Eq, PartialEq)]
pub struct Index {
	#[id]
	pub path: Id<AbsPath>,
	pub public: Id<PublicIndex>,
	pub decls: FxHashMap<Text, Declaration>,
	pub has_prelude: bool,
}

#[derive(Tracked, Debug, Eq, PartialEq)]
pub struct PublicIndex {
	#[id]
	path: Id<AbsPath>,
	pub names: FxHashSet<Text>,
}

impl PublicIndex {
	pub fn new(path: Id<AbsPath>) -> Self {
		Self {
			path,
			names: FxHashSet::default(),
		}
	}
}

#[derive(Tracked, Eq, PartialEq)]
pub struct PackageTree {
	#[id]
	pub id: (),
	pub packages: FxHashMap<PackageId, Id<ModuleTree>>,
}

#[derive(Tracked, Eq, PartialEq)]
pub struct ModuleTree {
	#[id]
	pub path: Id<AbsPath>,
	pub index: Option<Id<Index>>,
	pub children: FxHashMap<Text, Id<ModuleTree>>,
}

struct TempTree {
	path: Id<AbsPath>,
	index: Option<Id<Index>>,
	children: FxHashMap<Text, TempTree>,
}

pub(crate) fn name_of_item(item: &ast::Item) -> Option<(ast::Name, NameTy)> {
	match item.item_kind() {
		Some(ast::ItemKind::Fn(f)) => f.name().map(|x| (x, NameTy::Fn)),
		Some(ast::ItemKind::Struct(s)) => s.name().map(|x| (x, NameTy::Struct)),
		Some(ast::ItemKind::Enum(e)) => e.name().map(|x| (x, NameTy::Enum)),
		Some(ast::ItemKind::TypeAlias(t)) => t.name().map(|x| (x, NameTy::TypeAlias)),
		Some(ast::ItemKind::Static(s)) => s.name().map(|x| (x, NameTy::Static)),
		Some(ast::ItemKind::Import(_)) | None => None,
	}
}

struct IndexGen<'a> {
	ctx: &'a Ctx<'a>,
	map: &'a mut ModuleMap,
	path: Id<AbsPath>,
	public: PublicIndex,
	decls: FxHashMap<Text, Declaration>,
	has_prelude: bool,
}

impl<'a> IndexGen<'a> {
	fn new(ctx: &'a Ctx<'a>, map: &'a mut ModuleMap, path: Id<AbsPath>) -> Self {
		Self {
			ctx,
			map,
			path,
			public: PublicIndex::new(path),
			decls: FxHashMap::default(),
			has_prelude: false,
		}
	}

	fn item(&mut self, item: ast::Item) {
		let public = item.visibility().is_some();

		let Some((name, ty)) = name_of_item(&item) else {
			return;
		};
		let Some(text) = name.text() else {
			return;
		};

		let path = self.map.declare(self.ctx, text, item);
		if public {
			self.public.names.insert(text);
		}
		let old = self.decls.insert(
			text,
			Declaration::Name {
				path,
				ty,
				id: self.map.add_temp(name.clone()),
			},
		);

		if let Some((old, item)) = self.span(text, old) {
			let span = name.span().with(self.map.file);
			self.ctx.push(
				span.error("cannot redefine name in module")
					.label(old.label(if item { "old definition here" } else { "old import here" }))
					.label(span.label("redefined here")),
			);
		}
	}

	fn import(&mut self, public: bool, tree: ast::ImportTree, is_prelude: bool, prefix: RelPath) {
		if is_prelude {
			self.has_prelude = true;
		}

		match tree {
			ast::ImportTree::ListImport(i) => {
				let prefix = i
					.path()
					.map(|path| RelPath::from_ast(self.ctx, prefix, path, self.map))
					.unwrap_or(RelPath::default());
				if let Some(list) = i.import_tree_list() {
					for tree in list.import_trees() {
						self.import(public, tree, is_prelude, prefix.clone());
					}
				}
			},
			ast::ImportTree::RenameImport(i) => {
				let path = i.path().map(|path| RelPath::from_ast(self.ctx, prefix, path, self.map));
				let rename = i.rename().and_then(|r| r.name()).and_then(|x| {
					let name = x.text()?;
					Some(TempName {
						name,
						id: self.map.add_temp(x),
					})
				});

				let Some(path) = path else {
					return;
				};
				let Some(name) = rename.or_else(|| path.names.last().copied()) else {
					return;
				};

				if public {
					self.public.names.insert(name.name);
				}
				let old = self.decls.insert(
					name.name,
					Declaration::Import {
						path,
						rename: rename.map(|x| x.id),
						is_prelude,
					},
				);

				if let Some((old, item)) = self.span(name.name, old) {
					let span = self.map.span(name.id);
					self.ctx.push(
						span.error("name already defined in module")
							.label(old.label(if item { "old definition here" } else { "old import here" }))
							.label(span.label("imported here")),
					);
				}
			},
		}
	}

	fn span(&self, name: Text, decl: Option<Declaration>) -> Option<(FullSpan, bool)> {
		match decl? {
			Declaration::Name { .. } => self.map.item_span(name).map(|x| (x, true)),
			Declaration::Import { path, rename, .. } => {
				let id = rename.or_else(|| path.names.into_iter().last().map(|x| x.id))?;
				Some((self.map.span(id), false))
			},
		}
	}

	fn finish(self) -> Index {
		Index {
			path: self.path,
			public: self.ctx.insert(self.public),
			decls: self.decls,
			has_prelude: self.has_prelude,
		}
	}
}
