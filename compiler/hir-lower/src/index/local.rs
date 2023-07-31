use diagnostics::{FullSpan, Span};
use hir::ident::{AbsPath, PackageId};
use rustc_hash::{FxHashMap, FxHashSet};
use syntax::{ast, token, AstElement};
use text::Text;
use tracing::{span, Level};
use verde::{query, Ctx, Db, Id, Tracked};

use crate::{
	index::{ModuleMap, NameTy, TempId},
	Module,
};

/// Generate an index for the given module. This should be rerun on every reparse.
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
				let public = item.visibility().is_some();
				if let Some(tree) = i.import_tree() {
					gen.import(public, tree, RelPath::default());
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
		let real = ModuleTree {
			path: temp.path,
			index: temp.index,
			children: temp.children.into_iter().map(|(k, v)| (k, realify(ctx, v))).collect(),
		};
		ctx.insert(real)
	}

	PackageTree {
		id: (),
		packages: packages.into_iter().map(|(k, v)| (k, realify(db, v))).collect(),
	}
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
}

#[derive(Eq, PartialEq)]
pub enum Declaration {
	Name {
		ty: NameTy,
		id: TempId<ast::Name>,
	},
	Import {
		path: RelPath,
		rename: Option<TempId<ast::Name>>,
	},
}

#[derive(Tracked, Eq, PartialEq)]
pub struct Index {
	#[id]
	pub path: Id<AbsPath>,
	pub public: Id<PublicIndex>,
	pub decls: FxHashMap<Text, Declaration>,
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
	id: (),
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
}

impl<'a> IndexGen<'a> {
	fn new(ctx: &'a Ctx<'a>, map: &'a mut ModuleMap, path: Id<AbsPath>) -> Self {
		Self {
			ctx,
			map,
			path,
			public: PublicIndex::new(path),
			decls: FxHashMap::default(),
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

		if public {
			self.public.names.insert(text);
		}
		let old = self.decls.insert(
			text,
			Declaration::Name {
				ty,
				id: self.map.add_temp(name.clone()),
			},
		);
		self.map.declare(self.ctx, text, item);

		if let Some((old, item)) = self.span(text, old) {
			let span = name.span().with(self.map.file);
			self.ctx.push(
				span.error("cannot redefine name in module")
					.label(old.label(if item { "old definition here" } else { "old import here" }))
					.label(span.label("redefined here")),
			);
		}
	}

	fn import(&mut self, public: bool, tree: ast::ImportTree, prefix: RelPath) {
		match tree {
			ast::ImportTree::ListImport(i) => {
				let prefix = i
					.path()
					.map(|path| RelPath::from_ast(self.ctx, prefix, path, self.map))
					.unwrap_or(RelPath::default());
				if let Some(list) = i.import_tree_list() {
					for tree in list.import_trees() {
						self.import(public, tree, prefix.clone());
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
			Declaration::Import { path, rename } => {
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
		}
	}
}
