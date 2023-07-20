use diagnostics::Span;
use hir::ident::{AbsPath, PackageId};
use rustc_hash::{FxHashMap, FxHashSet};
use syntax::{
	ast,
	ast::{ImportTree, ItemKind, Name},
	AstElement,
};
use text::Text;
use tracing::{span, Level};
use verde::{query, Ctx, Db, Id, Tracked};

use crate::{
	index::{ModuleMap, RelPath},
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

	let mut public = InnerIndex::new(module.path, true);
	let mut private = InnerIndex::new(module.path, false);

	for item in module.ast.items() {
		let is_public = item.visibility().is_some();
		match item.item_kind() {
			Some(ItemKind::Import(i)) => {
				fn visit_tree(ctx: &Ctx, public: bool, tree: ImportTree, prefix: Option<RelPath>, map: &mut ModuleMap) {
					match tree {
						ImportTree::ListImport(i) => {
							if let Some(list) = i.import_tree_list() {
								for tree in list.import_trees() {
									visit_tree(
										ctx,
										public,
										tree,
										i.path().map(|path| RelPath::from_ast(ctx, prefix.clone(), path, map)),
										map,
									);
								}
							}
						},
						ImportTree::RenameImport(i) => {
							let path = i.path().map(|path| RelPath::from_ast(ctx, prefix, path, map));
							let rename = i.rename().and_then(|r| r.name()).and_then(|x| {
								let name = x.text()?;
								Some(hir::Name { name, id: map.add(x) })
							});

							let Some(path) = path else {
								return;
							};
							let Some(name) = rename.or_else(|| path.names.last().copied()) else {
								return;
							};
							let old = map.import(ctx, name.name, path, rename);

							if let Some((old, item)) = old {
								let span = map.span(name.id);
								ctx.push(
									span.error("name already defined in module")
										.label(old.label(if item { "old definition here" } else { "old import here" }))
										.label(span.label("imported here")),
								);
							}
						},
					}
				}

				if let Some(tree) = i.import_tree() {
					visit_tree(ctx, is_public, tree, None, map);
				}
			},
			Some(_) => {
				if let Some(name) = name_of_item(&item) {
					if let Some(text) = name.text() {
						let old = map.declare(ctx, text, item);
						private.names.insert(text);
						if is_public {
							public.names.insert(text);
						}

						if let Some((old, item)) = old {
							ctx.push(
								name.span()
									.with(map.file)
									.error("name already defined in module")
									.label(old.label(if item { "old definition here" } else { "old import here" }))
									.label(name.span().with(map.file).label("redefined here")),
							);
						}
					}
				}
			},
			None => {},
		}
	}

	Index {
		path: module.path,
		public: ctx.insert(public),
		private: ctx.insert(private),
	}
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
				AbsPath::Module { prec, .. } => p = *db.geti(prec),
			}
		};

		let mut tree = packages.entry(package).or_insert_with(|| TempTree {
			path: db.add(package.into()),
			index: None,
			children: FxHashMap::default(),
		});

		fn make_tree<'a>(db: &dyn Db, tree: &'a mut TempTree, path: Id<AbsPath>) -> &'a mut TempTree {
			match *db.geti(path) {
				AbsPath::Package(_) => tree,
				AbsPath::Module { prec, name } => {
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

	fn realify(ctx: &Ctx, modules: &mut FxHashMap<Id<AbsPath>, Id<ModuleTree>>, temp: TempTree) -> Id<ModuleTree> {
		let path = temp.path;
		let real = ModuleTree {
			path,
			index: temp.index,
			children: temp
				.children
				.into_iter()
				.map(|(k, v)| (k, realify(ctx, modules, v)))
				.collect(),
		};
		let ret = ctx.insert(real);
		modules.insert(path, ret);
		ret
	}
	let mut modules = FxHashMap::default();

	PackageTree {
		id: (),
		packages: packages
			.into_iter()
			.map(|(k, v)| (k, realify(db, &mut modules, v)))
			.collect(),
		modules,
	}
}

#[derive(Eq, PartialEq, Tracked)]
pub struct Index {
	#[id]
	pub path: Id<AbsPath>,
	pub public: Id<InnerIndex>,
	pub private: Id<InnerIndex>,
}

#[derive(Tracked, Debug, Eq, PartialEq)]
pub struct InnerIndex {
	#[id]
	path: (Id<AbsPath>, bool),
	/// The names that exist in this module. Use the `ModuleMap` to figure out what they actually point to.
	names: FxHashSet<Text>,
}

impl InnerIndex {
	pub fn new(path: Id<AbsPath>, public: bool) -> Self {
		Self {
			path: (path, public),
			names: FxHashSet::default(),
		}
	}
}

#[derive(Tracked, Debug)]
pub struct PackageTree {
	#[id]
	id: (),
	pub(crate) packages: FxHashMap<PackageId, Id<ModuleTree>>,
	pub(crate) modules: FxHashMap<Id<AbsPath>, Id<ModuleTree>>,
}

impl Eq for PackageTree {}
impl PartialEq for PackageTree {
	fn eq(&self, other: &Self) -> bool {
		// Don't compare `modules` as that's just a shortcut for recursively finding a module tree.
		self.packages == other.packages
	}
}

#[derive(Tracked, Eq, PartialEq, Debug)]
pub struct ModuleTree {
	#[id]
	pub(crate) path: Id<AbsPath>,
	pub(crate) index: Option<Id<Index>>,
	pub(crate) children: FxHashMap<Text, Id<ModuleTree>>,
}

struct TempTree {
	path: Id<AbsPath>,
	index: Option<Id<Index>>,
	children: FxHashMap<Text, TempTree>,
}

pub(crate) fn name_of_item(item: &ast::Item) -> Option<Name> {
	match item.item_kind() {
		Some(ItemKind::Fn(f)) => f.name(),
		Some(ItemKind::Struct(s)) => s.name(),
		Some(ItemKind::Enum(e)) => e.name(),
		Some(ItemKind::TypeAlias(t)) => t.name(),
		Some(ItemKind::Static(s)) => s.name(),
		Some(ItemKind::Import(_)) => None,
		None => None,
	}
}
