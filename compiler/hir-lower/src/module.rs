use std::path::Path;

use diagnostics::FilePath;
use hir::{
	ast::{AstId, ErasedAstId, ItemData},
	ident::{AbsPath, InnerPath, PackageId},
};
use rustc_hash::FxHashMap;
use syntax::{ast, AstElement};
use text::Text;
use verde::{Db, Id, Tracked};

use crate::index::Index;

/// A declaration that is bound to a name.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Declaration<T> {
	Item(T),
	Import { path: RelPath, name: ast::Name },
}

impl<T> Declaration<T> {
	fn map<U>(self, f: impl FnOnce(T) -> U) -> Declaration<U> {
		match self {
			Declaration::Item(x) => Declaration::Item(f(x)),
			Declaration::Import { path, name } => Declaration::Import { path, name },
		}
	}

	fn map_ref<U>(&self, f: impl FnOnce(&T) -> U) -> Declaration<U> {
		match self {
			Declaration::Item(x) => Declaration::Item(f(x)),
			Declaration::Import { path, name } => Declaration::Import {
				path: *path,
				name: name.clone(),
			},
		}
	}
}

/// A map from names in a module to their declarations.
#[derive(Debug, Clone)]
pub struct ModuleMap {
	module: Id<AbsPath>,
	file: FilePath,
	items: FxHashMap<Text, Declaration<ItemData>>,
}

impl ModuleMap {
	pub fn new(module: Id<AbsPath>, file: FilePath) -> Self {
		Self {
			module,
			file,
			items: FxHashMap::default(),
		}
	}

	pub fn declare(&mut self, db: &dyn Db, name: Text, item: Declaration<ast::Item>) -> Option<Declaration<ast::Item>> {
		let prec = db.geti(self.module);
		let path = db.add(InnerPath { prec: prec.path, name });
		let path = db.add(AbsPath {
			package: prec.package,
			path: Some(path),
		});
		let old = self.items.insert(
			name,
			item.map(|item| ItemData {
				item,
				file: self.file,
				path,
				sub: Vec::new(),
			}),
		);
		old.map(|x| x.map_ref(|x| x.item.clone()))
	}

	fn define(&mut self, name: Text) -> Option<ItemBuilder> {
		let decl = self
			.items
			.entry(name)
			.or_insert_with(|| panic!("Tried to define item that wasn't previously declared"));
		match decl {
			Declaration::Item(item) => Some(ItemBuilder { item }),
			Declaration::Import { .. } => None,
		}
	}
}

pub struct ItemBuilder<'a> {
	item: &'a mut ItemData,
}

impl ItemBuilder<'_> {
	fn add<T: AstElement>(&mut self, node: T) -> AstId<T> {
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

pub struct Module {
	pub path: Id<AbsPath>,
	pub file: FilePath,
	pub ast: ast::File,
}

impl Module {
	pub fn new(ast: ast::File, file: FilePath, path: Id<AbsPath>) -> Self { Self { path, file, ast } }

	/// Figure out the module's path from it's relative file path from a root file.
	///
	/// `prefix` is a prefix to add to path - possibly just the package ID if we're looking at the root module.
	pub fn from_file(db: &dyn Db, root: FilePath, ast: ast::File, file: FilePath, prefix: Id<AbsPath>) -> Self {
		if root == file {
			return Self::new(ast, file, prefix);
		}

		let root = root.path().parent().expect("root must be a file");
		// Ensure path is something like `src/x/x.yam`.
		let p = file.path();
		assert_eq!(p.extension(), Some("yam".as_ref()), "Path is not a .yam file");
		let relative = p.strip_prefix(root).expect("Path is not a child of the root");
		// `relative` is now `x/x.yam`.

		let mut last_name = String::new();
		let mut prec = *db.geti(prefix);
		for component in relative.components() {
			let path: &Path = component.as_ref(); // `x` or `x.yam`.
			let path = path.to_str().expect("Path is not valid UTF-8");

			let file_name = path.strip_suffix(".yam");
			if file_name != Some(last_name.as_str()) {
				// Create a child module if we aren't `x/x.yam`.
				let path = file_name.unwrap_or(path);
				last_name = path.to_string();
				let ident = Text::new(path);

				let path = db.add(InnerPath {
					prec: prec.path,
					name: ident,
				});
				let path = AbsPath {
					package: prec.package,
					path: Some(path),
				};
				prec = path;
			}
		}

		Self::new(ast, file, db.add(prec))
	}
}

/// A relative path. Loosely wraps `InnerPath` with a few extra rules:
/// - the initial `name` may be `.` - this signifies searching from the global namespace.
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct RelPath {
	inner: Id<InnerPath>,
}

impl RelPath {
	pub fn from_ast(db: &dyn Db, prefix: Option<Self>, path: ast::Path) -> Option<Self> {
		let prec = path
			.qualifier()
			.map(|prec| Self::from_ast(db, prefix, prec))
			.unwrap_or(prefix);
		let inner = db.add(InnerPath {
			prec: prec.map(|x| x.inner),
			name: match path.segment()? {
				ast::PathSegment::Name(name) => name.text()?,
				ast::PathSegment::Dot(_) if prec.is_none() => Text::new("."),
				ast::PathSegment::Dot(_) => return prec,
			},
		});
		Some(Self { inner })
	}
}

pub struct ModuleTree {
	pub modules: FxHashMap<Id<AbsPath>, Index>,
	pub packages: FxHashMap<PackageId, Id<PackageTree>>,
}

#[derive(Tracked, Eq, PartialEq)]
pub struct PackageTree {
	#[id]
	path: Id<AbsPath>,
	modules: FxHashMap<Text, Id<PackageTree>>,
}

struct TempTree {
	path: Id<AbsPath>,
	modules: FxHashMap<Text, TempTree>,
}

impl ModuleTree {
	pub fn new(db: &dyn Db, indices: impl IntoIterator<Item = Index>) -> Self {
		let mut modules = FxHashMap::default();
		let mut packages = FxHashMap::default();

		for index in indices {
			let path = db.geti(index.path);
			let mut tree = packages.entry(path.package).or_insert_with(|| TempTree {
				path: db.add(AbsPath {
					package: path.package,
					path: None,
				}),
				modules: FxHashMap::default(),
			});

			fn visit<'a>(db: &dyn Db, tree: &'a mut TempTree, path: Id<AbsPath>) -> &'a mut TempTree {
				// borrowck doesn't let me use combinators :(.
				let path = db.geti(path);
				if let Some(inner) = path.path {
					let p = db.geti(inner);
					let tree = visit(
						db,
						tree,
						db.add(AbsPath {
							package: path.package,
							path: p.prec,
						}),
					);

					tree.modules.entry(p.name).or_insert_with(|| TempTree {
						path: db.add(AbsPath {
							package: path.package,
							path: Some(inner),
						}),
						modules: FxHashMap::default(),
					})
				} else {
					tree
				}
			}

			visit(db, &mut tree, index.path);
			modules.insert(index.path, index);
		}

		fn temp_to_ptree(db: &dyn Db, tree: TempTree) -> Id<PackageTree> {
			let mut modules = FxHashMap::default();
			for (name, tree) in tree.modules {
				modules.insert(name, temp_to_ptree(db, tree));
			}
			db.set_input(PackageTree {
				path: tree.path,
				modules,
			})
		}

		let packages = packages
			.into_iter()
			.map(|(id, tree)| (id, temp_to_ptree(db, tree)))
			.collect();
		Self { modules, packages }
	}
}
