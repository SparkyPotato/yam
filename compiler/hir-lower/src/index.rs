use diagnostics::{FileDiagnostic, FilePath, Span};
use rustc_hash::{FxHashMap, FxHashSet};
use syntax::{
	ast,
	ast::{ImportTree, ItemKind, Name},
	AstElement,
};
use text::Text;
use tracing::{span, Level};
use verde::{Db, Id, Tracked};

use crate::{Declaration, Module, ModuleMap};

/// An index for the public interface of a module - visible to modules outside itself.
pub type PublicIndex = InnerIndex<true>;
/// An index for the private interface of a module - visible only to modules inside itself (and itself).
pub type PrivateIndex = InnerIndex<false>;

#[derive(Tracked, Debug, Eq, PartialEq)]
pub struct InnerIndex<const PUBLIC: bool> {
	#[id]
	path: FilePath,
	/// The names that exist in this module. Use the `ModuleMap` to figure out what they actually point to.
	names: FxHashSet<Text>,
}

impl<const PUBLIC: bool> InnerIndex<PUBLIC> {
	pub fn new(path: FilePath) -> Self {
		Self {
			path,
			names: FxHashSet::default(),
		}
	}
}

pub struct Index {
	pub public: Id<PublicIndex>,
	pub private: Id<PrivateIndex>,
	pub map: ModuleMap,
}

/// Generate an index for the given module. This should be rerun on every reparse.
pub fn generate_index(db: &dyn Db, module: &Module) -> (Index, Vec<FileDiagnostic>) {
	let s = span!(Level::TRACE, "generate index", path = %module.file.path().display());
	let _e = s.enter();

	let mut diags = Vec::new();
	let mut public = InnerIndex::new(module.file);
	let mut private = InnerIndex::new(module.file);
	let mut map = ModuleMap::new(module.path, module.file);

	let mut insert = |decl: Declaration<ast::Item>, is_public: bool| {
		if let Some(name) = name_of_decl(&decl) {
			if let Some(text) = name.text() {
				let old = map.declare(db, text, decl);

				private.names.insert(text);
				if is_public {
					public.names.insert(text);
				}

				if let Some(old) = old {
					let old = name_of_decl(&old).unwrap().ident().unwrap().span();
					diags.push(
						name.span()
							.error("duplicate definition")
							.label(old.label("old definition here"))
							.label(name.span().label("redefined here")),
					);
				}
			}
		}
	};

	for item in module.ast.items() {
		let public = item.visibility().is_some();
		match item.item_kind() {
			Some(ItemKind::Import(i)) => {
				fn visit_tree(
					db: &dyn Db, public: bool, tree: ImportTree, prefix: Option<Id<hir::Path>>,
					insert: &mut impl FnMut(Declaration<ast::Item>, bool),
				) {
					match tree {
						ImportTree::ListImport(i) => {
							if let Some(list) = i.import_tree_list() {
								for tree in list.import_trees() {
									visit_tree(
										db,
										public,
										tree,
										i.path().and_then(|path| hir::Path::from_ast(db, prefix, path)),
										insert,
									);
								}
							}
						},
						ImportTree::RenameImport(i) => {
							let path = i.path().and_then(|path| hir::Path::from_ast(db, prefix, path));
							let name = i
								.rename()
								.and_then(|r| r.name())
								.or_else(|| i.path().and_then(|x| x.segment().and_then(|x| x.name())));

							if let Some(path) = path {
								let decl = Declaration::Import {
									path,
									name: name.unwrap(),
								};
								insert(decl.clone(), public);
							}
						},
					}
				}
				if let Some(tree) = i.import_tree() {
					visit_tree(db, public, tree, None, &mut insert);
				}
				continue;
			},
			Some(_) => insert(Declaration::Item(item), public),
			None => continue,
		}
	}

	(
		Index {
			public: db.set_input(public),
			private: db.set_input(private),
			map,
		},
		diags,
	)
}

fn name_of_decl(decl: &Declaration<ast::Item>) -> Option<Name> {
	match decl {
		Declaration::Item(item) => name_of_item(item),
		Declaration::Import { name, .. } => Some(name.clone()),
	}
}

fn name_of_item(item: &ast::Item) -> Option<Name> {
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

#[derive(Debug)]
pub struct ModuleTree {
	children: FxHashMap<Text, ModuleTree>,
}

impl ModuleTree {
	pub fn new<'a>(db: &dyn Db, indices: impl IntoIterator<Item = &'a Index>) -> Self {
		let mut this = Self {
			children: FxHashMap::default(),
		};
		for index in indices {
			fn insert<'a>(db: &dyn Db, current: &'a mut ModuleTree, path: Option<Id<hir::Path>>) -> &'a mut ModuleTree {
				match path {
					Some(path) => {
						let path = db.geti(path);
						let tree = insert(db, current, path.prec);
						tree.children.entry(path.ident).or_insert_with(|| ModuleTree {
							children: FxHashMap::default(),
						})
					},
					None => current,
				}
			}

			insert(db, &mut this, index.map.module);
		}
		this
	}
}
