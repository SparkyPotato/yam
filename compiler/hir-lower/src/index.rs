use diagnostics::Span;
use hir::ident::AbsPath;
use rustc_hash::FxHashSet;
use syntax::{
	ast,
	ast::{ImportTree, ItemKind, Name},
	AstElement,
};
use text::Text;
use tracing::{span, Level};
use verde::{query, Ctx, Id, Tracked};

use crate::module::{Declaration, Module, ModuleMap, RelPath};

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

#[derive(Eq, PartialEq, Tracked)]
pub struct Index {
	#[id]
	pub path: Id<AbsPath>,
	pub public: Id<InnerIndex>,
	pub private: Id<InnerIndex>,
}

/// Generate an index for the given module. This should be rerun on every reparse.
///
/// Note: the `ModuleMap` is a stable side-channel used to allow mapping from items to spans without invalidating
/// everything if a span changes.
#[query]
pub fn generate_index(ctx: &Ctx, module: Id<Module>, #[ignore] map: &mut ModuleMap) -> Index {
	let module = ctx.get(module);

	let s = span!(Level::DEBUG, "generate index", path = %module.file.path().display());
	let _e = s.enter();

	let mut public = InnerIndex::new(module.path, true);
	let mut private = InnerIndex::new(module.path, false);

	let mut insert = |decl: Declaration<ast::Item>, is_public: bool| {
		if let Some(name) = name_of_decl(&decl) {
			if let Some(text) = name.text() {
				let old = map.declare(ctx, text, decl);

				private.names.insert(text);
				if is_public {
					public.names.insert(text);
				}

				if let Some(old) = old {
					let old = name_of_decl(&old).unwrap().ident().unwrap().span();
					ctx.push(
						name.span()
							.with(map.file)
							.error("duplicate definition")
							.label(old.with(map.file).label("old definition here"))
							.label(name.span().with(map.file).label("redefined here")),
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
					ctx: &Ctx, public: bool, tree: ImportTree, prefix: Option<RelPath>,
					insert: &mut impl FnMut(Declaration<ast::Item>, bool),
				) {
					match tree {
						ImportTree::ListImport(i) => {
							if let Some(list) = i.import_tree_list() {
								for tree in list.import_trees() {
									visit_tree(
										ctx,
										public,
										tree,
										i.path().and_then(|path| RelPath::from_ast(ctx, prefix, path)),
										insert,
									);
								}
							}
						},
						ImportTree::RenameImport(i) => {
							let path = i.path().and_then(|path| RelPath::from_ast(ctx, prefix, path));
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
					visit_tree(ctx, public, tree, None, &mut insert);
				}
			},
			Some(_) => insert(Declaration::Item(item), public),
			None => {},
		}
	}

	Index {
		path: module.path,
		public: ctx.insert(public),
		private: ctx.insert(private),
	}
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
