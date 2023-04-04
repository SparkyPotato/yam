use diagnostics::FilePath;
use hir::RawPathInner;
use rustc_hash::FxHashMap;
use syntax::{
	ast::{ImportTree, ItemKind},
	OptionNameExt,
};
use text::Text;
use verde::{query, Ctx, Id, Tracked};

use crate::Module;

#[derive(Tracked, Debug)]
pub struct Index {
	#[id]
	path: Id<RawPathInner>,
	file: FilePath,
	map: FxHashMap<IndexEntry, ItemData>,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
enum Namespace {
	Type,
	Value,
	Unknown,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
struct IndexEntry {
	name: Text,
	ns: Namespace,
}

#[derive(Debug)]
struct ItemData {
	public: bool,
}

impl Index {
	pub fn new(path: Id<RawPathInner>, file: FilePath) -> Self {
		Self {
			path,
			file,
			map: FxHashMap::default(),
		}
	}
}

impl PartialEq for Index {
	fn eq(&self, _: &Self) -> bool { false }
}
impl Eq for Index {}

#[query]
pub fn generate_index(ctx: &Ctx, path: Id<RawPathInner>, module: Id<Module>) -> Index {
	let module = ctx.get(module);
	let mut index = Index::new(path, module.path);
	for item in module.ast.items() {
		let public = item.visibility().is_some();
		let (name, ns) = match item.item_kind() {
			Some(ItemKind::Fn(f)) => (f.name(), Namespace::Value),
			Some(ItemKind::Struct(s)) => (s.name(), Namespace::Type),
			Some(ItemKind::Enum(e)) => (e.name(), Namespace::Type),
			Some(ItemKind::TypeAlias(t)) => (t.name(), Namespace::Type),
			Some(ItemKind::Static(s)) => (s.name(), Namespace::Value),
			Some(ItemKind::Import(i)) => {
				fn visit_tree(index: &mut Index, public: bool, tree: Option<ImportTree>) {
					match tree {
						Some(ImportTree::ListImport(i)) => {
							if let Some(list) = i.import_tree_list() {
								list.import_trees().for_each(|x| visit_tree(index, public, Some(x)));
							}
						},
						Some(ImportTree::RenameImport(i)) => {
							if let Some(name) = i
								.rename()
								.and_then(|r| r.name().text())
								.or_else(|| i.path().and_then(|x| x.segment().text()))
							{
								index.map.insert(
									IndexEntry {
										name,
										ns: Namespace::Unknown,
									},
									ItemData { public },
								);
							}
						},
						None => {},
					}
				}
				visit_tree(&mut index, public, i.import_tree());
				(None, Namespace::Unknown)
			},
			None => (None, Namespace::Unknown),
		};

		if let Some(name) = name.text() {
			index.map.insert(IndexEntry { name, ns }, ItemData { public });
		}
	}
	index
}
