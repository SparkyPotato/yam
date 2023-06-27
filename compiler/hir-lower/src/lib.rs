use std::{marker::PhantomData, path::Path};

use diagnostics::FilePath;
use hir::ast::{AstId, ErasedAstId, ItemData};
use rustc_hash::FxHashMap;
use syntax::{ast, AstElement};
use text::Text;
use verde::{storage, Db, Id, Tracked};

pub mod index;

// TODO: Packages and @autoimport.

#[storage]
pub struct Storage(index::PublicIndex, index::PrivateIndex);

/// A declaration that is bound to a name.
#[derive(Debug, Clone, Eq, PartialEq)]
enum Declaration<T> {
	Item(T),
	Import { path: Id<hir::Path>, name: ast::Name },
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

#[derive(Debug)]
pub struct ModuleMap {
	module: Option<Id<hir::Path>>,
	file: FilePath,
	items: FxHashMap<Text, Declaration<ItemData>>,
}

impl ModuleMap {
	pub fn new(module: Option<Id<hir::Path>>, file: FilePath) -> Self {
		Self {
			module,
			file,
			items: FxHashMap::default(),
		}
	}

	fn declare(&mut self, db: &dyn Db, name: Text, item: Declaration<ast::Item>) -> Option<Declaration<ast::Item>> {
		let path = db.add(hir::Path {
			prec: self.module,
			ident: name,
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

struct ItemBuilder<'a> {
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
			PhantomData,
		)
	}
}

#[derive(Clone, Tracked)]
pub struct Module {
	#[id]
	pub path: Option<Id<hir::Path>>,
	pub file: FilePath,
	pub ast: ast::File,
}

// Always compare to false since we want to regenerate indices and HIR anyway.
impl PartialEq for Module {
	fn eq(&self, _: &Self) -> bool { false }
}
impl Eq for Module {}

impl Module {
	pub fn new(ast: ast::File, file: FilePath, path: Option<Id<hir::Path>>) -> Self { Self { path, file, ast } }

	/// Figure out the module's path from it's relative file path from a root file.
	///
	/// `prefix` is a prefix to add to path, for example, the name of the package.
	pub fn from_file(
		db: &dyn Db, root: FilePath, ast: ast::File, file: FilePath, prefix: Option<Id<hir::Path>>,
	) -> Self {
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
		let mut prec = prefix;
		for component in relative.components() {
			let path: &Path = component.as_ref(); // `x` or `x.yam`.
			let path = path.to_str().expect("Path is not valid UTF-8");

			let file_name = path.strip_suffix(".yam");
			if file_name != Some(last_name.as_str()) {
				// Create a child module if we aren't `x/x.yam`.
				let path = file_name.unwrap_or(path);
				last_name = path.to_string();
				let ident = Text::new(path);

				let path = db.add(hir::Path { prec, ident });
				prec = Some(path)
			}
		}

		Self::new(ast, file, prec)
	}
}
