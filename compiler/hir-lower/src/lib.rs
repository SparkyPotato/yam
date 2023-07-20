use std::path::Path;

use diagnostics::{FilePath, FullDiagnostic};
use hir::ident::{AbsPath, PackageId};
use syntax::ast;
use text::Text;
use verde::{storage, Db, Id, Tracked};

pub mod index;
pub mod lower;

// TODO: prelude.

#[storage]
pub struct Storage(
	FullDiagnostic,
	Module,
	index::local::Index,
	index::local::InnerIndex,
	index::local::ModuleTree,
	index::local::PackageTree,
	index::local::generate_index,
	index::local::build_package_tree,
	lower::LoweredModule,
	lower::VisibilePackages,
	lower::lower_to_hir,
);

#[derive(Tracked)]
pub struct Module {
	#[id]
	pub path: Id<AbsPath>,
	pub file: FilePath,
	pub ast: ast::File,
}

impl Eq for Module {}
impl PartialEq for Module {
	fn eq(&self, other: &Self) -> bool {
		// This does a pointer comparison, which is surprsingly what we want.
		// On every reparse, this pointer will change and we want to invalidate the index as well the lowered HIR for
		// this module. However, if there wasn't a reparse, we want to keep the old index and HIR if possible - and the
		// pointer wouldn't have changed.
		self.path == other.path && self.ast == other.ast
	}
}

impl Module {
	pub fn new(ast: ast::File, file: FilePath, path: Id<AbsPath>) -> Self { Self { path, file, ast } }

	/// Figure out the module's path from it's relative file path from a root file.
	///
	/// `prefix` is a prefix to add to path - possibly just the package ID if we're looking at the root module.
	pub fn from_file(db: &dyn Db, root: FilePath, ast: ast::File, file: FilePath, package: PackageId) -> Self {
		if root == file {
			return Self::new(ast, file, db.add(package.into()));
		}

		let root = root.path().parent().expect("root must be a file");
		// Ensure path is something like `src/x/x.yam`.
		let p = file.path();
		assert_eq!(p.extension(), Some("yam".as_ref()), "Path is not a .yam file");
		let relative = p.strip_prefix(root).expect("Path is not a child of the root");
		// `relative` is now `x/x.yam`.

		let mut last_name = String::new();
		let mut prec = db.add(package.into());
		for component in relative.components() {
			let path: &Path = component.as_ref(); // `x` or `x.yam`.
			let path = path.to_str().expect("Path is not valid UTF-8");

			let file_name = path.strip_suffix(".yam");
			if file_name != Some(last_name.as_str()) {
				// Create a child module if we aren't `x/x.yam`.
				let path = file_name.unwrap_or(path);
				last_name = path.to_string();
				let name = Text::new(path);

				prec = db.add(AbsPath::Module { prec, name });
			}
		}

		Self::new(ast, file, prec)
	}
}
