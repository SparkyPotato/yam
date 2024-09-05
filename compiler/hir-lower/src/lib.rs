#![feature(drain_keep_rest)]
#![feature(let_chains)]
#![feature(try_blocks)]

use std::path::Path;

use diagnostics::{Diagnostic, FilePath, FullDiagnostic};
use hir::ident::{AbsPath, PackageId};
use rustc_hash::FxHashMap;
use syntax::ast;
use text::Text;
use verde::{storage, Ctx, Db, Id, Tracked};

use crate::index::ErasedTempId;

pub mod index;
pub mod lower;
pub mod prelude;
mod resolve;

pub type TempDiagnostic = Diagnostic<ErasedTempId>;

#[storage]
pub struct Storage(
	FullDiagnostic,
	TempDiagnostic,
	Module,
	Packages,
	VisiblePackages,
	prelude::PackagePrelude,
	prelude::Prelude,
	prelude::get_prelude,
	prelude::get_prelude_from_package,
	index::Index,
	index::PublicIndex,
	index::ModuleTree,
	index::PackageTree,
	index::generate_index,
	index::build_package_tree,
	lower::LoweredModule,
	lower::lower_to_hir,
);

// Note on deriving `Eq` and `PartialEq`:
// This does a pointer comparison, which is surprisingly what we want.
// On every reparse, this pointer will change and we want to invalidate the index as well the lowered HIR for
// this module. However, if there wasn't a reparse, we want to keep the old index and HIR if possible - and the
// pointer wouldn't have changed.
#[derive(Tracked, Eq, PartialEq)]
pub struct Module {
	#[id]
	pub path: Id<AbsPath>,
	pub file: FilePath,
	pub ast: ast::File,
}

impl Module {
	pub fn new(ast: ast::File, file: FilePath, path: Id<AbsPath>) -> Self { Self { path, file, ast } }

	/// Figure out the module's path from its relative file path from a root file.
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

				prec = db.add(AbsPath::Name { prec, name });
			}
		}

		Self::new(ast, file, prec)
	}
}

/// The packages visible to a package.
#[derive(Tracked, Eq, PartialEq)]
pub struct VisiblePackages {
	#[id]
	pub package: PackageId,
	pub packages: FxHashMap<Text, PackageId>,
}

#[derive(Tracked, Eq, PartialEq)]
pub struct Packages {
	#[id]
	pub id: (),
	pub packages: FxHashMap<PackageId, Id<VisiblePackages>>,
}

fn is_child_of(ctx: &Ctx, parent: Id<AbsPath>, mut child: Id<AbsPath>) -> bool {
	loop {
		if parent == child {
			return true;
		}

		child = match *ctx.geti(child) {
			AbsPath::Package(_) => return false,
			AbsPath::Name { prec, .. } => prec,
		};
	}
}
