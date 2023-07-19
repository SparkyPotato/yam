use diagnostics::FullDiagnostic;
use verde::storage;

pub mod index;
pub mod lower;
pub mod module;

// TODO: prelude.

#[storage]
pub struct Storage(
	index::Index,
	index::InnerIndex,
	index::generate_index,
	lower::LoweredModule,
	lower::VisibilePackages,
	lower::lower_to_hir,
	module::Module,
	module::ModuleTree,
	module::PackageTree,
	module::build_package_tree,
	FullDiagnostic,
);
