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
	lower::VisibilityMap,
	lower::lower_to_hir,
	module::Module,
);
