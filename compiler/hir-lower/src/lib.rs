use diagnostics::FilePath;
use syntax::ast::File;
use verde::{storage, Tracked};

pub mod index;

#[storage]
pub struct Storage(Module, index::Index, index::generate_index);

#[derive(Tracked)]
pub struct Module {
	#[id]
	pub path: FilePath,
	pub ast: File,
}

impl PartialEq for Module {
	fn eq(&self, _: &Self) -> bool { false }
}
impl Eq for Module {}
