use std::{
	collections::HashMap,
	fmt::{Debug, Display},
};

use ariadne::{Cache, Source};

mod diag;
mod span;

pub use crate::{diag::*, span::*};

#[derive(Default)]
pub struct FileCache {
	files: HashMap<FilePath, Source>,
}

impl FileCache {
	pub fn new() -> Self { Self::default() }

	pub fn set_file(&mut self, file: FilePath, data: String) { self.files.insert(file, Source::from(data)); }
}

impl Cache<FilePath> for &FileCache {
	fn fetch(&mut self, id: &FilePath) -> Result<&Source, Box<dyn Debug + '_>> { Ok(&self.files[id]) }

	fn display<'a>(&self, id: &'a FilePath) -> Option<Box<dyn Display + 'a>> { Some(Box::new(id)) }
}

pub mod test {
	use crate::Diagnostic;

	pub fn emit_test<F: Clone + PartialEq>(source: &str, diags: impl Iterator<Item = Diagnostic<F>>) -> String {
		let mut s = String::new();
		for diag in diags {
			s += &diag.emit_test(source);
		}
		s
	}
}
