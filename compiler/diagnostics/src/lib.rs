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
	use crate::{Diagnostic, Span};

	pub fn emit_test<S>(source: &str, diags: impl IntoIterator<Item = Diagnostic<S>>, ctx: &S::Ctx) -> String
	where
		S: Span,
		S::Relative: Clone + PartialEq,
	{
		let mut s = String::new();
		for diag in diags {
			s += &diag.emit_test(source, ctx);
		}
		s
	}
}
