use std::{
	collections::HashMap,
	fmt::{Debug, Display},
	ops::Range,
};

use ariadne::{Cache, Report, Source};
use intern::{Id, Resolver};

mod span;
pub use span::Span;
mod diag;
pub use crate::diag::*;

#[derive(Default)]
pub struct FileCacheBuilder {
	files: HashMap<Id<str>, Source>,
}

impl FileCacheBuilder {
	pub fn new() -> Self { Self::default() }

	pub fn set_file(&mut self, file: Id<str>, data: String) { self.files.insert(file, Source::from(data)); }

	pub fn cache<'a, T: Resolver<str>>(&'a self, intern: &'a T) -> FileCache<'a, T> {
		FileCache {
			files: &self.files,
			intern,
		}
	}
}

pub struct FileCache<'a, T> {
	files: &'a HashMap<Id<str>, Source>,
	intern: &'a T,
}

impl<T: Resolver<str>> Cache<Id<str>> for &FileCache<'_, T> {
	fn fetch(&mut self, id: &Id<str>) -> Result<&Source, Box<dyn Debug + '_>> { Ok(&self.files[id]) }

	fn display<'a>(&self, id: &'a Id<str>) -> Option<Box<dyn Display + 'a>> {
		Some(Box::new(String::from(self.intern.resolve(*id))))
	}
}

pub fn quick_diagnostic(kind: DiagKind, message: impl ToString) {
	Report::<Range<usize>>::build(kind.into_report_kind(), (), 0)
		.with_message(message)
		.finish()
		.eprint(Source::from(""))
		.unwrap();
}
