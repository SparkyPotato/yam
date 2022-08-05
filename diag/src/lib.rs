use std::{
	collections::HashMap,
	fmt::{Debug, Display},
	ops::Range,
	path::Path,
};

use ariadne::{Cache, Report, Source};
use intern::{Id, Interner, Resolver};

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

	pub fn add_file<T: Interner<str>>(&mut self, interner: &mut T, path: &Path) -> Id<str> {
		interner.intern(path.as_os_str().to_str().unwrap())
	}

	pub fn set_file(&mut self, file: Id<str>, data: String) { self.files.insert(file, Source::from(data)); }

	pub fn finish<T: Resolver<str>>(self, intern: &T) -> FileCache<T> {
		FileCache {
			files: self.files,
			intern,
		}
	}
}

pub struct FileCache<'a, T> {
	files: HashMap<Id<str>, Source>,
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
