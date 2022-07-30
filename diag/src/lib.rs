use std::{
	collections::HashMap,
	fmt::{Debug, Display},
	ops::Range,
	path::Path,
};

use ariadne::{Cache, Report, Source};
use lasso::{Rodeo, Spur};

mod span;
pub use span::Span;
mod diag;
pub use diag::*;

#[derive(Default)]
pub struct FileCacheBuilder {
	files: HashMap<Spur, Source>,
}

impl FileCacheBuilder {
	pub fn new() -> Self { Self::default() }

	pub fn add_file(&mut self, rodeo: &mut Rodeo, path: &Path) -> Spur {
		rodeo.get_or_intern(path.as_os_str().to_str().unwrap())
	}

	pub fn set_file(&mut self, file: Spur, data: String) { self.files.insert(file, Source::from(data)); }

	pub fn finish(self, rodeo: &Rodeo) -> FileCache {
		FileCache {
			files: self.files,
			rodeo,
		}
	}
}

pub struct FileCache<'a> {
	files: HashMap<Spur, Source>,
	rodeo: &'a Rodeo,
}

impl Cache<Spur> for &FileCache<'_> {
	fn fetch(&mut self, id: &Spur) -> Result<&Source, Box<dyn Debug + '_>> { Ok(&self.files[id]) }

	fn display<'a>(&self, id: &'a Spur) -> Option<Box<dyn Display + 'a>> {
		Some(Box::new(String::from(self.rodeo.resolve(id))))
	}
}

pub fn quick_diagnostic(kind: DiagKind, message: impl ToString) {
	Report::<Range<usize>>::build(kind.to_report_kind(), (), 0)
		.with_message(message)
		.finish()
		.eprint(Source::from(""))
		.unwrap();
}
