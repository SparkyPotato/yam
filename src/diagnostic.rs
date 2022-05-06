use std::{
	cell::RefCell,
	collections::HashMap,
	fmt::{Debug, Display},
	ops::Range,
	path::Path,
};

use ariadne::{Cache, Report, ReportKind, Source};
use lasso::{Rodeo, Spur};

use crate::parse::ast::Span;

pub struct FileCache<'a> {
	rodeo: &'a RefCell<Rodeo>,
	files: HashMap<Spur, Source>,
}

impl<'a> FileCache<'a> {
	pub fn new(rodeo: &'a RefCell<Rodeo>) -> Self {
		Self {
			rodeo,
			files: HashMap::new(),
		}
	}

	pub fn add_file(&mut self, path: &Path) -> Spur {
		self.rodeo
			.borrow_mut()
			.get_or_intern(path.as_os_str().to_str().unwrap())
	}

	pub fn set_file(&mut self, file: Spur, data: String) { self.files.insert(file, Source::from(data)); }
}

impl Cache<Spur> for &FileCache<'_> {
	fn fetch(&mut self, id: &Spur) -> Result<&Source, Box<dyn Debug + '_>> { Ok(&self.files[id]) }

	fn display<'a>(&self, id: &'a Spur) -> Option<Box<dyn Display + 'a>> {
		Some(Box::new(String::from(self.rodeo.borrow().resolve(id))))
	}
}

pub fn emit_diagnostics(cache: &FileCache, diagnostics: Vec<Report<Span>>) {
	for diagnostic in diagnostics {
		diagnostic.eprint(cache).unwrap();
	}
}

pub fn quick_diagnostic(kind: ReportKind, message: impl ToString) {
	Report::<Range<usize>>::build(kind, (), 0)
		.with_message(message)
		.finish()
		.eprint(Source::from(""))
		.unwrap();
}
