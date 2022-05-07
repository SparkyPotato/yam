use std::{
	cell::RefCell,
	collections::HashMap,
	fmt::{Debug, Display},
	ops::{Add, Index, Range},
	path::Path,
};

pub use ariadne;
use ariadne::{Cache, Report, ReportKind, Source};
use lasso::{Rodeo, Spur};

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
	pub start: u32,
	pub end: u32,
	pub file: Spur,
}

impl Add for Span {
	type Output = Span;

	fn add(self, other: Span) -> Span {
		debug_assert_eq!(self.file, other.file, "Cannot merge spans in different files");

		Span {
			start: self.start.min(other.start),
			end: self.end.max(other.end),
			file: self.file,
		}
	}
}

impl Index<Span> for str {
	type Output = str;

	fn index(&self, span: Span) -> &Self::Output { &self[span.start as usize..span.end as usize] }
}

impl ariadne::Span for Span {
	type SourceId = Spur;

	fn source(&self) -> &Self::SourceId { &self.file }

	fn start(&self) -> usize { self.start as _ }

	fn end(&self) -> usize { self.end as _ }
}

impl chumsky::Span for Span {
	type Context = Spur;
	type Offset = u32;

	fn new(file: Self::Context, range: Range<Self::Offset>) -> Self {
		Span {
			start: range.start,
			end: range.end,
			file,
		}
	}

	fn context(&self) -> Self::Context { self.file }

	fn start(&self) -> Self::Offset { self.start }

	fn end(&self) -> Self::Offset { self.end }
}

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
