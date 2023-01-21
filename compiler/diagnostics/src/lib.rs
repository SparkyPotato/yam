use std::{
	collections::HashMap,
	fmt::{Debug, Display},
	ops::Range,
};

use ariadne::{Cache, Report, Source};

mod diag;
mod span;

pub use crate::{diag::*, span::*};

#[derive(Default)]
pub struct FileCache {
	files: HashMap<File, Source>,
}

impl FileCache {
	pub fn new() -> Self { Self::default() }

	pub fn set_file(&mut self, file: File, data: String) { self.files.insert(file, Source::from(data)); }
}

impl Cache<File> for &FileCache {
	fn fetch(&mut self, id: &File) -> Result<&Source, Box<dyn Debug + '_>> { Ok(&self.files[id]) }

	fn display<'a>(&self, id: &'a File) -> Option<Box<dyn Display + 'a>> { Some(Box::new(id)) }
}

pub struct DiagSink<F> {
	inner: Vec<Diagnostic<F>>,
	had_error: bool,
}

impl<F> DiagSink<F> {
	pub fn new() -> Self {
		Self {
			inner: Vec::new(),
			had_error: false,
		}
	}

	pub fn push(&mut self, diag: Diagnostic<F>) {
		if diag.kind == DiagKind::Error {
			self.had_error = true;
		}
		self.inner.push(diag);
	}

	pub fn had_error(&self) -> bool { self.had_error }

	pub fn into_inner(self) -> Vec<Diagnostic<F>> { self.inner }
}

pub fn quick_diagnostic(kind: DiagKind, message: impl ToString) {
	Report::<Range<usize>>::build(kind.into_report_kind(), (), 0)
		.with_message(message)
		.finish()
		.eprint(Source::from(""))
		.unwrap();
}
