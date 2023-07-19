use std::{
	fmt::{Debug, Display},
	ops::Range,
};

use ariadne::{Cache, Report, Source};
use rustc_hash::FxHashMap;

mod diag;
mod span;

pub use crate::{diag::*, span::*};

/// A cache storing the contents of files.
#[derive(Default)]
pub struct FileCache {
	files: FxHashMap<FilePath, Source>,
}

impl FileCache {
	pub fn new() -> Self { Self::default() }

	pub fn set_file(&mut self, file: FilePath, data: String) { self.files.insert(file, Source::from(data)); }

	pub fn set_files(&mut self, files: impl IntoIterator<Item = (FilePath, String)>) {
		self.files.extend(files.into_iter().map(|(x, y)| (x, Source::from(y))));
	}
}

impl Cache<FilePath> for &FileCache {
	fn fetch(&mut self, id: &FilePath) -> Result<&Source, Box<dyn Debug + '_>> {
		Ok(self.files.get(id).expect("Invalid file"))
	}

	fn display<'a>(&self, id: &'a FilePath) -> Option<Box<dyn Display + 'a>> { Some(Box::new(id)) }
}

/// Emit diagnostics with a cache and span resolution context.
pub fn emit<S: Span>(diags: impl IntoIterator<Item = Diagnostic<S>>, cache: &FileCache, ctx: &S::Ctx) {
	for diag in diags {
		diag.emit(cache, ctx);
	}
}

/// Emit a quick diagnostic with no source.
pub fn quick_diagnostic(kind: DiagKind, message: impl ToString) {
	Report::<Range<usize>>::build(kind.into_report_kind(), (), 0)
		.with_message(message)
		.finish()
		.eprint(Source::from(""))
		.unwrap();
}

pub mod test {
	use crate::{Diagnostic, Span};

	pub fn emit_test<S>(source: &str, diags: impl IntoIterator<Item = Diagnostic<S>>, ctx: &S::Ctx) -> String
	where
		S: Span,
	{
		let mut s = String::new();
		for diag in diags {
			s += &diag.emit_test(source, ctx);
		}
		s
	}
}
