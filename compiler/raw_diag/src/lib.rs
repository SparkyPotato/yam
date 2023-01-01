use std::{
	collections::HashMap,
	fmt::{Debug, Display},
	ops::Range,
};

use ariadne::{Cache, Report, Source};

mod span;
pub use span::FullSpan;
use text::Text;

mod diag;
pub use crate::diag::*;

#[derive(Default)]
pub struct FileCache {
	files: HashMap<Text, Source>,
}

impl FileCache {
	pub fn new() -> Self { Self::default() }

	pub fn set_file(&mut self, file: Text, data: String) { self.files.insert(file, Source::from(data)); }
}

impl Cache<Text> for &FileCache {
	fn fetch(&mut self, id: &Text) -> Result<&Source, Box<dyn Debug + '_>> { Ok(&self.files[id]) }

	fn display<'a>(&self, id: &'a Text) -> Option<Box<dyn Display + 'a>> { Some(Box::new(id.as_str())) }
}

pub fn quick_diagnostic(kind: DiagKind, message: impl ToString) {
	Report::<Range<usize>>::build(kind.into_report_kind(), (), 0)
		.with_message(message)
		.finish()
		.eprint(Source::from(""))
		.unwrap();
}
