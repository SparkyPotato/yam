use std::{
	hash::{Hash, Hasher},
	ops::{Add, Index},
};

use text::Text;

use crate::{DiagKind, Label, RawDiagnostic};

#[derive(Debug, Default, Copy, Clone)]
pub struct FullSpan {
	pub start: u32,
	pub end: u32,
	pub file: Text,
}

impl PartialEq for FullSpan {
	fn eq(&self, _: &Self) -> bool { true }
}

impl Eq for FullSpan {}

impl Hash for FullSpan {
	fn hash<H: Hasher>(&self, _: &mut H) {}
}

impl Add for FullSpan {
	type Output = FullSpan;

	fn add(self, other: FullSpan) -> FullSpan {
		debug_assert_eq!(self.file, other.file, "Cannot merge spans in different files");

		FullSpan {
			start: self.start.min(other.start),
			end: self.end.max(other.end),
			file: self.file,
		}
	}
}

impl Index<FullSpan> for str {
	type Output = str;

	fn index(&self, span: FullSpan) -> &Self::Output { &self[span.start as usize..span.end as usize] }
}

impl ariadne::Span for FullSpan {
	type SourceId = Text;

	fn source(&self) -> &Self::SourceId { &self.file }

	fn start(&self) -> usize { self.start as _ }

	fn end(&self) -> usize { self.end as _ }
}

impl FullSpan {
	pub fn error(self, message: impl ToString) -> RawDiagnostic { RawDiagnostic::new(DiagKind::Error, message, self) }

	pub fn warning(self, message: impl ToString) -> RawDiagnostic {
		RawDiagnostic::new(DiagKind::Warning, message, self)
	}

	pub fn advice(self, message: impl ToString) -> RawDiagnostic { RawDiagnostic::new(DiagKind::Advice, message, self) }

	pub fn label(self, message: impl ToString) -> Label { Label::new(self, message) }

	pub fn mark(self) -> Label { Label::no_message(self) }
}
