use std::ops::{Add, Index, Range};

use lasso::Spur;

use crate::{DiagKind, Diagnostic, Label};

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

impl Span {
	pub fn error(self, message: impl ToString) -> Diagnostic { Diagnostic::new(DiagKind::Error, message, self) }

	pub fn warning(self, message: impl ToString) -> Diagnostic { Diagnostic::new(DiagKind::Warning, message, self) }

	pub fn advice(self, message: impl ToString) -> Diagnostic { Diagnostic::new(DiagKind::Advice, message, self) }

	pub fn label(self, message: impl ToString) -> Label { Label::new(self, message) }

	pub fn mark(self) -> Label { Label::no_message(self) }
}
