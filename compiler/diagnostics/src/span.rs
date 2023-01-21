use std::{
	fmt::{Debug, Display},
	hash::{Hash, Hasher},
	ops::{Add, Index},
};

use text::Text;

use crate::{DiagKind, Diagnostic, Label};

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct File(Text);

impl Display for File {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { self.0.fmt(f) }
}

impl File {
	pub fn new(name: &str) -> Self { Self(Text::new(name)) }
}

#[derive(Debug, Default, Copy, Clone)]
pub struct Span<F> {
	pub start: u32,
	pub end: u32,
	pub relative: F,
}

impl<F> PartialEq for Span<F> {
	fn eq(&self, _: &Self) -> bool { true }
}

impl<F> Eq for Span<F> {}

impl<F> Hash for Span<F> {
	fn hash<H: Hasher>(&self, _: &mut H) {}
}

impl<F: Debug + PartialEq> Add for Span<F> {
	type Output = Self;

	fn add(self, other: Self) -> Self {
		debug_assert_eq!(self.relative, other.relative, "Cannot merge unrelated spans");

		Span {
			start: self.start.min(other.start),
			end: self.end.max(other.end),
			relative: self.relative,
		}
	}
}

impl<F> Index<Span<F>> for str {
	type Output = str;

	fn index(&self, span: Span<F>) -> &Self::Output { &self[span.start as usize..span.end as usize] }
}

impl<F: Clone + PartialEq> ariadne::Span for Span<F> {
	type SourceId = F;

	fn source(&self) -> &Self::SourceId { &self.relative }

	fn start(&self) -> usize { self.start as _ }

	fn end(&self) -> usize { self.end as _ }
}

impl<F> Span<F> {
	pub fn error(self, message: impl ToString) -> Diagnostic<F> { Diagnostic::new(DiagKind::Error, message, self) }

	pub fn warning(self, message: impl ToString) -> Diagnostic<F> { Diagnostic::new(DiagKind::Warning, message, self) }

	pub fn advice(self, message: impl ToString) -> Diagnostic<F> { Diagnostic::new(DiagKind::Advice, message, self) }

	pub fn label(self, message: impl ToString) -> Label<F> { Label::new(self, message) }

	pub fn mark(self) -> Label<F> { Label::no_message(self) }
}

pub type FullSpan = Span<File>;
pub type FileSpan = Span<()>;
