use std::{
	fmt::{Debug, Display},
	hash::{Hash, Hasher},
	ops::{Add, Index},
	path::Path,
};

use text::Text;

use crate::{DiagKind, Diagnostic, Label};

pub trait Span: Sized {
	type Ctx;
	type Relative;

	fn to_raw(self, ctx: &Self::Ctx) -> RawSpan<Self::Relative>;

	fn error(self, message: impl ToString) -> Diagnostic<Self> { Diagnostic::new(DiagKind::Error, message, self) }

	fn warning(self, message: impl ToString) -> Diagnostic<Self> { Diagnostic::new(DiagKind::Warning, message, self) }

	fn advice(self, message: impl ToString) -> Diagnostic<Self> { Diagnostic::new(DiagKind::Advice, message, self) }

	fn label(self, message: impl ToString) -> Label<Self> { Label::new(self, message) }

	fn mark(self) -> Label<Self> { Label::no_message(self) }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct FilePath(Text);

impl Display for FilePath {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { self.0.fmt(f) }
}

impl FilePath {
	pub fn new(name: &str) -> Self { Self(Text::new(name)) }

	pub fn path(&self) -> &'static Path { Path::new(self.0.as_str()) }
}

#[derive(Debug, Default, Copy, Clone)]
pub struct RawSpan<F> {
	pub start: u32,
	pub end: u32,
	pub relative: F,
}

impl<F> PartialEq for RawSpan<F> {
	fn eq(&self, _: &Self) -> bool { true }
}

impl<F> Eq for RawSpan<F> {}

impl<F> Hash for RawSpan<F> {
	fn hash<H: Hasher>(&self, _: &mut H) {}
}

impl<F: Debug + PartialEq> Add for RawSpan<F> {
	type Output = Self;

	fn add(self, other: Self) -> Self {
		debug_assert_eq!(self.relative, other.relative, "Cannot merge unrelated spans");

		RawSpan {
			start: self.start.min(other.start),
			end: self.end.max(other.end),
			relative: self.relative,
		}
	}
}

impl<F> Index<RawSpan<F>> for str {
	type Output = str;

	fn index(&self, span: RawSpan<F>) -> &Self::Output { &self[span.start as usize..span.end as usize] }
}

impl<F: Clone + PartialEq> ariadne::Span for RawSpan<F> {
	type SourceId = F;

	fn source(&self) -> &Self::SourceId { &self.relative }

	fn start(&self) -> usize { self.start as _ }

	fn end(&self) -> usize { self.end as _ }
}

impl<F> Span for RawSpan<F> {
	type Ctx = ();
	type Relative = F;

	fn to_raw(self, _: &Self::Ctx) -> Self { self }
}

pub type FullSpan = RawSpan<FilePath>;
pub type FileSpan = RawSpan<()>;

impl FileSpan {
	pub fn with<T>(self, span: T) -> RawSpan<T> {
		RawSpan {
			start: self.start,
			end: self.end,
			relative: span,
		}
	}
}
