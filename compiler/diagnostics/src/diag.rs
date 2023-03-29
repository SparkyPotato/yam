use ariadne::{Report, ReportKind};
use verde::Pushable;

use crate::{FileCache, FilePath, Span};

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum DiagKind {
	Error,
	Warning,
	Advice,
}

impl DiagKind {
	pub(crate) fn into_report_kind(self) -> ReportKind<'static> {
		match self {
			DiagKind::Error => ReportKind::Error,
			DiagKind::Warning => ReportKind::Warning,
			DiagKind::Advice => ReportKind::Advice,
		}
	}
}

#[derive(Clone)]
pub struct Label<F> {
	pub span: Span<F>,
	pub message: Option<String>,
}

impl<F> Label<F> {
	pub fn new(span: Span<F>, message: impl ToString) -> Self {
		Self {
			span,
			message: Some(message.to_string()),
		}
	}

	pub fn no_message(span: Span<F>) -> Self { Self { span, message: None } }

	pub fn map_span<T>(self, f: impl FnOnce(Span<F>) -> Span<T>) -> Label<T> {
		Label {
			span: f(self.span),
			message: self.message,
		}
	}
}

#[derive(Pushable, Clone)]
pub struct Diagnostic<F> {
	pub kind: DiagKind,
	pub message: String,
	pub span: Span<F>,
	pub labels: Vec<Label<F>>,
}

impl<F> Diagnostic<F> {
	pub fn new(kind: DiagKind, message: impl ToString, span: Span<F>) -> Self {
		Self {
			kind,
			message: message.to_string(),
			span,
			labels: Vec::new(),
		}
	}

	pub fn label(mut self, label: Label<F>) -> Self {
		self.labels.push(label);
		self
	}

	pub fn map_span<T>(self, mut f: impl FnMut(Span<F>) -> Span<T>) -> Diagnostic<T> {
		Diagnostic {
			kind: self.kind,
			message: self.message,
			span: f(self.span),
			labels: self.labels.into_iter().map(|label| label.map_span(&mut f)).collect(),
		}
	}
}

impl Diagnostic<FilePath> {
	pub fn emit(&self, cache: &FileCache) {
		let mut builder = Report::build(self.kind.into_report_kind(), self.span.relative, self.span.start as _);
		builder.set_message(&self.message);
		for label in self.labels.iter() {
			builder.add_label(if let Some(message) = &label.message {
				ariadne::Label::new(label.span).with_message(message)
			} else {
				ariadne::Label::new(label.span)
			});
		}

		builder.finish().eprint(cache).expect("Failed to emit diagnostic");
	}
}

pub type FullDiagnostic = Diagnostic<FilePath>;
pub type FileDiagnostic = Diagnostic<()>;

pub mod test {
	use std::fmt::{Debug, Display};

	use ariadne::{CharSet, Config, Source};

	use super::*;

	impl<F: Clone + PartialEq> Diagnostic<F> {
		pub fn emit_test(self, source: &str) -> String {
			let cache = Cache {
				source: Source::from(source),
			};
			let mut s = Vec::new();

			let mut builder = Report::build(self.kind.into_report_kind(), self.span.relative, self.span.start as _)
				.with_config(Config::default().with_color(false).with_char_set(CharSet::Ascii));
			builder.set_message(self.message);
			for label in self.labels {
				builder.add_label(if let Some(message) = label.message {
					ariadne::Label::new(label.span).with_message(message)
				} else {
					ariadne::Label::new(label.span)
				});
			}

			builder
				.finish()
				.write(&cache, &mut s)
				.expect("Failed to emit diagnostic");

			String::from_utf8(s).expect("Failed to convert to string")
		}
	}

	struct Cache {
		source: Source,
	}

	impl<T: ?Sized> ariadne::Cache<T> for &Cache {
		fn fetch(&mut self, _: &T) -> Result<&Source, Box<dyn Debug + '_>> { Ok(&self.source) }

		fn display<'a>(&self, _: &'a T) -> Option<Box<dyn Display + 'a>> { None }
	}
}
