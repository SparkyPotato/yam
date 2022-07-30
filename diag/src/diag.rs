use ariadne::{Report, ReportKind};
use lasso::Spur;

use crate::{FileCache, Span};

pub enum DiagKind {
	Error,
	Warning,
	Advice,
}

impl DiagKind {
	pub(crate) fn to_report_kind(self) -> ReportKind {
		match self {
			DiagKind::Error => ReportKind::Error,
			DiagKind::Warning => ReportKind::Warning,
			DiagKind::Advice => ReportKind::Advice,
		}
	}
}

pub struct Label {
	span: Span,
	message: Option<String>,
}

impl Label {
	pub fn new(span: Span, message: impl ToString) -> Self {
		Self {
			span,
			message: Some(message.to_string()),
		}
	}

	pub fn no_message(span: Span) -> Self { Self { span, message: None } }
}

pub struct Diagnostic {
	kind: DiagKind,
	message: String,
	span: Span,
	labels: Vec<Label>,
}

impl Diagnostic {
	pub fn new(kind: DiagKind, message: impl ToString, span: Span) -> Self {
		Self {
			kind,
			message: message.to_string(),
			span,
			labels: Vec::new(),
		}
	}

	pub fn source(kind: DiagKind, message: impl ToString, source: Spur) -> Self {
		Self::new(
			kind,
			message,
			Span {
				start: 0,
				end: 0,
				file: source,
			},
		)
	}

	pub fn label(mut self, label: Label) -> Self {
		self.labels.push(label);
		self
	}
}

pub struct Diagnostics {
	inner: Vec<Diagnostic>,
	was_error: bool,
}

impl Diagnostics {
	pub fn new() -> Self {
		Self {
			inner: Vec::new(),
			was_error: false,
		}
	}

	pub fn push(&mut self, diagnostic: Diagnostic) {
		if matches!(diagnostic.kind, DiagKind::Error) {
			self.was_error = true;
		}

		self.inner.push(diagnostic);
	}

	pub fn was_error(&self) -> bool { self.was_error }

	pub fn emit(self, cache: &FileCache) {
		for diagnostic in self.inner {
			let mut builder = Report::build(
				diagnostic.kind.to_report_kind(),
				diagnostic.span.file,
				diagnostic.span.start as _,
			);
			builder.set_message(diagnostic.message);
			for label in diagnostic.labels {
				builder.add_label(if let Some(message) = label.message {
					ariadne::Label::new(label.span).with_message(message)
				} else {
					ariadne::Label::new(label.span)
				});
			}

			builder.finish().eprint(cache).expect("Failed to emit diagnostic");
		}
	}
}
