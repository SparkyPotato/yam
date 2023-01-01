use ariadne::{Report, ReportKind};
use text::Text;

use crate::{FileCache, FullSpan};

#[derive(Copy, Clone)]
pub enum DiagKind {
	Error,
	Warning,
	Advice,
}

impl DiagKind {
	pub(crate) fn into_report_kind(self) -> ReportKind {
		match self {
			DiagKind::Error => ReportKind::Error,
			DiagKind::Warning => ReportKind::Warning,
			DiagKind::Advice => ReportKind::Advice,
		}
	}
}

pub struct Label {
	span: FullSpan,
	message: Option<String>,
}

impl Label {
	pub fn new(span: FullSpan, message: impl ToString) -> Self {
		Self {
			span,
			message: Some(message.to_string()),
		}
	}

	pub fn no_message(span: FullSpan) -> Self { Self { span, message: None } }
}

pub struct RawDiagnostic {
	kind: DiagKind,
	message: String,
	span: FullSpan,
	labels: Vec<Label>,
}

impl RawDiagnostic {
	pub fn new(kind: DiagKind, message: impl ToString, span: FullSpan) -> Self {
		Self {
			kind,
			message: message.to_string(),
			span,
			labels: Vec::new(),
		}
	}

	pub fn source(kind: DiagKind, message: impl ToString, file: Text) -> Self {
		Self::new(kind, message, FullSpan { start: 0, end: 0, file })
	}

	pub fn label(mut self, label: Label) -> Self {
		self.labels.push(label);
		self
	}

	pub fn emit(&self, cache: &FileCache) {
		let mut builder = Report::build(self.kind.into_report_kind(), self.span.file, self.span.start as _);
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

pub mod test {
	use std::fmt::{Debug, Display};

	use ariadne::{CharSet, Config, Source};
	use text::Text;

	use super::*;

	impl RawDiagnostic {
		pub fn emit_test(self, source: &str) -> String {
			let cache = Cache {
				source: Source::from(source),
			};
			let mut s = Vec::new();

			let mut builder = Report::build(self.kind.into_report_kind(), self.span.file, self.span.start as _)
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

	impl ariadne::Cache<Text> for &Cache {
		fn fetch(&mut self, _: &Text) -> Result<&Source, Box<dyn Debug + '_>> { Ok(&self.source) }

		fn display<'a>(&self, _: &'a Text) -> Option<Box<dyn Display + 'a>> { None }
	}
}
