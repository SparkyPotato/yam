use diagnostics::FileSpan;

use crate::token::{Token, TokenKind};

#[cfg(test)]
mod tests;
pub mod token;

pub struct Lexer<'s> {
	inner: logos::Lexer<'s, TokenKind>,
}

impl<'s> Lexer<'s> {
	pub fn new(source: &'s str) -> Self {
		Self {
			inner: logos::Lexer::new(source),
		}
	}

	pub fn source(&self) -> &'s str { self.inner.source() }
}

impl Lexer<'_> {
	pub fn next(&mut self) -> Token {
		let token = self.inner.next();
		let span = self.inner.span();
		match token {
			Some(token) => Token {
				kind: token,
				span: FileSpan {
					start: span.start as _,
					end: span.end as _,
					relative: (),
				},
			},
			None => Token {
				kind: T![eof],
				span: self.eof_span(),
			},
		}
	}

	pub fn eof_span(&self) -> FileSpan {
		FileSpan {
			start: self.source().len() as u32 - 1,
			end: self.source().len() as _,
			relative: (),
		}
	}
}
