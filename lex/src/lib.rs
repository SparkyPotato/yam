use diag::Span;
use intern::Id;

use crate::token::{Token, TokenKind};

#[cfg(test)]
mod tests;
pub mod token;

pub struct Lexer<'s> {
	file_name: Id<str>,
	inner: logos::Lexer<'s, TokenKind>,
}

impl<'s> Lexer<'s> {
	pub fn new(file_name: Id<str>, source: &'s str) -> Self {
		Self {
			file_name,
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
				span: Span {
					start: span.start as _,
					end: span.end as _,
					file: self.file_name,
				},
			},
			None => Token {
				kind: T![eof],
				span: Span {
					start: self.inner.source().len() as _,
					end: self.inner.source().len() as u32 + 1,
					file: self.file_name,
				},
			},
		}
	}
}
