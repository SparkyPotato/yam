use diagnostics::FileSpan;
use lex::{token::TokenKind, T};
use syntax::SyntaxKind;

use crate::parse::Parser;

impl Parser<'_, '_> {
	pub fn is_empty(&self) -> bool { matches!(self.api.peek().kind, T![eof]) }

	pub fn expect(&mut self, kind: TokenKind, next: &[TokenKind]) -> FileSpan {
		let token = self.api.peek();

		if token.kind != kind {
			let diag = token.span.error(format!("expected `{}`", SyntaxKind::from(kind)));

			self.diags.push(if self.api.is_span_eof(token.span) {
				diag
			} else {
				diag.label(token.span.label(format!("found `{}`", SyntaxKind::from(token.kind))))
			});

			self.try_recover(kind, next)
		} else {
			self.api.bump();

			token.span
		}
	}

	pub fn comma_sep_list(&mut self, end: TokenKind, mut f: impl FnMut(&mut Self)) {
		let mut end_comma = true;
		loop {
			let next = self.api.peek();
			if next.kind == end {
				break;
			}

			if !end_comma {
				self.diags.push(
					next.span
						.error(format!("expected `,` or `{}`", SyntaxKind::from(end)))
						.label(next.span.label(format!("found `{}`", SyntaxKind::from(next.kind)))),
				);

				let b = self.api.start_node(SyntaxKind::Error);
				loop {
					let next = self.api.peek();
					if next.kind == end || next.kind == T![eof] {
						break;
					}
					self.api.bump();
				}
				self.api.finish_node(b);
				self.api.bump();

				return;
			}

			f(self);

			let next = self.api.peek();
			if next.kind == T![,] {
				self.api.bump();
				end_comma = true;
			} else {
				end_comma = false;
			}
		}
	}

	pub fn try_recover(&mut self, want: TokenKind, next: &[TokenKind]) -> FileSpan {
		let b = self.api.start_node(SyntaxKind::Error);

		enum Delim {
			Paren,
			Brace,
			Bracket,
		}
		let mut delim_stack = Vec::new();
		let ret = 'o: loop {
			let curr = self.api.peek();
			match curr.kind {
				x if x == want => {
					self.api.bump();
					break curr.span;
				},
				x if delim_stack.is_empty() && next.contains(&x) => break curr.span,
				T!['('] => {
					self.api.bump();
					delim_stack.push(Delim::Paren);
				},
				T!['{'] => {
					self.api.bump();
					delim_stack.push(Delim::Brace);
				},
				T!['['] => {
					self.api.bump();
					delim_stack.push(Delim::Bracket);
				},
				T![')'] => {
					let mut c = delim_stack.pop();
					self.api.bump();
					loop {
						match c {
							Some(Delim::Paren) => break 'o curr.span,
							Some(_) => {
								c = delim_stack.pop();
							},
							None => break 'o curr.span,
						}
					}
				},
				T!['}'] => {
					let mut c = delim_stack.pop();
					self.api.bump();
					loop {
						match c {
							Some(Delim::Brace) => break 'o curr.span,
							Some(_) => {
								c = delim_stack.pop();
							},
							None => break 'o curr.span,
						}
					}
				},
				T![']'] => {
					let mut c = delim_stack.pop();
					self.api.bump();
					loop {
						match c {
							Some(Delim::Bracket) => break 'o curr.span,
							Some(_) => {
								c = delim_stack.pop();
							},
							None => break 'o curr.span,
						}
					}
				},
				T![;] | T![,] => {
					self.api.bump();
					break curr.span;
				},
				T![eof] => break curr.span,
				_ => self.api.bump(),
			}
		};

		self.api.finish_node(b);

		ret
	}

	pub fn fmt_kinds(kinds: &[TokenKind]) -> String {
		let mut s = String::new();
		for (i, kind) in kinds.iter().copied().enumerate() {
			if i != 0 {
				s.push_str(", ");
			}
			s.push('`');
			s.push_str(&SyntaxKind::from(kind).to_string());
			s.push('`');
		}
		s
	}
}

macro_rules! select {
	($self:ident { $(T!$kind:tt => $value:expr,)* }) => {
		let tok = $self.api.peek();
		let expected = [$(T!$kind,)*];
		match tok.kind {
			$(T!$kind => $value,)*
			_ => {
				$self.diags.push(
					tok.span
						.error(format!("expected one of: {}", Self::fmt_kinds(&expected)))
						.label(tok.span.label(format!("found `{}`", SyntaxKind::from(tok.kind)))),
				);
				$self.try_recover(T![eof], &expected);
			},
		}
	};
}
pub(crate) use select;
