use diag::Span;
use lex::{token::TokenKind, T};
use syntax::kind::SyntaxKind;

use crate::Parser;

impl Parser<'_, '_, '_> {
	pub fn is_empty(&self) -> bool { matches!(self.api.peek().kind, T![eof]) }

	pub fn expect(&mut self, kind: TokenKind, next: &[TokenKind]) -> Span {
		let token = self.api.peek();

		if token.kind != kind {
			if !self.silent {
				let diag = token.span.error(format!("expected `{}`", kind));

				self.diags.push(if self.api.is_span_eof(token.span) {
					diag
				} else {
					diag.label(token.span.label(format!("found `{}`", token.kind)))
				});
			}

			self.try_recover(next)
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
				if !self.silent {
					self.diags.push(
						next.span
							.error(format!("expected `,` or `{}`", end))
							.label(next.span.label(format!("found `{}`", next.kind))),
					);
				}

				let b = self.api.start_node(SyntaxKind::Err);
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

	pub fn try_recover(&mut self, next: &[TokenKind]) -> Span {
		let b = self.api.start_node(SyntaxKind::Err);

		self.silent = true;

		let mut delim_stack = Vec::new();
		let ret = loop {
			let curr = self.api.peek();
			self.api.bump();
			match curr.kind {
				x if x.is_delim_kw() || (delim_stack.is_empty() && next.contains(&x)) => break curr.span,
				T![ldelim: delim] => delim_stack.push(delim),
				T![rdelim: delim] => match delim_stack.last() {
					Some(_) => {
						let mut curr = delim_stack.pop();
						while curr != Some(delim) {
							curr = delim_stack.pop();
						}
					},
					None => break curr.span,
				},
				T![;] => break curr.span,
				T![,] => break curr.span,
				T![eof] => break curr.span,
				_ => {},
			}
		};

		self.api.finish_node(b);

		ret
	}

	pub fn recover_at_kw(&mut self) {
		let b = self.api.start_node(SyntaxKind::Err);

		while !self.api.peek().kind.is_delim_kw() {
			self.api.bump();
		}

		self.silent = false;

		self.api.finish_node(b);
	}

	pub fn recover_until(&mut self, end: TokenKind) {
		let b = self.api.start_node(SyntaxKind::Err);

		while self.api.peek().kind != end {
			self.api.bump();
		}

		self.silent = false;

		self.api.finish_node(b);
	}
}

macro_rules! select {
	($self:ident { $(T!$kind:tt => $value:expr,)* }) => {
		let tok = $self.api.peek();
		match tok.kind {
			$(T!$kind => $value,)*
			_ => {
				$self.diags.push(
					tok.span
						.error(format!("expected one of: {}", stringify! { $($kind, )* }))
						.label(tok.span.label(format!("found `{}`", tok.kind))),
				);
				$self.try_recover(&[$(T!$kind,)*]);
			},
		}
	};
}
pub(crate) use select;
