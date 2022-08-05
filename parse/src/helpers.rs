use diag::Span;
use lex::{token::TokenKind, T};
use syntax::kind::SyntaxKind;

use crate::Parser;

impl Parser<'_, '_, '_> {
	pub fn is_empty(&self) -> bool { matches!(self.api.peek().kind, T![eof]) }

	pub fn expect(&mut self, kind: TokenKind) -> Span {
		let token = self.api.peek();

		if token.kind != kind {
			let diag = token.span.error(format!("expected `{}`", kind));

			self.diags.push(if self.api.is_span_eof(token.span) {
				diag
			} else {
				diag.label(token.span.label(format!("found `{}`", token.kind)))
			});

			self.recover_with_tokentree()
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
				self.api.bump();
				break;
			}

			if !end_comma {
				self.diags.push(
					next.span
						.error(format!("expected `,` or `{}`", end))
						.label(next.span.label(format!("found `{}`", next.kind))),
				);

				let b = self.api.start_node(SyntaxKind::Err);
				loop {
					self.api.bump();
					let next = self.api.peek();
					if next.kind == end || next.kind == T![eof] {
						break;
					}
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

	pub fn recover_with_tokentree(&mut self) -> Span {
		let b = self.api.start_node(SyntaxKind::Err);

		let mut delim_stack = Vec::new();
		let ret = loop {
			let curr = self.api.peek();
			self.api.bump();
			match curr.kind {
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
				T![eof] => break curr.span,
				_ => {},
			}
		};

		self.api.finish_node(b);

		ret
	}
}

macro_rules! one_of {
	($self:ident { $($name:literal : $m:pat $(if $cond:expr)? => $value:expr,)* }) => {
		let tok = $self.api.peek();
		match tok.kind {
			$($m $(if $cond)? => $value,)*
			_ => {
				$self.diags.push(
					tok.span
						.error(format!("expected one of: {}", stringify! { $($name, )* }))
						.label(tok.span.label(format!("found `{}`", tok.kind))),
				);
				$self.recover_with_tokentree();
			},
		}
	};
}
pub(crate) use one_of;
