use std::cell::RefCell;

use chumsky::{prelude::*, Parser as CParser, Stream};
use lasso::Rodeo;

use crate::parse::{
	ast::{Arg, Module, Block, Fn, Ident, Item, ItemKind, Pat, PatKind, Span, Stmt, StmtKind, Visibility, WhereClause},
	token::{Delim, Token, TokenKind},
};

pub mod ast;
mod token;

pub struct ParseContext {
	rodeo: RefCell<Rodeo>,
	parser: Box<dyn CParser<TokenKind, Module>>,
}

impl ParseContext {
	pub fn
}

struct Lexer<'a> {
	lexer: logos::Lexer<'a, TokenKind>,
}

impl Iterator for Lexer<'_> {
	type Item = Token;

	fn next(&mut self) -> Option<Self::Item> {
		self.lexer.next().map(|kind| Token {
			node: kind,
			span: self.lexer.span().into(),
		})
	}
}

struct Parser<'a> {
	lexer: Lexer<'a>,
	intern: &'a RefCell<Rodeo>,
}

impl<'a> Parser<'a> {
	fn new(input: &'a str, intern: &'a RefCell<Rodeo>) -> Self {
		Self {
			lexer: Lexer {
				lexer: logos::Lexer::new(input),
			},
			intern,
		}
	}

	fn parse(mut self) -> (Option<Module>, Vec<Simple<TokenKind, Span>>) {
		let spur = just(TokenKind::Ident)
			.map_with_span(|_, span| self.intern.borrow_mut().get_or_intern(&self.lexer.lexer.slice()[span]));
		let ident = spur.clone().map_with_span(|spur, span| Ident { node: spur, span });

		let pat = ident
			.clone()
			.map(|i| PatKind::Binding(i.node))
			.or(just(TokenKind::Underscore).map(|_| PatKind::Ignore))
			.map_with_span(|node, span| Pat { node, span });

		let expr = todo!();
		let mut item = Recursive::declare();

		let stmt = item
			.clone()
			.map(|item: Item| StmtKind::Item(item.visibility, item.kind))
			.or(expr.clone().map(|expr| StmtKind::Expr(expr.node)))
			.or(expr
				.clone()
				.then_ignore(just(TokenKind::Semi))
				.map(|expr| StmtKind::Semi(expr.node)))
			.map_with_span(|node, span| Stmt { node, span });
		let block = stmt
			.repeated()
			.delimited_by(
				just(TokenKind::LDelim(Delim::Brace)),
				just(TokenKind::RDelim(Delim::Brace)),
			)
			.map_with_span(|node, span| Block { node, span });

		let arg = pat
			.clone()
			.then(just(TokenKind::Colon).ignore_then(expr.clone()).or_not())
			.map_with_span(|(pat, bounds), span| Arg { pat, bounds, span });
		let generics = arg
			.clone()
			.separated_by(just(TokenKind::Comma))
			.allow_trailing()
			.delimited_by(just(TokenKind::Le), just(TokenKind::Ge))
			.or_not()
			.map(|params| params.unwrap_or(Vec::new()));
		let args = arg.separated_by(just(TokenKind::Comma)).allow_trailing().delimited_by(
			just(TokenKind::LDelim(Delim::Paren)),
			just(TokenKind::RDelim(Delim::Paren)),
		);

		let where_clause = just(TokenKind::Where)
			.ignore_then(
				expr.clone()
					.then_ignore(just(TokenKind::Colon))
					.then(expr.clone())
					.separated_by(just(TokenKind::Comma))
					.allow_trailing()
					.map_with_span(|(on, bounds), span| WhereClause { on, bounds, span }),
			)
			.or_not()
			.map(|w| w.unwrap_or(Vec::new()));

		let function_item = just(TokenKind::Fn)
			.ignore_then(ident.clone())
			.then(generics.clone())
			.then(args.clone())
			.then(just(TokenKind::Arrow).ignore_then(expr.clone()).or_not())
			.then(where_clause.clone())
			.then(block.clone())
			.map(|(((((ident, generics), args), ret), where_clause), block)| Fn {
				ident,
				generics,
				args,
				ret,
				where_clause,
				block,
			});

		item.define(
			just(TokenKind::Pub)
				.or_not()
				.then(function_item)
				.map_with_span(|(vis, f), span| Item {
					kind: ItemKind::Fn(f),
					visibility: if vis.is_some() {
						Visibility::Public
					} else {
						Visibility::Private
					},
					span,
				}),
		);

		let parser = item.repeated().map(|items| Module { items });

		parser.parse_recovery(Stream::from_iter(
			Span {
				start: self.lexer.lexer.slice().len() as _,
				end: self.lexer.lexer.slice().len() as _ + 1,
			},
			std::iter::from_fn(|| self.lexer.next().map(|Token { node: kind, span }| (kind, span))),
		))
	}
}
