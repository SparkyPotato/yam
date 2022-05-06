use std::cell::RefCell;

use ariadne::{Label, Report, ReportKind};
use chumsky::{error::SimpleReason, prelude::*, Parser as CParser, Stream};
use lasso::{Rodeo, Spur};

use crate::parse::{
	ast::{
		Arg,
		Block,
		Expr,
		ExprKind,
		Fn,
		Ident,
		Item,
		ItemKind,
		Lit,
		LitKind,
		Module,
		Pat,
		PatKind,
		Span,
		Stmt,
		StmtKind,
		Visibility,
		WhereClause,
	},
	token::{Delim, TokenKind},
};

pub mod ast;
mod token;

pub fn parse(file: Spur, input: &str, rodeo: &RefCell<Rodeo>, diagnostics: &mut Vec<Report<Span>>) -> Module {
	let lexer = Lexer {
		file,
		lexer: logos::Lexer::new(input),
	};

	let (module, errors) = Parser::new(input, rodeo).parse().parse_recovery(Stream::from_iter(
		Span {
			start: input.len() as _,
			end: input.len() as _,
			file,
		},
		lexer,
	));

	for error in errors {
		let mut builder = Report::build(ReportKind::Error, file, 0);

		let mut label = Label::new(error.span());
		match error.reason() {
			SimpleReason::Custom(s) => builder.set_message(s),
			SimpleReason::Unexpected => {
				builder.set_message(match error.found() {
					Some(tok) => format!("unexpected `{}`", tok),
					None => "unexpected `<eof>`".into(),
				});

				match error.expected().len() {
					0 => {},
					1 => {
						label = label.with_message(match error.expected().next().unwrap() {
							Some(tok) => format!("expected `{}`", tok),
							None => "expected `<eof>`".into(),
						})
					},
					_ => {
						label = label.with_message(format!(
							"expected one of {}",
							error
								.expected()
								.map(|tok| match tok {
									Some(tok) => format!("`{}`", tok),
									None => "`<eof>`".into(),
								})
								.collect::<Vec<_>>()
								.join(", ")
						))
					},
				}
			},
			SimpleReason::Unclosed { delimiter, .. } => {
				builder.set_message(format!("unclosed `{}`", delimiter));
			},
		}

		builder.add_label(label);

		diagnostics.push(builder.finish());
	}

	module.unwrap_or_else(|| Module { items: Vec::new() })
}

struct Lexer<'a> {
	file: Spur,
	lexer: logos::Lexer<'a, TokenKind>,
}

impl Iterator for Lexer<'_> {
	type Item = (TokenKind, Span);

	fn next(&mut self) -> Option<Self::Item> {
		self.lexer.next().map(|kind| {
			(kind, {
				let span = self.lexer.span();
				Span {
					start: span.start as _,
					end: span.end as _,
					file: self.file,
				}
			})
		})
	}
}

struct Parser<'a> {
	input: &'a str,
	rodeo: &'a RefCell<Rodeo>,
}

impl<'a> Parser<'a> {
	fn new(input: &'a str, rodeo: &'a RefCell<Rodeo>) -> Self { Self { input, rodeo } }

	fn intern(&self, span: Span) -> Spur { self.rodeo.borrow_mut().get_or_intern(&self.input[span]) }

	fn parse(&'a self) -> impl CParser<TokenKind, Module, Error = Simple<TokenKind, Span>> + 'a {
		let spur = just(TokenKind::Ident).map_with_span(|_, span| self.intern(span));
		let ident = spur.map_with_span(|spur, span| Ident { node: spur, span });

		let pat = ident
			.clone()
			.map(|i| PatKind::Binding(i.node))
			.or(just(TokenKind::Underscore).map(|_| PatKind::Ignore))
			.map_with_span(|node, span| Pat { node, span });

		let lit = just(TokenKind::BoolLiteral)
			.map(|_| LitKind::Bool)
			.or(just(TokenKind::CharLiteral).map(|_| LitKind::Char))
			.or(just(TokenKind::IntLiteral).map(|_| LitKind::Int))
			.or(just(TokenKind::FloatLiteral).map(|_| LitKind::Float))
			.or(just(TokenKind::StringLiteral).map(|_| LitKind::String))
			.map_with_span(|kind, span| Lit {
				kind,
				spur: self.intern(span),
			});

		let expr = lit.map_with_span(|lit, span| Expr {
			node: ExprKind::Lit(lit),
			span,
		});
		let mut item = Recursive::declare();

		let stmt = item
			.clone()
			.map(|item: Item| StmtKind::Item(item.visibility, item.kind))
			.or(expr.clone().map(|expr: Expr| StmtKind::Expr(expr.node)))
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
					.then(expr.clone().or_not())
					.map_with_span(|(on, bounds), span| WhereClause { on, bounds, span })
					.separated_by(just(TokenKind::Comma))
					.allow_trailing(),
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

		item.repeated().map(|items| Module { items }).then_ignore(end())
	}
}
