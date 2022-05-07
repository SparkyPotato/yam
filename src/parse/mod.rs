use std::cell::RefCell;

use ariadne::{Label, Report, ReportKind};
use chumsky::{error::SimpleReason, prelude::*, Parser as CParser, Stream};
use lasso::{Rodeo, Spur};

use crate::parse::{
	ast::{
		Access,
		Arg,
		Array,
		BinOp,
		Binary,
		Block,
		Call,
		CallArg,
		Expr,
		ExprKind,
		Fn,
		Ident,
		Index,
		Let,
		Lit,
		LitKind,
		Module,
		Pat,
		PatKind,
		Span,
		Stmt,
		StmtKind,
		Struct,
		UnOp,
		Unary,
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

	module.unwrap_or_else(|| Module { stmts: Vec::new() })
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
		let sym = just(TokenKind::Ident).map_with_span(|_, span| self.intern(span));
		let ident = sym.clone().map_with_span(|spur, span| Ident { node: spur, span });

		let pat = sym
			.clone()
			.map(PatKind::Binding)
			.or(just(TokenKind::Underscore).map(|_| PatKind::Ignore))
			.map_with_span(|node, span| Pat { node, span });

		let visibility = just(TokenKind::Pub).or_not().map(|vis| {
			if vis.is_some() {
				Visibility::Public
			} else {
				Visibility::Private
			}
		});

		let mut item = Recursive::declare();
		let mut expr = Recursive::declare();

		let stmt = item
			.clone()
			.map(|item: Expr| StmtKind::Semi(item.node))
			.or(expr
				.clone()
				.then(just(TokenKind::Semi).or_not())
				.map(|(expr, semi): (Expr, _)| {
					if semi.is_some() {
						StmtKind::Semi(expr.node)
					} else {
						StmtKind::Expr(expr.node)
					}
				}))
			.map_with_span(|node, span| Stmt { node, span });

		let block = just(TokenKind::Const)
			.or_not()
			.then(stmt.clone().repeated().delimited_by(
				just(TokenKind::LDelim(Delim::Brace)),
				just(TokenKind::RDelim(Delim::Brace)),
			))
			.map_with_span(|(constness, stmts), span| Block {
				is_const: constness.is_some(),
				stmts,
				span,
			});

		let lit = select! {
			TokenKind::BoolLiteral => LitKind::Bool,
			TokenKind::CharLiteral => LitKind::Char,
			TokenKind::IntLiteral => LitKind::Int,
			TokenKind::FloatLiteral => LitKind::Float,
			TokenKind::StringLiteral => LitKind::String,
		}
		.map_with_span(|kind, span| Lit {
			kind,
			sym: self.intern(span),
		});

		let let_ = pat
			.clone()
			.then(just(TokenKind::Colon).ignore_then(expr.clone()).or_not())
			.then(just(TokenKind::Assign).ignore_then(expr.clone()).or_not())
			.map(|((pat, ty), expr)| (pat, ty.map(|ty| Box::new(ty)), expr.map(|expr| Box::new(expr))));

		let const_ = visibility
			.clone()
			.then_ignore(just(TokenKind::Const))
			.then(let_.clone())
			.map(|(visibility, (pat, ty, expr))| Let {
				visibility,
				pat,
				ty,
				expr,
			});
		let let_ =
			visibility
				.clone()
				.then_ignore(just(TokenKind::Let))
				.then(let_)
				.map(|(visibility, (pat, ty, expr))| Let {
					visibility,
					pat,
					ty,
					expr,
				});

		let list = just(TokenKind::LDelim(Delim::Bracket))
			.ignore_then(expr.clone().separated_by(just(TokenKind::Comma)).allow_trailing())
			.then_ignore(just(TokenKind::RDelim(Delim::Bracket)));

		let array = just(TokenKind::LDelim(Delim::Bracket))
			.ignore_then(expr.clone())
			.then_ignore(just(TokenKind::Semi))
			.then(expr.clone())
			.then_ignore(just(TokenKind::RDelim(Delim::Bracket)))
			.map(|(expr, count)| Array {
				expr: Box::new(expr),
				count: Box::new(count),
			});

		let arg = visibility
			.then(just(TokenKind::Const).or_not())
			.then(pat.clone())
			.then(just(TokenKind::Colon).ignore_then(expr.clone()).or_not())
			.map_with_span(|(((visibility, constness), pat), bounds), span| Arg {
				visibility,
				is_const: constness.is_some(),
				pat,
				bounds,
				span,
			});
		let fields = arg
			.clone()
			.separated_by(just(TokenKind::Comma))
			.allow_trailing()
			.delimited_by(
				just(TokenKind::LDelim(Delim::Brace)),
				just(TokenKind::RDelim(Delim::Brace)),
			);
		let generics = arg
			.clone()
			.separated_by(just(TokenKind::Comma))
			.allow_trailing()
			.delimited_by(just(TokenKind::Lt), just(TokenKind::Gt))
			.or_not()
			.map(|params| params.unwrap_or(Vec::new()));
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

		let struct_ty = just(TokenKind::Struct)
			.ignore_then(generics.clone())
			.then(where_clause.clone())
			.then(fields.clone())
			.map(|((generics, where_clause), fields)| Struct {
				generics,
				where_clause,
				fields,
			});

		let args = arg
			.clone()
			.separated_by(just(TokenKind::Comma))
			.allow_trailing()
			.delimited_by(
				just(TokenKind::LDelim(Delim::Paren)),
				just(TokenKind::RDelim(Delim::Paren)),
			);
		let lambda = just(TokenKind::Fn)
			.ignore_then(generics.clone())
			.then(args.clone())
			.then(just(TokenKind::Arrow).ignore_then(expr.clone()).or_not())
			.then(where_clause.clone())
			.then(block.clone())
			.map(|((((generics, args), ret), where_clause), block)| Fn {
				generics,
				args,
				ret: ret.map(|ret| Box::new(ret)),
				where_clause,
				block,
			});

		let atom = lit
			.map(ExprKind::Lit)
			.or(block.clone().map(ExprKind::Block))
			.or(ident.clone().map(ExprKind::Ident))
			.or(const_.map(ExprKind::Const))
			.or(let_.map(ExprKind::Let))
			.or(list.map(ExprKind::List))
			.or(array.map(ExprKind::Array))
			.or(just(TokenKind::Type).map(|_| ExprKind::Type))
			.or(struct_ty.map(ExprKind::Struct))
			.or(lambda.map(ExprKind::Fn))
			.map_with_span(|node, span| Expr { node, span })
			.or(expr.clone().delimited_by(
				just(TokenKind::LDelim(Delim::Paren)),
				just(TokenKind::RDelim(Delim::Paren)),
			))
			.recover_with(nested_delimiters(
				TokenKind::LDelim(Delim::Paren),
				TokenKind::RDelim(Delim::Paren),
				[
					(TokenKind::LDelim(Delim::Brace), TokenKind::RDelim(Delim::Brace)),
					(TokenKind::LDelim(Delim::Bracket), TokenKind::RDelim(Delim::Bracket)),
				],
				|span| Expr {
					node: ExprKind::Error,
					span,
				},
			))
			.recover_with(nested_delimiters(
				TokenKind::LDelim(Delim::Brace),
				TokenKind::RDelim(Delim::Brace),
				[
					(TokenKind::LDelim(Delim::Paren), TokenKind::RDelim(Delim::Paren)),
					(TokenKind::LDelim(Delim::Bracket), TokenKind::RDelim(Delim::Bracket)),
				],
				|span| Expr {
					node: ExprKind::Error,
					span,
				},
			))
			.recover_with(nested_delimiters(
				TokenKind::LDelim(Delim::Bracket),
				TokenKind::RDelim(Delim::Bracket),
				[
					(TokenKind::LDelim(Delim::Paren), TokenKind::RDelim(Delim::Paren)),
					(TokenKind::LDelim(Delim::Brace), TokenKind::RDelim(Delim::Brace)),
				],
				|span| Expr {
					node: ExprKind::Error,
					span,
				},
			))
			.boxed();

		enum CallIndexAccess {
			Call(Vec<CallArg>),
			Index(Expr),
			Access(Ident),
		}
		let call = atom
			.then(
				pat.clone()
					.then_ignore(just(TokenKind::Colon))
					.then(expr.clone())
					.map_with_span(|(pat, expr), span| CallArg {
						pat: Some(pat),
						expr,
						span,
					})
					.or(expr.clone().map(|expr| CallArg {
						pat: None,
						span: expr.span,
						expr,
					}))
					.separated_by(just(TokenKind::Comma))
					.allow_trailing()
					.delimited_by(
						just(TokenKind::LDelim(Delim::Paren)),
						just(TokenKind::RDelim(Delim::Paren)),
					)
					.map_with_span(|args, span| (CallIndexAccess::Call(args), span))
					.or(expr
						.clone()
						.delimited_by(
							just(TokenKind::LDelim(Delim::Bracket)),
							just(TokenKind::RDelim(Delim::Bracket)),
						)
						.map_with_span(|index, span| (CallIndexAccess::Index(index), span)))
					.or(just(TokenKind::Dot)
						.ignore_then(ident.clone())
						.map_with_span(|field, span| (CallIndexAccess::Access(field), span)))
					.repeated(),
			)
			.foldl(|f, access| {
				let span = f.span + access.1;
				Expr {
					node: match access.0 {
						CallIndexAccess::Call(args) => ExprKind::Call(Call {
							target: Box::new(f),
							args,
						}),
						CallIndexAccess::Index(index) => ExprKind::Index(Index {
							target: Box::new(f),
							index: Box::new(index),
						}),
						CallIndexAccess::Access(field) => ExprKind::Access(Access {
							target: Box::new(f),
							field,
						}),
					},
					span,
				}
			});

		let unary = select! {
			TokenKind::Not => UnOp::Not,
			TokenKind::Minus => UnOp::Neg,
			TokenKind::BitAnd => UnOp::Addr,
			TokenKind::Mul => UnOp::Deref,
		}
		.or(just([TokenKind::BitAnd, TokenKind::Mut]).to(UnOp::AddrMut))
		.map_with_span(|op, span| (op, span))
		.repeated()
		.then(call)
		.foldr(|op, expr| {
			let span = op.1 + expr.span;
			Expr {
				node: ExprKind::Unary(Unary {
					op: op.0,
					expr: Box::new(expr),
				}),
				span,
			}
		});

		fn binary(
			side: impl CParser<TokenKind, Expr, Error = Simple<TokenKind, Span>> + Clone,
			op: impl CParser<TokenKind, BinOp, Error = Simple<TokenKind, Span>> + Clone,
		) -> impl CParser<TokenKind, Expr, Error = Simple<TokenKind, Span>> + Clone {
			side.clone().then(op.then(side).repeated()).foldl(|lhs, (op, rhs)| {
				let span = lhs.span + rhs.span;
				Expr {
					node: ExprKind::Binary(Binary {
						lhs: Box::new(lhs),
						op,
						rhs: Box::new(rhs),
					}),
					span,
				}
			})
		}

		let product = binary(
			unary,
			select! {
				TokenKind::Mul => BinOp::Mul,
				TokenKind::Div => BinOp::Div,
				TokenKind::Percent => BinOp::Rem,
			},
		);
		let sum = binary(
			product,
			select! {
				TokenKind::Plus => BinOp::Add,
				TokenKind::Minus => BinOp::Sub,
			},
		);
		let shift = binary(
			sum,
			select! {
				TokenKind::Shl => BinOp::Shl,
				TokenKind::Shr => BinOp::Shr,
			},
		);
		let comparison = binary(
			shift,
			select! {
				TokenKind::Gt => BinOp::Gt,
				TokenKind::Geq => BinOp::Geq,
				TokenKind::Lt => BinOp::Lt,
				TokenKind::Leq => BinOp::Leq,
			},
		);
		let equality = binary(
			comparison,
			select! {
				TokenKind::Eq => BinOp::Eq,
				TokenKind::Neq => BinOp::Neq,
			},
		);
		let bitand = binary(equality, just(TokenKind::BitAnd).to(BinOp::BitAnd)).boxed();
		let bitxor = binary(bitand, just(TokenKind::BitXor).to(BinOp::BitXor)).boxed();
		let bitor = binary(bitxor, just(TokenKind::BitOr).to(BinOp::BitOr)).boxed();
		let and = binary(bitor, just(TokenKind::And).to(BinOp::And)).boxed();
		let or = binary(and, just(TokenKind::Or).to(BinOp::Or)).boxed();
		let assign = binary(
			or,
			select! {
				TokenKind::Assign => BinOp::Assign,
				TokenKind::PlusEq => BinOp::AddAssign,
				TokenKind::MinusEq => BinOp::SubAssign,
				TokenKind::MulEq => BinOp::MulAssign,
				TokenKind::DivEq => BinOp::DivAssign,
				TokenKind::PercentEq => BinOp::RemAssign,
				TokenKind::ShlEq => BinOp::ShlAssign,
				TokenKind::ShrEq => BinOp::ShrAssign,
				TokenKind::BitAndEq => BinOp::BitAndAssign,
				TokenKind::BitXorEq => BinOp::BitXorAssign,
				TokenKind::BitOrEq => BinOp::BitOrAssign,
			},
		);

		expr.define(assign.boxed());

		let function_item = just(TokenKind::Fn)
			.ignore_then(ident.clone())
			.then(generics.clone())
			.then(args)
			.then(just(TokenKind::Arrow).ignore_then(expr.clone()).or_not())
			.then(where_clause.clone())
			.then(block.clone())
			.map(|(((((ident, generics), args), ret), where_clause), block)| {
				(
					ident,
					Fn {
						generics,
						args,
						ret: ret.map(|ret| Box::new(ret)),
						where_clause,
						block,
					},
				)
			});

		let struct_item = just(TokenKind::Struct)
			.ignore_then(ident.clone())
			.then(generics.clone())
			.then(where_clause.clone())
			.then(fields)
			.map(|(((ident, generics), where_clause), fields)| {
				(
					ident,
					Struct {
						generics,
						where_clause,
						fields,
					},
				)
			});

		item.define(
			visibility
				.then(
					function_item
						.map(|(ident, kind)| (ident, ExprKind::Fn(kind)))
						.or(struct_item.map(|(ident, kind)| (ident, ExprKind::Struct(kind)))),
				)
				.map_with_span(|(visibility, (ident, kind)), span| Expr {
					node: ExprKind::Const(Let {
						visibility,
						pat: Pat {
							node: PatKind::Binding(ident.node),
							span: ident.span,
						},
						ty: None,
						expr: Some(Box::new(Expr { node: kind, span })),
					}),
					span,
				}),
		);

		stmt.repeated().map(|stmts| Module { stmts }).then_ignore(end())
	}
}
