use std::cell::RefCell;

use chumsky::{error::SimpleReason, prelude::*, Parser as CParser, Stream};
use diag::{Diagnostics, Span};
pub use lasso::{Rodeo, Spur};

use crate::{
	ast::*,
	token::{Delim, TokenKind},
};

pub mod ast;
pub mod token;

pub fn parse(file: Spur, input: &str, rodeo: &mut Rodeo, diagnostics: &mut Diagnostics) -> Module {
	let lexer = Lexer {
		file,
		lexer: logos::Lexer::new(input),
	};

	let (module, errors) =
		Parser::parse(&Parser::new(input, rodeo, diagnostics), file).parse_recovery(Stream::from_iter(
			Span {
				start: input.len() as _,
				end: input.len() as _,
				file,
			},
			lexer,
		));

	for error in errors {
		let span = error.span();

		let diag = match error.reason() {
			SimpleReason::Custom(s) => span.error(s).label(span.mark()),
			SimpleReason::Unexpected => {
				let diag = span.error(match error.found() {
					Some(tok) => match error.label() {
						Some(label) => format!("unexpected `{}` while parsing {}", tok, label),
						None => format!("unexpected `{}`", tok),
					},
					None => "unexpected `<eof>`".into(),
				});

				match error.expected().len() {
					0 => diag.label(span.mark()),
					1 => diag.label(span.label(match error.expected().next().unwrap() {
						Some(tok) => format!("expected `{}`", tok),
						None => "expected `<eof>`".into(),
					})),
					_ => diag.label(span.label(format!(
						"expected one of {}",
						error
							.expected()
							.map(|tok| match tok {
								Some(tok) => format!("`{}`", tok),
								None => "`<eof>`".into(),
							})
							.collect::<Vec<_>>()
							.join(", ")
					))),
				}
			},
			SimpleReason::Unclosed { delimiter, span } => match error.label() {
				Some(label) => span
					.error(format!("unclosed `{}` while parsing {}", delimiter, label))
					.label(span.mark()),
				None => span.error(format!("unclosed `{}`", delimiter)).label(span.mark()),
			},
		};

		diagnostics.push(diag);
	}

	module.unwrap_or_else(|| Module {
		items: Vec::new(),
		source: file,
	})
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
	rodeo: &'a mut Rodeo,
	diagnostics: &'a mut Diagnostics,
}

impl<'a> Parser<'a> {
	fn new(input: &'a str, rodeo: &'a mut Rodeo, diagnostics: &'a mut Diagnostics) -> RefCell<Self> {
		RefCell::new(Self {
			input,
			rodeo,
			diagnostics,
		})
	}

	fn intern(this: &RefCell<Self>, span: Span) -> Spur {
		let mut this = this.borrow_mut();
		let input = &this.input[span];
		this.rodeo.get_or_intern(input)
	}

	fn parse(
		this: &'a RefCell<Self>, file: Spur,
	) -> impl CParser<TokenKind, Module, Error = Simple<TokenKind, Span>> + 'a {
		let sym = just(TokenKind::Ident)
			.map_with_span(|_, span| Self::intern(this, span))
			.debug("<ident>")
			.labelled("<ident>");
		let ident = sym
			.clone()
			.map_with_span(|spur, span| Ident { node: spur, span })
			.debug("<ident>")
			.labelled("<ident>");

		let binding = just(TokenKind::Mut)
			.or_not()
			.then(sym.clone())
			.map(|(var, binding)| Binding {
				mutability: var.is_some(),
				binding,
			})
			.debug("<binding>")
			.labelled("<binding>");
		let pat = binding
			.map_with_span(|binding, span| Pat {
				node: PatKind::Binding(binding),
				span,
			})
			.debug("<pat>")
			.labelled("<pat>");

		let mut item = Recursive::declare();
		let mut expr = Recursive::declare();

		let stmt = choice((
			item.clone().map(|item: Item| StmtKind::Item(item.kind)),
			expr.clone()
				.then(just(TokenKind::Semi).or_not())
				.map(|(expr, semi): (Expr, _)| {
					if semi.is_some() {
						StmtKind::Semi(expr.node)
					} else {
						StmtKind::Expr(expr.node)
					}
				}),
		))
		.map_with_span(|node, span| Stmt { node, span })
		.debug("<stmt>")
		.labelled("<stmt>")
		.boxed();

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
			})
			.debug("<block>")
			.labelled("<block>")
			.boxed();

		let lit = select! {
			TokenKind::BoolLiteral => LitKind::Bool,
			TokenKind::CharLiteral => LitKind::Char,
			TokenKind::IntLiteral => LitKind::Int,
			TokenKind::FloatLiteral => LitKind::Float,
			TokenKind::StringLiteral => LitKind::String,
		}
		.map_with_span(|kind, span| Lit {
			kind,
			sym: Self::intern(this, span),
		})
		.debug("<lit>")
		.labelled("<lit>")
		.boxed();

		let let_ = pat
			.clone()
			.then(just(TokenKind::Colon).ignore_then(expr.clone()).or_not())
			.then(just(TokenKind::Assign).ignore_then(expr.clone()).or_not())
			.map(|((pat, ty), expr)| (pat, ty.map(Box::new), expr.map(Box::new)))
			.debug("<let>")
			.labelled("<let>")
			.boxed();

		fn make_pat_mutable(pat: &mut Pat, diagnostics: &mut Diagnostics) {
			match &mut pat.node {
				PatKind::Binding(binding) => {
					if binding.mutability {
						diagnostics.push(
							pat.span
								.warning("mutable pattern in `var` declaration")
								.label(pat.span.label("`var`s are already mutable")),
						);
					}
					binding.mutability = true;
				},
			}
		}

		let const_ = just(TokenKind::Const)
			.ignore_then(let_.clone())
			.then_ignore(just(TokenKind::Semi))
			.map(|(pat, ty, expr)| ItemKind::Const(Let { pat, ty, expr }))
			.debug("<const>")
			.labelled("<const>")
			.boxed();

		let static_ = just(TokenKind::Static)
			.ignore_then(let_.clone())
			.then_ignore(just(TokenKind::Semi))
			.map(|(pat, ty, expr)| ItemKind::Static(Let { pat, ty, expr }))
			.debug("<static>")
			.labelled("<static>")
			.boxed();
		let var = just(TokenKind::Var)
			.ignore_then(let_.clone())
			.map(|(mut pat, ty, expr)| {
				let mut this = this.borrow_mut();
				make_pat_mutable(&mut pat, this.diagnostics);
				Let { pat, ty, expr }
			})
			.debug("<var>")
			.labelled("<var>")
			.boxed();
		let let_ = just(TokenKind::Let)
			.ignore_then(let_)
			.map(|(pat, ty, expr)| Let { pat, ty, expr })
			.debug("<let>")
			.labelled("<let>")
			.boxed();

		let list = just(TokenKind::LDelim(Delim::Bracket))
			.ignore_then(expr.clone().separated_by(just(TokenKind::Comma)).allow_trailing())
			.then_ignore(just(TokenKind::RDelim(Delim::Bracket)))
			.debug("<list>")
			.labelled("<list>")
			.boxed();

		let array = just(TokenKind::LDelim(Delim::Bracket))
			.ignore_then(expr.clone())
			.then_ignore(just(TokenKind::Semi))
			.then(expr.clone())
			.then_ignore(just(TokenKind::RDelim(Delim::Bracket)))
			.map(|(expr, count)| Array {
				expr: Box::new(expr),
				count: Box::new(count),
			})
			.debug("<array>")
			.labelled("<array>")
			.boxed();

		let visibility = just(TokenKind::Pub)
			.or_not()
			.map(|vis| {
				if vis.is_some() {
					Visibility::Public
				} else {
					Visibility::Private
				}
			})
			.debug("<vis>")
			.labelled("<vis>");

		let arg = visibility
			.clone()
			.then(just(TokenKind::Const).or_not())
			.then(pat.clone())
			.then(just(TokenKind::Colon).ignore_then(expr.clone()))
			.map_with_span(|(((visibility, constness), pat), ty), span| Arg {
				visibility,
				is_const: constness.is_some(),
				pat,
				ty,
				span,
			})
			.debug("<arg>")
			.labelled("<arg>")
			.boxed();
		let fields = arg
			.clone()
			.separated_by(just(TokenKind::Comma))
			.allow_trailing()
			.delimited_by(
				just(TokenKind::LDelim(Delim::Brace)),
				just(TokenKind::RDelim(Delim::Brace)),
			)
			.debug("<fields>")
			.labelled("<fields>")
			.boxed();
		let args = arg
			.clone()
			.separated_by(just(TokenKind::Comma))
			.allow_trailing()
			.delimited_by(
				just(TokenKind::LDelim(Delim::Paren)),
				just(TokenKind::RDelim(Delim::Paren)),
			)
			.debug("<args>")
			.labelled("<args>")
			.boxed();

		let lambda = just(TokenKind::Fn)
			.ignore_then(args.clone())
			.then(just(TokenKind::Arrow).ignore_then(expr.clone()).or_not())
			.then(block.clone())
			.map(|((args, ret), block)| Fn {
				abi: Abi::None,
				args,
				block: Some(block),
				ret: ret.map(Box::new),
			})
			.debug("<fn>")
			.labelled("<fn>")
			.boxed();

		let if_ = just(TokenKind::If)
			.ignore_then(expr.clone())
			.then(block.clone())
			.then(just(TokenKind::Else).ignore_then(expr.clone()).or_not())
			.map(|((cond, then), else_)| If {
				cond: Box::new(cond),
				then,
				else_: else_.map(Box::new),
			});

		let loop_ = just(TokenKind::Loop)
			.ignore_then(block.clone())
			.then(just(TokenKind::While).ignore_then(expr.clone()).or_not())
			.map(|(block, while_)| Loop {
				block,
				while_: while_.map(Box::new),
			});

		let while_ = just(TokenKind::While)
			.ignore_then(expr.clone())
			.then(block.clone())
			.map(|(cond, block)| While {
				cond: Box::new(cond),
				block,
			});

		let for_ = just(TokenKind::For)
			.ignore_then(pat.clone())
			.then_ignore(just(TokenKind::In))
			.then(expr.clone())
			.then(block.clone())
			.map(|((pat, iter), block)| For {
				pat,
				iter: Box::new(iter),
				block,
			});

		let atom = choice((
			just(TokenKind::Underscore).to(ExprKind::Infer),
			expr.clone()
				.delimited_by(
					just(TokenKind::LDelim(Delim::Paren)),
					just(TokenKind::RDelim(Delim::Paren)),
				)
				.map(|expr| expr.node),
			block.clone().map(ExprKind::Block),
			lit.map(ExprKind::Lit),
			sym.clone().map(ExprKind::Ident),
			let_.map(ExprKind::Let),
			var.map(ExprKind::Let),
			list.map(ExprKind::List).or(array.map(ExprKind::Array)),
			just(TokenKind::Type)
				.ignore_then(
					expr.clone()
						.delimited_by(
							just(TokenKind::LDelim(Delim::Paren)),
							just(TokenKind::RDelim(Delim::Paren)),
						)
						.or_not(),
				)
				.map(|of| match of {
					Some(of) => ExprKind::TypeOf(Box::new(of)),
					None => ExprKind::Type,
				}),
			lambda.map(ExprKind::Fn),
			just(TokenKind::Break)
				.ignore_then(expr.clone().or_not())
				.map(|expr| ExprKind::Break(expr.map(Box::new))),
			just(TokenKind::Continue).to(ExprKind::Continue),
			just(TokenKind::Return)
				.ignore_then(expr.clone().or_not())
				.map(|expr| ExprKind::Return(expr.map(Box::new))),
			if_.map(ExprKind::If),
			loop_.map(ExprKind::Loop),
			while_.map(ExprKind::While),
			for_.map(ExprKind::For),
		))
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
				node: ExprKind::Err,
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
				node: ExprKind::Err,
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
				node: ExprKind::Err,
				span,
			},
		))
		.debug("<atom>")
		.labelled("<atom>")
		.boxed();

		let ptr = recursive(|ptr| {
			just(TokenKind::Mul)
				.ignore_then(
					select! {
						TokenKind::Const => false,
						TokenKind::Mut => true,
					}
					.or_not(),
				)
				.then(ptr)
				.map(|(mutability, to)| match mutability {
					Some(mutability) => ExprKind::Ptr(Ptr {
						mutability,
						to: Box::new(to),
					}),
					None => ExprKind::Unary(Unary {
						op: UnOp::Deref,
						expr: Box::new(to),
					}),
				})
				.map_with_span(|node, span| Expr { node, span })
				.or(atom)
				.debug("<ptr>")
				.labelled("<ptr>")
				.boxed()
		});

		enum CallIndexAccess {
			Call(Vec<Expr>),
			Index(Expr),
			Access(Ident),
		}
		let call = ptr
			.then(
				expr.clone()
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
			})
			.debug("<call>")
			.labelled("<call>")
			.boxed();

		let unary = select! {
			TokenKind::Not => UnOp::Not,
			TokenKind::Minus => UnOp::Neg,
			TokenKind::BitAnd => UnOp::Addr,
			TokenKind::And => UnOp::DoubleAddr,
		}
		.or(just([TokenKind::BitAnd, TokenKind::Mut]).to(UnOp::AddrMut))
		.or(just([TokenKind::And, TokenKind::Mut]).to(UnOp::DoubleAddrMut))
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
		})
		.or(just(TokenKind::Not).map_with_span(|_, span| Expr {
			node: ExprKind::Never,
			span,
		}))
		.debug("<unary>")
		.labelled("<unary>")
		.boxed();

		fn binary<'a>(
			side: impl CParser<TokenKind, Expr, Error = Simple<TokenKind, Span>> + Clone + 'a,
			op: impl CParser<TokenKind, BinOp, Error = Simple<TokenKind, Span>> + Clone + 'a,
		) -> impl CParser<TokenKind, Expr, Error = Simple<TokenKind, Span>> + Clone + 'a {
			side.clone()
				.then(op.then(side).repeated())
				.foldl(|lhs, (op, rhs)| {
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
				.boxed()
		}

		let cast = unary
			.clone()
			.then(just(TokenKind::As).ignore_then(unary).repeated())
			.foldl(|expr, ty| {
				let span = expr.span + ty.span;
				Expr {
					node: ExprKind::Cast(Cast {
						expr: Box::new(expr),
						ty: Box::new(ty),
					}),
					span,
				}
			})
			.boxed();

		let product = binary(
			cast,
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
		)
		.boxed();
		let bitand = binary(equality, just(TokenKind::BitAnd).to(BinOp::BitAnd));
		let bitxor = binary(bitand, just(TokenKind::BitXor).to(BinOp::BitXor));
		let bitor = binary(bitxor, just(TokenKind::BitOr).to(BinOp::BitOr));
		let and = binary(bitor, just(TokenKind::And).to(BinOp::And));
		let or = binary(and, just(TokenKind::Or).to(BinOp::Or));
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
				TokenKind::Arrow => BinOp::PlaceConstruct,
			},
		);

		expr.define(assign.debug("<expr>").labelled("<expr>"));

		let import = just(TokenKind::Import)
			.ignore_then(recursive(|import| {
				ident
					.clone()
					.separated_by(just(TokenKind::Dot))
					.then(
						just(TokenKind::Dot)
							.ignore_then(choice((
								just(TokenKind::Mul).to(ImportTree::Wildcard),
								import
									.separated_by(just(TokenKind::Comma))
									.allow_trailing()
									.delimited_by(
										just(TokenKind::LDelim(Delim::Brace)),
										just(TokenKind::RDelim(Delim::Brace)),
									)
									.map(ImportTree::List),
							)))
							.or_not(),
					)
					.then_ignore(just(TokenKind::Semi).or_not())
					.map(|(prefix, tree)| Import {
						prefix,
						tree: tree.unwrap_or(ImportTree::None),
					})
			}))
			.map(ItemKind::Import)
			.debug("<import>")
			.labelled("<import>");

		let function_item = just(TokenKind::Extern)
			.ignore_then(
				just(TokenKind::StringLiteral)
					.map_with_span(|_, span| Spanned {
						node: Self::intern(this, span),
						span,
					})
					.or_not(),
			)
			.or_not()
			.then_ignore(just(TokenKind::Fn))
			.then(ident.clone())
			.then(args.clone())
			.then(just(TokenKind::Arrow).ignore_then(expr.clone()).or_not())
			.then(block.clone().map(Some).or(just(TokenKind::Semi).to(None)))
			.map(|((((abi, ident), args), ret), block)| {
				ItemKind::Fn(
					ident,
					Fn {
						abi: match abi {
							Some(Some(abi)) => Abi::Abi(abi),
							Some(None) => Abi::Extern,
							None => Abi::None,
						},
						args,
						ret: ret.map(Box::new),
						block,
					},
				)
			})
			.debug("<fn>")
			.labelled("<fn>")
			.boxed();

		let struct_item = just(TokenKind::Struct)
			.ignore_then(ident.clone())
			.then(fields)
			.map(|(ident, fields)| ItemKind::Struct(ident, Struct { fields }))
			.debug("<struct>")
			.labelled("<struct>")
			.boxed();

		let attrib = just(TokenKind::At)
			.ignore_then(ident.clone())
			.then(
				none_of([TokenKind::RDelim(Delim::Paren)])
					.map_with_span(|kind, span| Token {
						kind,
						data: Self::intern(this, span),
						span,
					})
					.repeated()
					.delimited_by(
						just(TokenKind::LDelim(Delim::Paren)),
						just(TokenKind::RDelim(Delim::Paren)),
					),
			)
			.map_with_span(|(name, values), span| Attr { name, values, span })
			.debug("<attrib>")
			.labelled("<attrib>")
			.boxed();

		item.define(
			attrib
				.repeated()
				.then(visibility.then(choice((const_, static_, function_item, struct_item, import))))
				.map_with_span(|(attribs, (visibility, kind)), span| Item {
					attrs: attribs,
					visibility,
					kind,
					span,
				})
				.debug("<item>")
				.labelled("<item>"),
		);

		item.repeated()
			.map(move |items| Module { items, source: file })
			.then_ignore(end())
	}
}
