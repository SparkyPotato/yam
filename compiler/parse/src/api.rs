use std::{collections::VecDeque, ops::Range};

use lex::{
	token::{FileSpan, Token, TokenKind},
	Lexer,
	T,
};
use syntax::{
	builder::{Branch, Checkpoint, TreeBuilder, TreeBuilderContext},
	SyntaxKind,
};

pub struct Api<'c, 's> {
	lexer: Lexer<'s>,
	builder: TreeBuilder<'c>,
	lookahead: [Token; Api::MAX_LOOKAHEAD],
	trivia_ranges: [Range<usize>; Api::MAX_LOOKAHEAD],
	trivia_buf: VecDeque<Token>,
}

impl<'c, 's> Api<'c, 's> {
	pub fn new(source: &'s str, ctx: &'c mut TreeBuilderContext) -> Self {
		const EMPTY_RANGE: Range<usize> = 0..0;

		let mut builder = TreeBuilder::new(ctx);

		let mut this = Api {
			builder,
			lexer: Lexer::new(source),
			lookahead: [Token::default(); Self::MAX_LOOKAHEAD],
			trivia_ranges: [EMPTY_RANGE; Self::MAX_LOOKAHEAD],
			trivia_buf: VecDeque::new(),
		};

		this.fill_lookahead();
		this
	}

	pub fn finish(mut self) -> TreeBuilder<'c> {
		for _ in 0..Self::MAX_LOOKAHEAD {
			self.output_trivia();

			let (next, range) = self.next();
			self.push_lookahead(next, range);
		}

		self.builder
	}
}

impl Api<'_, '_> {
	const MAX_LOOKAHEAD: usize = 2;

	/// Skips trivia.
	pub fn bump(&mut self) {
		self.output_trivia();

		let tok = self.peek();
		if !matches!(tok.kind, T![eof]) {
			self::tok(tok, &self.lexer, &mut self.builder);

			let (next, range) = self.next();
			self.push_lookahead(next, range);
		}
	}

	pub fn peek_n(&self, i: u8) -> Token { self.lookahead[i as usize] }

	pub fn peek(&self) -> Token { self.peek_n(0) }

	pub fn start_node(&mut self, kind: SyntaxKind) -> Branch { self.builder.start_node(kind) }

	pub fn finish_node(&mut self, branch: Branch) { self.builder.finish_node(branch) }

	pub fn checkpoint(&self) -> Checkpoint { self.builder.checkpoint() }

	pub fn start_node_at(&mut self, checkpoint: Checkpoint, kind: SyntaxKind) -> Branch {
		self.builder.start_node_at(checkpoint, kind)
	}

	pub fn is_span_eof(&self, span: FileSpan) -> bool {
		let len = self.lexer.source().len();
		span.start >= len as u32
	}

	fn push_lookahead(&mut self, tok: Token, trivia_range: Range<usize>) {
		for i in 0..Self::MAX_LOOKAHEAD - 1 {
			self.lookahead[i] = self.lookahead[i + 1];
			self.trivia_ranges[i] = self.trivia_ranges[i + 1].clone();
		}
		self.lookahead[Self::MAX_LOOKAHEAD - 1] = tok;
		self.trivia_ranges[Self::MAX_LOOKAHEAD - 1] = trivia_range;
	}

	fn next(&mut self) -> (Token, Range<usize>) {
		let len = self.trivia_buf.len();
		let mut added = 0;
		loop {
			let next = self.lexer.next();
			if !matches!(next.kind, T![comment] | T![ws]) {
				return (next, len..len + added);
			}
			self.trivia_buf.push_back(next);
			added += 1;
		}
	}

	fn output_trivia(&mut self) {
		let range_removed = self.trivia_ranges[0].clone();
		let count = range_removed.len();
		for tok in self.trivia_buf.drain(range_removed) {
			self::tok(tok, &self.lexer, &mut self.builder);
		}

		let range = &mut self.trivia_ranges[0];
		range.start = 0;
		range.end = 0;
		for trivia in self.trivia_ranges[1..].iter_mut() {
			*trivia = trivia.start - count..trivia.end - count;
		}
	}

	fn fill_lookahead(&mut self) {
		for _ in 0..Self::MAX_LOOKAHEAD {
			let (tok, range) = self.next();
			self.push_lookahead(tok, range);
		}
	}
}

fn tok(tok: Token, lexer: &Lexer, builder: &mut TreeBuilder) {
	let kind = tok_to_syntax(tok.kind);
	let text = &lexer.source()[tok.span.start as usize..tok.span.end as usize];
	builder.token(kind, text);
}

pub fn tok_to_syntax(tok: TokenKind) -> SyntaxKind {
	match tok {
		T![bool] => SyntaxKind::BoolLit,
		T![char] => SyntaxKind::CharLit,
		T![float] => SyntaxKind::FloatLit,
		T![int] => SyntaxKind::IntLit,
		T![string] => SyntaxKind::StringLit,
		T![ident] => SyntaxKind::Ident,
		T![@] => SyntaxKind::At,
		T!['('] => SyntaxKind::LParen,
		T!['{'] => SyntaxKind::LBrace,
		T!['['] => SyntaxKind::LBracket,
		T![')'] => SyntaxKind::RParen,
		T!['}'] => SyntaxKind::RBrace,
		T![']'] => SyntaxKind::RBracket,
		T![:] => SyntaxKind::Colon,
		T![,] => SyntaxKind::Comma,
		T![;] => SyntaxKind::Semi,
		T![_] => SyntaxKind::Underscore,
		T![->] => SyntaxKind::Arrow,
		T![op] => SyntaxKind::Operator,
		T![err] => SyntaxKind::Error,
		T![ws] => SyntaxKind::Whitespace,
		T![comment] => SyntaxKind::Comment,
		T![eof] => unreachable!("eof not allowed in syntax"),
	}
}
