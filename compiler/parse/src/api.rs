use std::{collections::VecDeque, ops::Range};

use lex::{token::Token, Lexer, T};
use syntax::{
	builder::{Checkpoint, TreeBuilder, TreeBuilderContext},
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
		builder.start_node(SyntaxKind::File);
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

		self.builder.finish_node();
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

	pub fn node_depth(&self) -> usize { self.builder.node_depth() }

	pub fn start_node(&mut self, kind: SyntaxKind) {
		self.output_trivia();
		self.builder.start_node(kind);
	}

	pub fn finish_node(&mut self) {
		self.builder.finish_node();
		self.output_trivia();
	}

	pub fn finish_node_at(&mut self, node_depth: usize) {
		self.builder.finish_node_at(node_depth);
		self.output_trivia();
	}

	pub fn checkpoint(&self) -> Checkpoint { self.builder.checkpoint() }

	pub fn start_node_at(&mut self, checkpoint: Checkpoint, kind: SyntaxKind) {
		self.builder.start_node_at(checkpoint, kind);
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
	let kind = SyntaxKind::from(tok.kind);
	let text = &lexer.source()[tok.span.start as usize..tok.span.end as usize];
	builder.token(kind, text);
}
