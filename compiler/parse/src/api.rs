use std::{collections::VecDeque, ops::Range};

use diag::Span;
use intern::Id;
use lex::{
	token::{Delim, Lit, Token, TokenKind},
	Lexer,
	T,
};
use syntax::{
	builder::{Branch, Checkpoint, TreeBuilder, TreeBuilderContext},
	kind::SyntaxKind,
};

pub struct Api<'i, 'c, 's> {
	lexer: Lexer<'s>,
	builder: TreeBuilder<'c, 'i>,
	lookahead: [Token; Api::MAX_LOOKAHEAD],
	trivia_ranges: [Range<usize>; Api::MAX_LOOKAHEAD],
	trivia_buf: VecDeque<Token>,
	ender: Branch,
}

impl<'i, 'c, 's> Api<'i, 'c, 's>
where
	'i: 'c,
{
	pub fn new(file_name: Id<str>, source: &'s str, ctx: &'c mut TreeBuilderContext<'i>) -> Self {
		const EMPTY_RANGE: Range<usize> = 0..0;

		let mut builder = TreeBuilder::new(ctx);
		let ender = builder.start_node(SyntaxKind::File);

		let mut this = Api {
			builder,
			lexer: Lexer::new(file_name, source),
			lookahead: [Token::default(); Self::MAX_LOOKAHEAD],
			trivia_ranges: [EMPTY_RANGE; Self::MAX_LOOKAHEAD],
			trivia_buf: VecDeque::new(),
			ender,
		};

		this.fill_lookahead();

		this
	}

	pub fn finish(mut self) -> TreeBuilder<'c, 'i> {
		for _ in 0..Self::MAX_LOOKAHEAD {
			self.output_trivia();

			let (next, range) = self.next();
			self.push_lookahead(next, range);
		}

		self.builder.finish_node(self.ender);
		self.builder
	}
}

impl Api<'_, '_, '_> {
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

	pub fn is_span_eof(&self, span: Span) -> bool {
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
		TokenKind::Lit(lit) => match lit {
			Lit::Bool => SyntaxKind::BoolLit,
			Lit::Char => SyntaxKind::CharLit,
			Lit::Float => SyntaxKind::FloatLit,
			Lit::Int => SyntaxKind::IntLit,
			Lit::String => SyntaxKind::StringLit,
		},
		TokenKind::Ident => SyntaxKind::Ident,
		TokenKind::At => SyntaxKind::At,
		TokenKind::LDelim(delim) => match delim {
			Delim::Paren => SyntaxKind::LParen,
			Delim::Brace => SyntaxKind::LBrace,
			Delim::Bracket => SyntaxKind::LBracket,
		},
		TokenKind::RDelim(delim) => match delim {
			Delim::Paren => SyntaxKind::RParen,
			Delim::Brace => SyntaxKind::RBrace,
			Delim::Bracket => SyntaxKind::RBracket,
		},
		TokenKind::Lt => SyntaxKind::Lt,
		TokenKind::Gt => SyntaxKind::Gt,
		TokenKind::Eq => SyntaxKind::Eq,
		TokenKind::Plus => SyntaxKind::Plus,
		TokenKind::Minus => SyntaxKind::Minus,
		TokenKind::Star => SyntaxKind::Star,
		TokenKind::Slash => SyntaxKind::Slash,
		TokenKind::Percent => SyntaxKind::Percent,
		TokenKind::And => SyntaxKind::And,
		TokenKind::Pipe => SyntaxKind::Pipe,
		TokenKind::Caret => SyntaxKind::Caret,
		TokenKind::Exclaim => SyntaxKind::Exclaim,
		TokenKind::Tilde => SyntaxKind::Tilde,
		TokenKind::Dot => SyntaxKind::Dot,
		TokenKind::Colon => SyntaxKind::Colon,
		TokenKind::Comma => SyntaxKind::Comma,
		TokenKind::Semi => SyntaxKind::Semi,
		TokenKind::Question => SyntaxKind::Question,
		TokenKind::Underscore => SyntaxKind::Underscore,
		TokenKind::As => SyntaxKind::AsKw,
		TokenKind::Break => SyntaxKind::BreakKw,
		TokenKind::Const => SyntaxKind::ConstKw,
		TokenKind::Continue => SyntaxKind::ContinueKw,
		TokenKind::Else => SyntaxKind::ElseKw,
		TokenKind::Enum => SyntaxKind::EnumKw,
		TokenKind::Extern => SyntaxKind::ExternKw,
		TokenKind::Fn => SyntaxKind::FnKw,
		TokenKind::For => SyntaxKind::ForKw,
		TokenKind::If => SyntaxKind::IfKw,
		TokenKind::In => SyntaxKind::InKw,
		TokenKind::Import => SyntaxKind::ImportKw,
		TokenKind::Let => SyntaxKind::LetKw,
		TokenKind::Loop => SyntaxKind::LoopKw,
		TokenKind::Mod => SyntaxKind::ModKw,
		TokenKind::Mut => SyntaxKind::MutKw,
		TokenKind::Pub => SyntaxKind::PubKw,
		TokenKind::Return => SyntaxKind::ReturnKw,
		TokenKind::Static => SyntaxKind::StaticKw,
		TokenKind::Struct => SyntaxKind::StructKw,
		TokenKind::Type => SyntaxKind::TypeKw,
		TokenKind::Union => SyntaxKind::UnionKw,
		TokenKind::Var => SyntaxKind::VarKw,
		TokenKind::Where => SyntaxKind::WhereKw,
		TokenKind::While => SyntaxKind::WhileKw,
		TokenKind::Err => SyntaxKind::Err,
		TokenKind::Whitespace => SyntaxKind::Whitespace,
		TokenKind::Comment => SyntaxKind::Comment,
		TokenKind::Match => SyntaxKind::MatchKw,
		TokenKind::Trait => SyntaxKind::TraitKw,
		TokenKind::Impl => SyntaxKind::ImplKw,
		TokenKind::Eof => unreachable!("eof not allowed in syntax"),
	}
}
