use diagnostics::FileSpan;
use logos::Logos;

#[derive(Clone, Copy, Default)]
pub struct Token {
	pub kind: TokenKind,
	pub span: FileSpan,
}

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, Hash, Logos)]
pub enum TokenKind {
	#[regex("true|false")]
	BoolLit,
	#[regex(r"'(\.|[^'\\])*'")]
	CharLit,
	#[regex(r"(\d*[.])?\d+")]
	FloatLit,
	#[regex(r"(\d+)|(0x[0-9a-fA-F]+)|(0b[01]+)", priority = 2)]
	IntLit,
	#[regex(r#""(\.|[^"\\])*""#)]
	StringLit,
	#[regex(r"(\p{XID_Start}\p{XID_Continue}*)|(_\p{XID_Continue}+)", priority = 2)]
	Ident,
	#[token("@")]
	At,
	#[token("(")]
	LParen,
	#[token("{")]
	LBrace,
	#[token("[")]
	LBracket,
	#[token(")")]
	RParen,
	#[token("}")]
	RBrace,
	#[token("]")]
	RBracket,
	#[token("=")]
	Eq,
	#[token(".")]
	Dot,
	#[token(":")]
	Colon,
	#[token(",")]
	Comma,
	#[token(";")]
	Semi,
	#[token("->")]
	Arrow,
	#[token("=>")]
	FatArrow,
	#[token("_")]
	Underscore,
	#[token("||")]
	PipePipe,
	#[token("&&")]
	AmpAmp,
	#[token("!")]
	Not,
	#[token("==")]
	EqEq,
	#[token("!=")]
	Neq,
	#[token("<")]
	Lt,
	#[token(">")]
	Gt,
	#[token("<=")]
	Leq,
	#[token(">=")]
	Geq,
	#[token("+")]
	Plus,
	#[token("-")]
	Minus,
	#[token("*")]
	Star,
	#[token("/")]
	Slash,
	#[token("%")]
	Percent,
	#[token("^")]
	Caret,
	#[token("&")]
	Amp,
	#[token("|")]
	Pipe,
	#[token("<<")]
	Shl,
	#[token(">>")]
	Shr,
	#[token("+=")]
	PlusEq,
	#[token("-=")]
	MinusEq,
	#[token("*=")]
	StarEq,
	#[token("/=")]
	SlashEq,
	#[token("%=")]
	PercentEq,
	#[token("^=")]
	CaretEq,
	#[token("&=")]
	AmpEq,
	#[token("|=")]
	PipeEq,
	#[token("<<=")]
	ShlEq,
	#[token(">>=")]
	ShrEq,
	#[regex("[ \t\n\r]+")]
	Whitespace,
	#[regex("//[^\n]*")]
	Comment,
	#[token("fn")]
	FnKw,
	#[token("let")]
	LetKw,
	#[token("if")]
	IfKw,
	#[token("else")]
	ElseKw,
	#[token("while")]
	WhileKw,
	#[token("for")]
	ForKw,
	#[token("loop")]
	LoopKw,
	#[token("in")]
	InKw,
	#[token("return")]
	ReturnKw,
	#[token("break")]
	BreakKw,
	#[token("continue")]
	ContinueKw,
	#[token("match")]
	MatchKw,
	#[token("struct")]
	StructKw,
	#[token("enum")]
	EnumKw,
	#[token("type")]
	TypeKw,
	#[token("pub")]
	PubKw,
	#[token("extern")]
	ExternKw,
	#[token("static")]
	StaticKw,
	#[token("import")]
	ImportKw,
	#[token("as")]
	AsKw,
	#[token("mut")]
	MutKw,
	Eof,
	#[error]
	#[default]
	Error,
}

#[macro_export]
macro_rules! T {
	(bool) => {
		$crate::token::TokenKind::BoolLit
	};
	(char) => {
		$crate::token::TokenKind::CharLit
	};
	(float) => {
		$crate::token::TokenKind::FloatLit
	};
	(int) => {
		$crate::token::TokenKind::IntLit
	};
	(string) => {
		$crate::token::TokenKind::StringLit
	};
	(ident) => {
		$crate::token::TokenKind::Ident
	};
	(op) => {
		$crate::token::TokenKind::Operator
	};
	(@) => {
		$crate::token::TokenKind::At
	};
	('(') => {
		$crate::token::TokenKind::LParen
	};
	(')') => {
		$crate::token::TokenKind::RParen
	};
	('{') => {
		$crate::token::TokenKind::LBrace
	};
	('}') => {
		$crate::token::TokenKind::RBrace
	};
	('[') => {
		$crate::token::TokenKind::LBracket
	};
	(']') => {
		$crate::token::TokenKind::RBracket
	};
	(:) => {
		$crate::token::TokenKind::Colon
	};
	(;) => {
		$crate::token::TokenKind::Semi
	};
	(,) => {
		$crate::token::TokenKind::Comma
	};
	(->) => {
		$crate::token::TokenKind::Arrow
	};
	(=>) => {
		$crate::token::TokenKind::FatArrow
	};
	(=) => {
		$crate::token::TokenKind::Eq
	};
	(.) => {
		$crate::token::TokenKind::Dot
	};
	(||) => {
		$crate::token::TokenKind::PipePipe
	};
	(&&) => {
		$crate::token::TokenKind::AmpAmp
	};
	(!) => {
		$crate::token::TokenKind::Not
	};
	(==) => {
		$crate::token::TokenKind::EqEq
	};
	(!=) => {
		$crate::token::TokenKind::Neq
	};
	(<) => {
		$crate::token::TokenKind::Lt
	};
	(>) => {
		$crate::token::TokenKind::Gt
	};
	(<=) => {
		$crate::token::TokenKind::Leq
	};
	(>=) => {
		$crate::token::TokenKind::Geq
	};
	(+) => {
		$crate::token::TokenKind::Plus
	};
	(-) => {
		$crate::token::TokenKind::Minus
	};
	(*) => {
		$crate::token::TokenKind::Star
	};
	(/) => {
		$crate::token::TokenKind::Slash
	};
	(%) => {
		$crate::token::TokenKind::Percent
	};
	(^) => {
		$crate::token::TokenKind::Caret
	};
	(&) => {
		$crate::token::TokenKind::Amp
	};
	(|) => {
		$crate::token::TokenKind::Pipe
	};
	(<<) => {
		$crate::token::TokenKind::Shl
	};
	(>>) => {
		$crate::token::TokenKind::Shr
	};
	(+=) => {
		$crate::token::TokenKind::PlusEq
	};
	(-=) => {
		$crate::token::TokenKind::MinusEq
	};
	(*=) => {
		$crate::token::TokenKind::StarEq
	};
	(/=) => {
		$crate::token::TokenKind::SlashEq
	};
	(%=) => {
		$crate::token::TokenKind::PercentEq
	};
	(^=) => {
		$crate::token::TokenKind::CaretEq
	};
	(&=) => {
		$crate::token::TokenKind::AmpEq
	};
	(|=) => {
		$crate::token::TokenKind::PipeEq
	};
	(<<=) => {
		$crate::token::TokenKind::ShlEq
	};
	(>>=) => {
		$crate::token::TokenKind::ShrEq
	};
	(_) => {
		$crate::token::TokenKind::Underscore
	};
	(err) => {
		$crate::token::TokenKind::Error
	};
	(ws) => {
		$crate::token::TokenKind::Whitespace
	};
	(comment) => {
		$crate::token::TokenKind::Comment
	};
	(fn) => {
		$crate::token::TokenKind::FnKw
	};
	(let) => {
		$crate::token::TokenKind::LetKw
	};
	(if) => {
		$crate::token::TokenKind::IfKw
	};
	(else) => {
		$crate::token::TokenKind::ElseKw
	};
	(while) => {
		$crate::token::TokenKind::WhileKw
	};
	(for) => {
		$crate::token::TokenKind::ForKw
	};
	(loop) => {
		$crate::token::TokenKind::LoopKw
	};
	(in) => {
		$crate::token::TokenKind::InKw
	};
	(return) => {
		$crate::token::TokenKind::ReturnKw
	};
	(break) => {
		$crate::token::TokenKind::BreakKw
	};
	(continue) => {
		$crate::token::TokenKind::ContinueKw
	};
	(match) => {
		$crate::token::TokenKind::MatchKw
	};
	(struct) => {
		$crate::token::TokenKind::StructKw
	};
	(enum) => {
		$crate::token::TokenKind::EnumKw
	};
	(type) => {
		$crate::token::TokenKind::TypeKw
	};
	(pub) => {
		$crate::token::TokenKind::PubKw
	};
	(extern) => {
		$crate::token::TokenKind::ExternKw
	};
	(static) => {
		$crate::token::TokenKind::StaticKw
	};
	(import) => {
		$crate::token::TokenKind::ImportKw
	};
	(as) => {
		$crate::token::TokenKind::AsKw
	};
	(mut) => {
		$crate::token::TokenKind::MutKw
	};
	(eof) => {
		$crate::token::TokenKind::Eof
	};
}
