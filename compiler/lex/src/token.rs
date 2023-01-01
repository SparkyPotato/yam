use logos::Logos;

#[derive(Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct FileSpan {
	pub start: u32,
	pub end: u32,
}

#[derive(Clone, Copy, Default)]
pub struct Token {
	pub kind: TokenKind,
	pub span: FileSpan,
}

#[derive(Copy, Clone, Default, PartialEq, Eq, Hash, Logos, Debug)]
pub enum TokenKind {
	#[regex("true|false")]
	BoolLit,
	#[regex(r#"'(\.|[^'\\])*'"#)]
	CharLit,
	#[regex(r"[+-]?(\d*[.])?\d+")]
	FloatLit,
	#[regex(r"[+-]?(\d+)|(0x[0-9a-f]+)|(0b[01]+)", priority = 2)]
	IntLit,
	#[regex(r#""(\.|[^"\\])*""#)]
	StringLit,
	#[regex(r"\p{XID_Start}\p{XID_Continue}*", priority = 2)]
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
	#[regex(r#"[\p{Sm}\p{So}\p{Sk}\p{Pc}\p{Pd}\p{Po}]+"#)]
	Operator,
	#[regex("[ \t\n\r]+")]
	Whitespace,
	#[regex("//[^\n]*")]
	Comment,
	Eof,
	#[error]
	#[default]
	Err,
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
	(_) => {
		$crate::token::TokenKind::Underscore
	};
	(err) => {
		$crate::token::TokenKind::Err
	};
	(ws) => {
		$crate::token::TokenKind::Whitespace
	};
	(comment) => {
		$crate::token::TokenKind::Comment
	};
	(eof) => {
		$crate::token::TokenKind::Eof
	};
}
