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
	#[token("*")]
	Star,
	#[regex(r#"[\p{Sm}\p{So}\p{Sk}\p{Pc}\p{Pd}\p{Po}]+"#)]
	Operator,
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
	#[token("const")]
	ConstKw,
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
	(=) => {
		$crate::token::TokenKind::Eq
	};
	(*) => {
		$crate::token::TokenKind::Star
	};
	(.) => {
		$crate::token::TokenKind::Dot
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
	(const) => {
		$crate::token::TokenKind::ConstKw
	};
	(mut) => {
		$crate::token::TokenKind::MutKw
	};
	(eof) => {
		$crate::token::TokenKind::Eof
	};
}
