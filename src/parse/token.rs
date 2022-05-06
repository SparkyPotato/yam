use std::fmt::{Display, Formatter};

use logos::Logos;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Delim {
	Paren,
	Brace,
	Bracket,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Logos)]
pub enum TokenKind {
	#[regex("true|false")]
	BoolLiteral,
	#[regex(r#"'(\.|[^'\\])*'"#)]
	CharLiteral,
	#[regex(r"[+-]?(\d*[.])?\d+")]
	FloatLiteral,
	#[regex(r"\p{XID_Start}\p{XID_Continue}*")]
	Ident,
	#[regex(r"[+-]?(\d+)|(0x[0-9a-f]+)|(0b[01]+)", priority = 2)]
	IntLiteral,
	#[regex(r#""(\.|[^"\\])*""#)]
	StringLiteral,
	#[token("@")]
	At,
	#[token("(", |_| Delim::Paren)]
	#[token("{", |_| Delim::Brace)]
	#[token("[", |_| Delim::Bracket)]
	LDelim(Delim),
	#[token(")", |_| Delim::Paren)]
	#[token("}", |_| Delim::Brace)]
	#[token("]", |_| Delim::Bracket)]
	RDelim(Delim),
	#[token("<")]
	Le,
	#[token(">")]
	Ge,
	#[token("<=")]
	Leq,
	#[token(">=")]
	Geq,
	#[token("==")]
	Eq,
	#[token("!=")]
	Neq,
	#[token("=")]
	Assign,
	#[token("+")]
	Plus,
	#[token("+=")]
	PlusEq,
	#[token("-")]
	Minus,
	#[token("-=")]
	MinusEq,
	#[token("*")]
	Mul,
	#[token("*=")]
	MulEq,
	#[token("/")]
	Div,
	#[token("/=")]
	DivEq,
	#[token("%")]
	Percent,
	#[token("%=")]
	PercentEq,
	#[token("&")]
	BitAnd,
	#[token("&=")]
	BitAndEq,
	#[token("&&")]
	And,
	#[token("|")]
	BitOr,
	#[token("|=")]
	BitOrEq,
	#[token("||")]
	Or,
	#[token("^")]
	Caret,
	#[token("!")]
	Not,
	#[token("~")]
	Tilde,
	#[token(".")]
	Dot,
	#[token(":")]
	Colon,
	#[token(",")]
	Comma,
	#[token(";")]
	Semi,
	#[token("?")]
	Question,
	#[token("_")]
	Underscore,
	#[token("->")]
	Arrow,
	#[token("=>")]
	FatArrow,
	#[token("const")]
	Const,
	#[token("enum")]
	Enum,
	#[token("fn")]
	Fn,
	#[token("if")]
	If,
	#[token("let")]
	Let,
	#[token("mod")]
	Mod,
	#[token("pub")]
	Pub,
	#[token("struct")]
	Struct,
	#[token("type")]
	Type,
	#[token("union")]
	Union,
	#[token("var")]
	Var,
	#[token("where")]
	Where,
	#[token("while")]
	While,
	#[error]
	#[regex("([ \t\n\r]+)|(//.*)", logos::skip)]
	Error,
}

impl Display for TokenKind {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(
			f,
			"{}",
			match self {
				TokenKind::BoolLiteral => "<lit>",
				TokenKind::CharLiteral => "<lit>",
				TokenKind::FloatLiteral => "<lit>",
				TokenKind::Ident => "<ident>",
				TokenKind::IntLiteral => "<lit>",
				TokenKind::StringLiteral => "<lit>",
				TokenKind::At => "@",
				TokenKind::LDelim(Delim::Paren) => "(",
				TokenKind::LDelim(Delim::Brace) => "{",
				TokenKind::LDelim(Delim::Bracket) => "[",
				TokenKind::RDelim(Delim::Paren) => ")",
				TokenKind::RDelim(Delim::Brace) => "}",
				TokenKind::RDelim(Delim::Bracket) => "]",
				TokenKind::Le => "<",
				TokenKind::Ge => ">",
				TokenKind::Leq => "<=",
				TokenKind::Geq => ">=",
				TokenKind::Eq => "==",
				TokenKind::Neq => "!=",
				TokenKind::Assign => "=",
				TokenKind::Plus => "+",
				TokenKind::PlusEq => "+=",
				TokenKind::Minus => "-",
				TokenKind::MinusEq => "-=",
				TokenKind::Mul => "*",
				TokenKind::MulEq => "*=",
				TokenKind::Div => "/",
				TokenKind::DivEq => "/=",
				TokenKind::Percent => "%",
				TokenKind::PercentEq => "%=",
				TokenKind::BitAnd => "&",
				TokenKind::BitAndEq => "&=",
				TokenKind::And => "&&",
				TokenKind::BitOr => "|",
				TokenKind::BitOrEq => "|=",
				TokenKind::Or => "||",
				TokenKind::Caret => "^",
				TokenKind::Not => "!",
				TokenKind::Tilde => "~",
				TokenKind::Dot => ".",
				TokenKind::Colon => ":",
				TokenKind::Comma => ",",
				TokenKind::Semi => ";",
				TokenKind::Question => "?",
				TokenKind::Underscore => "_",
				TokenKind::Arrow => "->",
				TokenKind::FatArrow => "=>",
				TokenKind::Const => "const",
				TokenKind::Enum => "enum",
				TokenKind::Fn => "fn",
				TokenKind::If => "if",
				TokenKind::Let => "let",
				TokenKind::Mod => "mod",
				TokenKind::Pub => "pub",
				TokenKind::Struct => "struct",
				TokenKind::Type => "type",
				TokenKind::Union => "union",
				TokenKind::Var => "var",
				TokenKind::Where => "where",
				TokenKind::While => "while",
				TokenKind::Error => "<error>",
			}
		)
	}
}
