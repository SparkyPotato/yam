use std::fmt::{Debug, Display, Formatter};

use diag::Span;
use logos::Logos;

use crate::T;

#[derive(Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct Token {
	pub kind: TokenKind,
	pub span: Span,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Delim {
	Paren,
	Brace,
	Bracket,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Lit {
	Bool,
	Char,
	Float,
	Int,
	String,
}

#[derive(Copy, Clone, Default, PartialEq, Eq, Hash, Logos)]
pub enum TokenKind {
	#[regex("true|false", |_| Lit::Bool)]
	#[regex(r#"'(\.|[^'\\])*'"#, |_| Lit::Char)]
	#[regex(r"[+-]?(\d*[.])?\d+", |_| Lit::Float)]
	#[regex(r"[+-]?(\d+)|(0x[0-9a-f]+)|(0b[01]+)", |_| Lit::Int, priority = 2)]
	#[regex(r#""(\.|[^"\\])*""#, |_| Lit::String)]
	Lit(Lit),
	#[regex(r"\p{XID_Start}\p{XID_Continue}*")]
	Ident,
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
	Lt,
	#[token(">")]
	Gt,
	#[token("=")]
	Eq,
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
	#[token("&")]
	And,
	#[token("|")]
	Pipe,
	#[token("^")]
	Caret,
	#[token("!")]
	Exclaim,
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
	#[token("as")]
	As,
	#[token("break")]
	Break,
	#[token("const")]
	Const,
	#[token("continue")]
	Continue,
	#[token("else")]
	Else,
	#[token("enum")]
	Enum,
	#[token("extern")]
	Extern,
	#[token("fn")]
	Fn,
	#[token("for")]
	For,
	#[token("if")]
	If,
	#[token("in")]
	In,
	#[token("import")]
	Import,
	#[token("let")]
	Let,
	#[token("loop")]
	Loop,
	#[token("match")]
	Match,
	#[token("trait")]
	Trait,
	#[token("mod")]
	Mod,
	#[token("mut")]
	Mut,
	#[token("pub")]
	Pub,
	#[token("return")]
	Return,
	#[token("static")]
	Static,
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
	#[default]
	Err,
	#[regex("[ \t\n\r]+")]
	Whitespace,
	#[regex("//[^\n]*")]
	Comment,
	Eof,
}

impl Debug for TokenKind {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result { write!(f, "{}", self) }
}

impl Display for TokenKind {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(
			f,
			"{}",
			match self {
				T![lit(_)] => "<lit>",
				T![ident] => "<ident>",
				T![@] => "@",
				T!['('] => "(",
				T!['{'] => "{",
				T!['['] => "[",
				T![')'] => ")",
				T!['}'] => "}",
				T![']'] => "]",
				T![<] => "<",
				T![>] => ">",
				T![=] => "=",
				T![+] => "+",
				T![-] => "-",
				T![*] => "*",
				T![/] => "/",
				T![%] => "%",
				T![&] => "&",
				T![|] => "|",
				T![^] => "^",
				T![!] => "!",
				T![~] => "~",
				T![.] => ".",
				T![:] => ":",
				T![,] => ",",
				T![;] => ";",
				T![?] => "?",
				T![_] => "_",
				T![as] => "as",
				T![const] => "const",
				T![else] => "else",
				T![enum] => "enum",
				T![extern] => "extern",
				T![fn] => "fn",
				T![if] => "if",
				T![import] => "import",
				T![let] => "let",
				T![match] => "match",
				T![trait] => "trait",
				T![mod] => "mod",
				T![mut] => "mut",
				T![pub] => "pub",
				T![static] => "static",
				T![struct] => "struct",
				T![type] => "type",
				T![union] => "union",
				T![var] => "var",
				T![where] => "where",
				T![while] => "while",
				T![break] => "break",
				T![continue] => "continue",
				T![for] => "for",
				T![loop] => "loop",
				T![return] => "return",
				T![in] => "in",
				T![err] => "<error>",
				T![ws] => "<whitespace>",
				T![comment] => "<comment>",
				T![eof] => "<eof>",
			}
		)
	}
}

impl TokenKind {
	pub fn is_delim_kw(self) -> bool {
		matches!(
			self,
			T![break]
				| T![const] | T![continue]
				| T![else] | T![enum]
				| T![extern] | T![fn]
				| T![for] | T![if]
				| T![import] | T![let]
				| T![loop] | T![mod]
				| T![match] | T![pub]
				| T![return] | T![static]
				| T![type] | T![union]
				| T![var] | T![where]
				| T![while]
		)
	}
}

#[macro_export]
macro_rules! T {
	(lit(_)) => {
		$crate::token::TokenKind::Lit(_)
	};
	(lit(bool)) => {
		$crate::token::TokenKind::Lit($crate::token::Lit::Bool)
	};
	(lit(char)) => {
		$crate::token::TokenKind::Lit($crate::token::Lit::Char)
	};
	(lit(float)) => {
		$crate::token::TokenKind::Lit($crate::token::Lit::Float)
	};
	(lit(int)) => {
		$crate::token::TokenKind::Lit($crate::token::Lit::Int)
	};
	(lit(str)) => {
		$crate::token::TokenKind::Lit($crate::token::Lit::String)
	};
	(ident) => {
		$crate::token::TokenKind::Ident
	};
	(@) => {
		$crate::token::TokenKind::At
	};
	(ldelim: $delim:tt) => {
		$crate::token::TokenKind::LDelim($delim)
	};
	(rdelim: $delim:tt) => {
		$crate::token::TokenKind::RDelim($delim)
	};
	('(') => {
		$crate::token::TokenKind::LDelim($crate::token::Delim::Paren)
	};
	(')') => {
		$crate::token::TokenKind::RDelim($crate::token::Delim::Paren)
	};
	('{') => {
		$crate::token::TokenKind::LDelim($crate::token::Delim::Brace)
	};
	('}') => {
		$crate::token::TokenKind::RDelim($crate::token::Delim::Brace)
	};
	('[') => {
		$crate::token::TokenKind::LDelim($crate::token::Delim::Bracket)
	};
	(']') => {
		$crate::token::TokenKind::RDelim($crate::token::Delim::Bracket)
	};
	(<) => {
		$crate::token::TokenKind::Lt
	};
	(>) => {
		$crate::token::TokenKind::Gt
	};
	(=) => {
		$crate::token::TokenKind::Eq
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
	(&) => {
		$crate::token::TokenKind::And
	};
	(|) => {
		$crate::token::TokenKind::Pipe
	};
	(^) => {
		$crate::token::TokenKind::Caret
	};
	(!) => {
		$crate::token::TokenKind::Exclaim
	};
	(~) => {
		$crate::token::TokenKind::Tilde
	};
	(.) => {
		$crate::token::TokenKind::Dot
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
	(?) => {
		$crate::token::TokenKind::Question
	};
	(_) => {
		$crate::token::TokenKind::Underscore
	};
	(as) => {
		$crate::token::TokenKind::As
	};
	(const) => {
		$crate::token::TokenKind::Const
	};
	(else) => {
		$crate::token::TokenKind::Else
	};
	(enum) => {
		$crate::token::TokenKind::Enum
	};
	(extern) => {
		$crate::token::TokenKind::Extern
	};
	(fn) => {
		$crate::token::TokenKind::Fn
	};
	(if) => {
		$crate::token::TokenKind::If
	};
	(import) => {
		$crate::token::TokenKind::Import
	};
	(let) => {
		$crate::token::TokenKind::Let
	};
	(mod) => {
		$crate::token::TokenKind::Mod
	};
	(mut) => {
		$crate::token::TokenKind::Mut
	};
	(pub) => {
		$crate::token::TokenKind::Pub
	};
	(static) => {
		$crate::token::TokenKind::Static
	};
	(struct) => {
		$crate::token::TokenKind::Struct
	};
	(type) => {
		$crate::token::TokenKind::Type
	};
	(union) => {
		$crate::token::TokenKind::Union
	};
	(var) => {
		$crate::token::TokenKind::Var
	};
	(where) => {
		$crate::token::TokenKind::Where
	};
	(while) => {
		$crate::token::TokenKind::While
	};
	(break) => {
		$crate::token::TokenKind::Break
	};
	(continue) => {
		$crate::token::TokenKind::Continue
	};
	(for) => {
		$crate::token::TokenKind::For
	};
	(match) => {
		$crate::token::TokenKind::Match
	};
	(trait) => {
		$crate::token::TokenKind::Trait
	};
	(loop) => {
		$crate::token::TokenKind::Loop
	};
	(return) => {
		$crate::token::TokenKind::Return
	};
	(in) => {
		$crate::token::TokenKind::In
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
