#![allow(clippy::all)]
// This file is generated by build.rs
// Do not edit

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, cstree :: Syntax)]
#[repr(u32)]
pub enum SyntaxKind {
	/// Terminal tokens
	Dot,
	Ident,
	Semi,
	At,
	FnKw,
	LParen,
	Comma,
	RParen,
	Colon,
	Arrow,
	PubKw,
	ExternKw,
	StringLit,
	StructKw,
	LBrace,
	RBrace,
	EnumKw,
	TypeKw,
	Eq,
	StaticKw,
	AsKw,
	ImportKw,
	LBracket,
	RBracket,
	Underscore,
	Star,
	MutKw,
	ContinueKw,
	LetKw,
	PipePipe,
	AmpAmp,
	EqEq,
	Neq,
	Leq,
	Geq,
	Lt,
	Gt,
	Plus,
	Minus,
	Slash,
	Percent,
	Shl,
	Shr,
	Caret,
	Pipe,
	Amp,
	PlusEq,
	SlashEq,
	StarEq,
	PercentEq,
	ShrEq,
	ShlEq,
	MinusEq,
	PipeEq,
	AmpEq,
	CaretEq,
	BreakKw,
	ForKw,
	InKw,
	IfKw,
	ElseKw,
	BoolLit,
	CharLit,
	FloatLit,
	IntLit,
	LoopKw,
	WhileKw,
	MatchKw,
	FatArrow,
	Not,
	ReturnKw,
	Whitespace,
	Comment,
	Error,
	/// Non-terminal nodes
	Path,
	Name,
	File,
	Item,
	TokenTree,
	Attribute,
	Visibility,
	Fn,
	Struct,
	Enum,
	TypeAlias,
	Static,
	Import,
	Abi,
	ParamList,
	RetTy,
	Block,
	Param,
	VariantList,
	Rename,
	ListImport,
	RenameImport,
	ImportTreeList,
	ArrayType,
	FnType,
	InferType,
	PathType,
	PtrType,
	TyParamList,
	SemiExpr,
	ArrayExpr,
	InfixExpr,
	BreakExpr,
	CallExpr,
	CastExpr,
	FieldExpr,
	ForExpr,
	IfExpr,
	IndexExpr,
	LoopExpr,
	MatchExpr,
	ParenExpr,
	NameExpr,
	PrefixExpr,
	RefExpr,
	ReturnExpr,
	WhileExpr,
	LetExpr,
	ArrayList,
	ArrayRepeat,
	ArgList,
	MatchArm,
	#[doc(hidden)]
	Eof,
}

impl std::fmt::Display for SyntaxKind {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Dot => write!(f, "`.`"),
			Self::Ident => write!(f, "identifier"),
			Self::Semi => write!(f, "`;`"),
			Self::At => write!(f, "`@`"),
			Self::FnKw => write!(f, "`fn`"),
			Self::LParen => write!(f, "`(`"),
			Self::Comma => write!(f, "`,`"),
			Self::RParen => write!(f, "`)`"),
			Self::Colon => write!(f, "`:`"),
			Self::Arrow => write!(f, "`->`"),
			Self::PubKw => write!(f, "`pub`"),
			Self::ExternKw => write!(f, "`extern`"),
			Self::StringLit => write!(f, "string"),
			Self::StructKw => write!(f, "`struct`"),
			Self::LBrace => write!(f, "`{{`"),
			Self::RBrace => write!(f, "`}}`"),
			Self::EnumKw => write!(f, "`enum`"),
			Self::TypeKw => write!(f, "`type`"),
			Self::Eq => write!(f, "`=`"),
			Self::StaticKw => write!(f, "`static`"),
			Self::AsKw => write!(f, "`as`"),
			Self::ImportKw => write!(f, "`import`"),
			Self::LBracket => write!(f, "`[`"),
			Self::RBracket => write!(f, "`]`"),
			Self::Underscore => write!(f, "`_`"),
			Self::Star => write!(f, "`*`"),
			Self::MutKw => write!(f, "`mut`"),
			Self::ContinueKw => write!(f, "`continue`"),
			Self::LetKw => write!(f, "`let`"),
			Self::PipePipe => write!(f, "`||`"),
			Self::AmpAmp => write!(f, "`&&`"),
			Self::EqEq => write!(f, "`==`"),
			Self::Neq => write!(f, "`!=`"),
			Self::Leq => write!(f, "`<=`"),
			Self::Geq => write!(f, "`>=`"),
			Self::Lt => write!(f, "`<`"),
			Self::Gt => write!(f, "`>`"),
			Self::Plus => write!(f, "`+`"),
			Self::Minus => write!(f, "`-`"),
			Self::Slash => write!(f, "`/`"),
			Self::Percent => write!(f, "`%`"),
			Self::Shl => write!(f, "`<<`"),
			Self::Shr => write!(f, "`>>`"),
			Self::Caret => write!(f, "`^`"),
			Self::Pipe => write!(f, "`|`"),
			Self::Amp => write!(f, "`&`"),
			Self::PlusEq => write!(f, "`+=`"),
			Self::SlashEq => write!(f, "`/=`"),
			Self::StarEq => write!(f, "`*=`"),
			Self::PercentEq => write!(f, "`%=`"),
			Self::ShrEq => write!(f, "`>>=`"),
			Self::ShlEq => write!(f, "`<<=`"),
			Self::MinusEq => write!(f, "`-=`"),
			Self::PipeEq => write!(f, "`|=`"),
			Self::AmpEq => write!(f, "`&=`"),
			Self::CaretEq => write!(f, "`^=`"),
			Self::BreakKw => write!(f, "`break`"),
			Self::ForKw => write!(f, "`for`"),
			Self::InKw => write!(f, "`in`"),
			Self::IfKw => write!(f, "`if`"),
			Self::ElseKw => write!(f, "`else`"),
			Self::BoolLit => write!(f, "boolean"),
			Self::CharLit => write!(f, "character"),
			Self::FloatLit => write!(f, "floating-point number"),
			Self::IntLit => write!(f, "integer"),
			Self::LoopKw => write!(f, "`loop`"),
			Self::WhileKw => write!(f, "`while`"),
			Self::MatchKw => write!(f, "`match`"),
			Self::FatArrow => write!(f, "`=>`"),
			Self::Not => write!(f, "`!`"),
			Self::ReturnKw => write!(f, "`return`"),
			Self::Whitespace => write!(f, "whitespace"),
			Self::Comment => write!(f, "comment"),
			Self::Error => write!(f, "error"),
			Self::Path => write!(f, "Path"),
			Self::Name => write!(f, "Name"),
			Self::File => write!(f, "File"),
			Self::Item => write!(f, "Item"),
			Self::TokenTree => write!(f, "TokenTree"),
			Self::Attribute => write!(f, "Attribute"),
			Self::Visibility => write!(f, "Visibility"),
			Self::Fn => write!(f, "Fn"),
			Self::Struct => write!(f, "Struct"),
			Self::Enum => write!(f, "Enum"),
			Self::TypeAlias => write!(f, "TypeAlias"),
			Self::Static => write!(f, "Static"),
			Self::Import => write!(f, "Import"),
			Self::Abi => write!(f, "Abi"),
			Self::ParamList => write!(f, "ParamList"),
			Self::RetTy => write!(f, "RetTy"),
			Self::Block => write!(f, "Block"),
			Self::Param => write!(f, "Param"),
			Self::VariantList => write!(f, "VariantList"),
			Self::Rename => write!(f, "Rename"),
			Self::ListImport => write!(f, "ListImport"),
			Self::RenameImport => write!(f, "RenameImport"),
			Self::ImportTreeList => write!(f, "ImportTreeList"),
			Self::ArrayType => write!(f, "ArrayType"),
			Self::FnType => write!(f, "FnType"),
			Self::InferType => write!(f, "InferType"),
			Self::PathType => write!(f, "PathType"),
			Self::PtrType => write!(f, "PtrType"),
			Self::TyParamList => write!(f, "TyParamList"),
			Self::SemiExpr => write!(f, "SemiExpr"),
			Self::ArrayExpr => write!(f, "ArrayExpr"),
			Self::InfixExpr => write!(f, "InfixExpr"),
			Self::BreakExpr => write!(f, "BreakExpr"),
			Self::CallExpr => write!(f, "CallExpr"),
			Self::CastExpr => write!(f, "CastExpr"),
			Self::FieldExpr => write!(f, "FieldExpr"),
			Self::ForExpr => write!(f, "ForExpr"),
			Self::IfExpr => write!(f, "IfExpr"),
			Self::IndexExpr => write!(f, "IndexExpr"),
			Self::LoopExpr => write!(f, "LoopExpr"),
			Self::MatchExpr => write!(f, "MatchExpr"),
			Self::ParenExpr => write!(f, "ParenExpr"),
			Self::NameExpr => write!(f, "NameExpr"),
			Self::PrefixExpr => write!(f, "PrefixExpr"),
			Self::RefExpr => write!(f, "RefExpr"),
			Self::ReturnExpr => write!(f, "ReturnExpr"),
			Self::WhileExpr => write!(f, "WhileExpr"),
			Self::LetExpr => write!(f, "LetExpr"),
			Self::ArrayList => write!(f, "ArrayList"),
			Self::ArrayRepeat => write!(f, "ArrayRepeat"),
			Self::ArgList => write!(f, "ArgList"),
			Self::MatchArm => write!(f, "MatchArm"),
			Self::Eof => write!(f, "<eof>"),
		}
	}
}

impl From<lex::token::TokenKind> for SyntaxKind {
	fn from(kind: lex::token::TokenKind) -> Self {
		match kind {
			lex::token::TokenKind::Dot => Self::Dot,
			lex::token::TokenKind::Ident => Self::Ident,
			lex::token::TokenKind::Semi => Self::Semi,
			lex::token::TokenKind::At => Self::At,
			lex::token::TokenKind::FnKw => Self::FnKw,
			lex::token::TokenKind::LParen => Self::LParen,
			lex::token::TokenKind::Comma => Self::Comma,
			lex::token::TokenKind::RParen => Self::RParen,
			lex::token::TokenKind::Colon => Self::Colon,
			lex::token::TokenKind::Arrow => Self::Arrow,
			lex::token::TokenKind::PubKw => Self::PubKw,
			lex::token::TokenKind::ExternKw => Self::ExternKw,
			lex::token::TokenKind::StringLit => Self::StringLit,
			lex::token::TokenKind::StructKw => Self::StructKw,
			lex::token::TokenKind::LBrace => Self::LBrace,
			lex::token::TokenKind::RBrace => Self::RBrace,
			lex::token::TokenKind::EnumKw => Self::EnumKw,
			lex::token::TokenKind::TypeKw => Self::TypeKw,
			lex::token::TokenKind::Eq => Self::Eq,
			lex::token::TokenKind::StaticKw => Self::StaticKw,
			lex::token::TokenKind::AsKw => Self::AsKw,
			lex::token::TokenKind::ImportKw => Self::ImportKw,
			lex::token::TokenKind::LBracket => Self::LBracket,
			lex::token::TokenKind::RBracket => Self::RBracket,
			lex::token::TokenKind::Underscore => Self::Underscore,
			lex::token::TokenKind::Star => Self::Star,
			lex::token::TokenKind::MutKw => Self::MutKw,
			lex::token::TokenKind::ContinueKw => Self::ContinueKw,
			lex::token::TokenKind::LetKw => Self::LetKw,
			lex::token::TokenKind::PipePipe => Self::PipePipe,
			lex::token::TokenKind::AmpAmp => Self::AmpAmp,
			lex::token::TokenKind::EqEq => Self::EqEq,
			lex::token::TokenKind::Neq => Self::Neq,
			lex::token::TokenKind::Leq => Self::Leq,
			lex::token::TokenKind::Geq => Self::Geq,
			lex::token::TokenKind::Lt => Self::Lt,
			lex::token::TokenKind::Gt => Self::Gt,
			lex::token::TokenKind::Plus => Self::Plus,
			lex::token::TokenKind::Minus => Self::Minus,
			lex::token::TokenKind::Slash => Self::Slash,
			lex::token::TokenKind::Percent => Self::Percent,
			lex::token::TokenKind::Shl => Self::Shl,
			lex::token::TokenKind::Shr => Self::Shr,
			lex::token::TokenKind::Caret => Self::Caret,
			lex::token::TokenKind::Pipe => Self::Pipe,
			lex::token::TokenKind::Amp => Self::Amp,
			lex::token::TokenKind::PlusEq => Self::PlusEq,
			lex::token::TokenKind::SlashEq => Self::SlashEq,
			lex::token::TokenKind::StarEq => Self::StarEq,
			lex::token::TokenKind::PercentEq => Self::PercentEq,
			lex::token::TokenKind::ShrEq => Self::ShrEq,
			lex::token::TokenKind::ShlEq => Self::ShlEq,
			lex::token::TokenKind::MinusEq => Self::MinusEq,
			lex::token::TokenKind::PipeEq => Self::PipeEq,
			lex::token::TokenKind::AmpEq => Self::AmpEq,
			lex::token::TokenKind::CaretEq => Self::CaretEq,
			lex::token::TokenKind::BreakKw => Self::BreakKw,
			lex::token::TokenKind::ForKw => Self::ForKw,
			lex::token::TokenKind::InKw => Self::InKw,
			lex::token::TokenKind::IfKw => Self::IfKw,
			lex::token::TokenKind::ElseKw => Self::ElseKw,
			lex::token::TokenKind::BoolLit => Self::BoolLit,
			lex::token::TokenKind::CharLit => Self::CharLit,
			lex::token::TokenKind::FloatLit => Self::FloatLit,
			lex::token::TokenKind::IntLit => Self::IntLit,
			lex::token::TokenKind::LoopKw => Self::LoopKw,
			lex::token::TokenKind::WhileKw => Self::WhileKw,
			lex::token::TokenKind::MatchKw => Self::MatchKw,
			lex::token::TokenKind::FatArrow => Self::FatArrow,
			lex::token::TokenKind::Not => Self::Not,
			lex::token::TokenKind::ReturnKw => Self::ReturnKw,
			lex::token::TokenKind::Whitespace => Self::Whitespace,
			lex::token::TokenKind::Comment => Self::Comment,
			lex::token::TokenKind::Error => Self::Error,
			lex::token::TokenKind::Eof => Self::Eof,
		}
	}
}
