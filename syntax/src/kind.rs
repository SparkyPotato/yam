#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u16)]
pub enum SyntaxKind {
	BoolLit,
	CharLit,
	FloatLit,
	IntLit,
	StringLit,
	Ident,
	At,
	LParen,
	LBrace,
	LBracket,
	RParen,
	RBrace,
	RBracket,
	Lt,
	Gt,
	Eq,
	Plus,
	Minus,
	Star,
	Slash,
	Percent,
	And,
	Pipe,
	Caret,
	Exclaim,
	Tilde,
	Dot,
	Colon,
	Comma,
	Semi,
	Question,
	Underscore,
	AsKw,
	BreakKw,
	ConstKw,
	ContinueKw,
	ElseKw,
	EnumKw,
	ExternKw,
	FnKw,
	ForKw,
	IfKw,
	InKw,
	ImportKw,
	LetKw,
	LoopKw,
	ModKw,
	MutKw,
	PubKw,
	ReturnKw,
	StaticKw,
	StructKw,
	TypeKw,
	UnionKw,
	VarKw,
	WhereKw,
	WhileKw,
	Err,
	Whitespace,
	Comment,

	// Composite nodes
	Item,
	Attributes,
	Attribute,
	Visibility,
	TokenTree,
	Struct,
	Generics,
	Generic,
	GenericBound,
	File, // Always keep last
}

impl From<SyntaxKind> for cstree::SyntaxKind {
	fn from(v: SyntaxKind) -> Self { Self(v as _) }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Lang;

impl cstree::Language for Lang {
	type Kind = SyntaxKind;

	fn kind_from_raw(raw: cstree::SyntaxKind) -> Self::Kind {
		assert!(raw.0 <= SyntaxKind::File as _);
		unsafe { std::mem::transmute(raw.0) }
	}

	fn kind_to_raw(kind: Self::Kind) -> cstree::SyntaxKind { kind.into() }
}