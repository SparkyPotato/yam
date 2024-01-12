use std::fmt::Debug;

use arena::{Arena, Ix};
use diagnostics::Diagnostic;
use ident::AbsPath;
pub use lang_item::LangItem;
use syntax::{ast as a, token::StringLit};
use text::Text;
use verde::{storage, Id, Tracked};

use crate::ast::{AstId, ErasedAstId};

pub mod ast;
pub mod ident;
pub mod lang_item;

#[storage]
pub struct Storage(
	AbsPath,
	Item,
	ItemDiagnostic,
	lang_item::LangItemMap,
	lang_item::build_lang_item_map,
);

pub type ItemDiagnostic = Diagnostic<ErasedAstId>;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Name {
	pub name: Text,
	pub id: AstId<a::Name>,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum AttrKind {
	LangItem(LangItem),
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Attr {
	pub kind: AttrKind,
	pub id: AstId<a::Attribute>,
}

#[derive(Tracked, Clone, PartialEq, Eq)]
pub struct Item {
	#[id]
	pub path: Id<AbsPath>,
	pub name: Name,
	pub attrs: Vec<Attr>,
	pub exprs: Arena<Expr>,
	pub types: Arena<Type>,
	pub locals: Arena<Local>,
	pub kind: ItemKind,
}

#[derive(Clone, PartialEq, Eq)]
pub enum ItemKind {
	Struct(Struct),
	Enum(Enum),
	Fn(Fn),
	TypeAlias(TypeAlias),
	Static(Static),
}

impl Debug for ItemKind {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			ItemKind::Struct(_) => write!(f, "struct"),
			ItemKind::Enum(_) => write!(f, "enum"),
			ItemKind::Fn(_) => write!(f, "fn"),
			ItemKind::TypeAlias(_) => write!(f, "type alias"),
			ItemKind::Static(_) => write!(f, "static"),
		}
	}
}

#[derive(Clone, PartialEq, Eq)]
pub struct Fn {
	pub abi: Option<Abi>,
	pub name: Name,
	pub params: Arena<Param>,
	pub ret: Option<Ix<Type>>,
	pub body: Option<Block>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Param {
	pub name: Name,
	pub ty: Ix<Type>,
	pub id: AstId<a::Param>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct AbiDecl {
	pub abi: &'static str,
	pub id: AstId<StringLit>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Abi {
	pub abi: Option<AbiDecl>,
	pub id: AstId<a::Abi>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Struct {
	pub name: Name,
	pub fields: Arena<Param>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Enum {
	pub name: Name,
	pub variants: Arena<Variant>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Variant(pub Name);

#[derive(Clone, PartialEq, Eq)]
pub struct TypeAlias {
	pub name: Name,
	pub ty: Ix<Type>,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Static {
	pub name: Name,
	pub ty: Ix<Type>,
	pub init: Ix<Expr>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Type {
	pub kind: TypeKind,
	pub id: AstId<a::Type>,
}

#[derive(Clone, PartialEq, Eq)]
pub enum TypeKind {
	Array(ArrayType),
	Fn(FnType),
	Infer,
	Struct(Id<AbsPath>),
	Enum(Id<AbsPath>),
	Alias(Id<AbsPath>),
	Ptr(PtrType),
}

impl Debug for TypeKind {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			TypeKind::Array(_) => write!(f, "array"),
			TypeKind::Fn(_) => write!(f, "fn"),
			TypeKind::Infer => write!(f, "<unknown>"),
			TypeKind::Struct(_) => write!(f, "struct"),
			TypeKind::Enum(_) => write!(f, "enum"),
			TypeKind::Alias(_) => write!(f, "alias"),
			TypeKind::Ptr(_) => write!(f, "pointer"),
		}
	}
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct ArrayType {
	pub ty: Ix<Type>,
	pub len: Ix<Expr>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct FnType {
	pub abi: Option<Abi>,
	pub params: Vec<Ix<Type>>,
	pub ret: Option<Ix<Type>>,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct PtrType {
	pub mutable: bool,
	pub ty: Ix<Type>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Expr {
	pub kind: ExprKind,
	pub id: Option<AstId<a::Expr>>,
}

#[derive(Clone, PartialEq, Eq)]
pub enum ExprKind {
	Continue,
	Array(ArrayExpr),
	Let(LetExpr),
	Block(Block),
	Infix(InfixExpr),
	Break(Option<Ix<Expr>>),
	Call(CallExpr),
	Struct(StructExpr),
	Cast(CastExpr),
	Field(FieldExpr),
	Index(IndexExpr),
	Literal(Literal),
	Loop(LoopExpr),
	Match(MatchExpr),
	Fn(Id<AbsPath>),
	Static(Id<AbsPath>),
	Local(Ix<Local>),
	Param(Ix<Param>),
	EnumVariant(VariantExpr),
	Ref(RefExpr),
	Prefix(PrefixExpr),
	Return(Option<Ix<Expr>>),
}

#[derive(Clone, PartialEq, Eq)]
pub struct ArrayExpr {
	pub elems: Vec<Ix<Expr>>,
	pub repeat: bool,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Local {
	pub decl: Name,
}

#[derive(Clone, PartialEq, Eq)]
pub struct LetExpr {
	pub name: Name,
	pub ty: Option<Ix<Type>>,
	pub init: Option<Ix<Expr>>,
	pub local: Ix<Local>,
}

#[derive(Clone, PartialEq, Eq, Default)]
pub struct Block {
	pub discard: Vec<Ix<Expr>>,
	pub value: Option<Ix<Expr>>,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct InfixExpr {
	pub lhs: Ix<Expr>,
	pub op: InfixOp,
	pub op_id: AstId<a::InfixOp>,
	pub rhs: Ix<Expr>,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum InfixOp {
	Or,
	And,
	Eq,
	NotEq,
	Lt,
	Leq,
	Gt,
	Geq,
	Add,
	Sub,
	Mul,
	Div,
	Mod,
	Shl,
	Shr,
	Xor,
	BitOr,
	BitAnd,
	Assign,
	AddAssign,
	SubAssign,
	MulAssign,
	DivAssign,
	ModAssign,
	ShlAssign,
	ShrAssign,
	XorAssign,
	BitOrAssign,
	BitAndAssign,
}

#[derive(Clone, PartialEq, Eq)]
pub struct CallExpr {
	pub callee: Ix<Expr>,
	pub args: Vec<Ix<Expr>>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct StructExpr {
	pub struct_: Id<AbsPath>,
	pub args: Vec<Ix<Expr>>,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct CastExpr {
	pub expr: Ix<Expr>,
	pub ty: Ix<Type>,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct FieldExpr {
	pub expr: Ix<Expr>,
	pub field: Name,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct IndexExpr {
	pub expr: Ix<Expr>,
	pub index: Ix<Expr>,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Literal {
	Bool(bool),
	Char(u8),
	Float(u64),
	Int(i64),
	String(&'static str),
}

#[derive(Clone, PartialEq, Eq)]
pub struct LoopExpr {
	pub body: Block,
}

#[derive(Clone, PartialEq, Eq)]
pub struct MatchExpr {
	pub expr: Ix<Expr>,
	pub arms: Vec<MatchArm>,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct MatchArm {
	pub value: Ix<Expr>,
	pub then: Ix<Expr>,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct VariantExpr {
	pub path: Id<AbsPath>,
	pub variant: Name,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct RefExpr {
	pub expr: Ix<Expr>,
	pub mutable: bool,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct PrefixExpr {
	pub op: PrefixOp,
	pub op_id: AstId<a::PrefixOp>,
	pub expr: Ix<Expr>,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum PrefixOp {
	Not,
	Neg,
	Deref,
}
