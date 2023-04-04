mod ast;

use arena::{Arena, Ix};
pub use ast::AstMap;
use syntax::{
	ast as a,
	token::{Ident, StringLit},
};
use text::Text;
use verde::{storage, Id, Interned, Tracked};

use crate::ast::AstId;

#[storage]
pub struct Storage(RawPath, RawPathInner, Value, TypeDecl);

#[derive(Interned, Copy, Clone, PartialEq, Eq, Hash)]
pub struct RawPath {
	pub inner: RawPathInner,
	pub ns: Namespace,
}

#[derive(Interned, Copy, Clone, PartialEq, Eq, Hash)]
pub struct RawPathInner {
	pub prec: Option<Id<Self>>,
	pub ident: Text,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum Namespace {
	Type,
	Value,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Attr {}

#[derive(Tracked, Clone, PartialEq, Eq)]
pub struct Value {
	#[id]
	pub path: Id<RawPath>,
	pub kind: ValueKind,
	pub common: ItemCommon,
}

#[derive(Clone, PartialEq, Eq)]
pub enum ValueKind {
	Fn(Fn),
	Static(Static),
}

#[derive(Tracked, Clone, PartialEq, Eq)]
pub struct TypeDecl {
	#[id]
	pub path: Id<RawPath>,
	pub kind: TypeDeclKind,
	pub common: ItemCommon,
}

#[derive(Clone, PartialEq, Eq)]
pub enum TypeDeclKind {
	Struct(Struct),
	Enum(Enum),
}

#[derive(Clone, PartialEq, Eq)]
pub struct ItemCommon {
	pub attrs: Vec<Attr>,
	pub exprs: Arena<Expr>,
	pub types: Arena<Type>,
	pub locals: Arena<Local>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Fn {
	pub abi: Option<Abi>,
	pub name: AstId<Ident>,
	pub params: Vec<Param>,
	pub ret: Option<Ix<Type>>,
	pub body: Option<Block>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Param {
	pub name: AstId<Ident>,
	pub ty: Ix<Type>,
	pub id: AstId<a::Param>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct AbiDecl {
	pub abi: Text,
	pub id: AstId<StringLit>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Abi {
	pub abi: Option<AbiDecl>,
	pub id: AstId<a::Abi>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Struct {
	pub name: AstId<Ident>,
	pub fields: Vec<Param>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Enum {
	pub variants: Vec<AstId<Ident>>,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Static {
	pub name: AstId<Ident>,
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
	Path(Id<TypeDecl>),
	Ptr(PtrType),
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
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct PtrType {
	pub mutable: bool,
	pub ty: Ix<Type>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Expr {
	pub kind: ExprKind,
	pub id: AstId<a::Expr>,
}

#[derive(Clone, PartialEq, Eq)]
pub enum ExprKind {
	Array(ArrayExpr),
	Let(LetExpr),
	Block(Block),
	Infix(InfixExpr),
	Break(BreakExpr),
	Call(CallExpr),
	Cast(CastExpr),
	Field(FieldExpr),
	Index(IndexExpr),
	Literal(Literal),
	Loop(LoopExpr),
	Match(MatchExpr),
	Path(Id<Value>),
	Local(u32),
	Ref(RefExpr),
	Prefix(PrefixExpr),
	Return(ReturnExpr),
}

#[derive(Clone, PartialEq, Eq)]
pub struct ArrayExpr {
	pub elems: Vec<Ix<Expr>>,
	pub repeat: bool,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Local;

#[derive(Clone, PartialEq, Eq)]
pub struct LetExpr {
	pub name: AstId<Ident>,
	pub ty: Option<Ix<Type>>,
	pub init: Option<Ix<Expr>>,
	pub local: Ix<Local>,
	pub succ: Block,
}

#[derive(Clone, PartialEq, Eq)]
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

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum InfixOp {
	Or,
	And,
	Eq,
	NotEq,
	Lt,
	LtEq,
	Gt,
	GtEq,
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

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct BreakExpr {
	pub with: Option<Ix<Expr>>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct CallExpr {
	pub callee: Ix<Expr>,
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
	pub field: AstId<Ident>,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct IndexExpr {
	pub expr: Ix<Expr>,
	pub index: Ix<Expr>,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Literal {
	pub kind: LiteralKind,
	pub value: Text,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum LiteralKind {
	Bool,
	Char,
	Float,
	Int,
	String,
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

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum PrefixOp {
	Not,
	Neg,
	Deref,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct ReturnExpr {
	pub with: Option<Ix<Expr>>,
}
