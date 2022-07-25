use std::collections::HashMap;

use diag::Span;
use name_resolve::resolved::{BinOp, Ident, InbuiltType, Lit, LocalRef, Pat, Path, Spanned, Spur, TyRef, UnOp, ValRef};

use crate::TypeId;

#[derive(Debug)]
pub struct Ctx {
	pub(crate) types: HashMap<TyRef, Ty>,
	pub(crate) globals: HashMap<ValRef, Val>,
}

#[derive(Debug)]
pub enum Ty {
	Inbuilt(InbuiltType),
	Struct(Struct),
}

#[derive(Debug)]
pub enum Val {
	Static(GlobalLet),
	Const(GlobalLet),
	Fn(Fn),
}

#[derive(Debug, Clone)]
pub struct GlobalLet {
	pub ty: Type,
	pub value: Expr,
	pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Struct {
	pub path: Path,
	pub fields: Vec<Field>,
	pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Fn {
	pub path: Path,
	pub args: Vec<Arg>,
	pub ret: Option<Box<Type>>,
	pub block: Block,
	pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Arg {
	pub is_const: bool,
	pub pat: Pat,
	pub ty: Type,
	pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Field {
	pub name: Ident,
	pub ty: Type,
	pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Ptr {
	pub mutability: bool,
	pub to: Box<Type>,
}

#[derive(Debug, Clone)]
pub enum Type {
	TyRef(TyRef),
	Type,
	Ptr(Ptr),
	Unresolved(TypeId),
	Err,
}

#[derive(Debug, Clone)]
pub struct Block {
	pub is_const: bool,
	pub stmts: Vec<Expr>,
	pub span: Span,
}

pub type Expr = Spanned<ExprData>;
#[derive(Debug, Clone)]
pub struct ExprData {
	pub kind: ExprKind,
	pub ty: Type,
}
#[derive(Debug, Clone)]
pub enum ExprKind {
	Lit(Lit),
	Block(Block),
	ValRef(ValRef),
	LocalRef(LocalRef),
	Let(Let),
	List(Vec<Expr>),
	Array(Array),
	Cast(Cast),
	Fn(Fn),
	MacroRef(Spur),
	Call(Call),
	Index(Index),
	Access(Access),
	Unary(Unary),
	Binary(Binary),
	Break(Option<Box<Expr>>),
	Continue(Option<Box<Expr>>),
	Return(Option<Box<Expr>>),
	If(If),
	Loop(Loop),
	Err,
}

#[derive(Debug, Clone)]
pub struct Let {
	pub pat: Pat,
	pub ty: Type,
	pub expr: Option<Box<Expr>>,
	pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Array {
	pub expr: Box<Expr>,
	pub count: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Cast {
	pub expr: Box<Expr>,
	pub ty: Box<Type>,
}

#[derive(Debug, Clone)]
pub struct Call {
	pub target: Box<Expr>,
	pub args: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct Index {
	pub target: Box<Expr>,
	pub index: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Access {
	pub target: Box<Expr>,
	pub field: Ident,
}

#[derive(Debug, Clone)]
pub struct Unary {
	pub op: UnOp,
	pub expr: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Binary {
	pub lhs: Box<Expr>,
	pub op: BinOp,
	pub rhs: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct If {
	pub cond: Box<Expr>,
	pub then: Block,
	pub else_: Option<Box<Expr>>,
}

#[derive(Debug, Clone)]
pub struct Loop {
	pub block: Block,
	pub while_: Option<Box<Expr>>,
}
