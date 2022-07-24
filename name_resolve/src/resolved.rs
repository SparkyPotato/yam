use std::collections::HashMap;

use diag::Span;
use parse::{
	ast::{BinOp, Ident, Spanned, UnOp, Visibility},
	Spur,
};

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct TyRef(pub(crate) u32);
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct ValRef(pub(crate) u32);
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct LocalRef(pub(crate) u32);

pub type Path = Vec<Ident>;

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
pub enum InbuiltType {
	Bool,
	Float(u8),
	Int(u8),
	Uint(u8),
}

#[derive(Debug)]
pub enum Val {
	Static(GlobalLet),
	Const(GlobalLet),
	Fn(Fn),
}

#[derive(Debug, Clone)]
pub struct Struct {
	pub path: Path,
	pub fields: Vec<Field>,
	pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Field {
	pub visibility: Visibility,
	pub name: Ident,
	pub ty: TyExpr,
	pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Fn {
	pub path: Path,
	pub args: Vec<Arg>,
	pub ret: Option<Box<TyExpr>>,
	pub block: Block,
	pub span: Span,
}

pub type Pat = Spanned<PatKind>;
#[derive(Debug, Copy, Clone)]
pub enum PatKind {
	Binding(Binding),
}

#[derive(Debug, Copy, Clone)]
pub struct Binding {
	pub mutability: bool,
	pub binding: LocalRef,
}

#[derive(Debug, Clone)]
pub struct Arg {
	pub is_const: bool,
	pub pat: Pat,
	pub ty: TyExpr,
	pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Block {
	pub is_const: bool,
	pub stmts: Vec<Stmt>,
	pub span: Span,
}

pub type Stmt = Spanned<StmtKind>;
#[derive(Debug, Clone)]
pub enum StmtKind {
	Expr(ValExprKind),
	Semi(ValExprKind),
	Err,
}

pub type Expr = Spanned<ExprKind>;
#[derive(Debug, Clone)]
pub enum ExprKind {
	Val(ValExprKind),
	Ty(TyExprKind),
	Err,
}

pub type ValExpr = Spanned<ValExprKind>;
#[derive(Debug, Clone)]
pub enum ValExprKind {
	Lit(Lit),
	Block(Block),
	ValRef(ValRef),
	LocalRef(LocalRef),
	Let(Let),
	List(Vec<ValExpr>),
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
	While(While),
	For(For),
	Err,
}

pub type TyExpr = Spanned<TyExprKind>;
#[derive(Debug, Clone)]
pub enum TyExprKind {
	TyRef(TyRef),
	Type,
	TypeOf(Box<Expr>),
	Ptr(Ptr),
	Tuple(Vec<TyExpr>),
	Err,
}

#[derive(Debug, Clone)]
pub enum Lit {
	Bool(bool),
	Char(char),
	Float(f64),
	Int(i128),
	String(Spur),
}

#[derive(Debug, Clone)]
pub struct Let {
	pub pat: Pat,
	pub ty: Option<Box<TyExpr>>,
	pub expr: Option<Box<ValExpr>>,
	pub span: Span,
}

#[derive(Debug, Clone)]
pub struct GlobalLet {
	pub ty: Option<TyExpr>,
	pub expr: ValExpr,
	pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Array {
	pub expr: Box<ValExpr>,
	pub count: Box<ValExpr>,
}

#[derive(Debug, Clone)]
pub struct Cast {
	pub expr: Box<ValExpr>,
	pub ty: Box<TyExpr>,
}

#[derive(Debug, Clone)]
pub struct Ptr {
	pub mutability: bool,
	pub to: Box<TyExpr>,
}

#[derive(Debug, Clone)]
pub struct Call {
	pub target: Box<ValExpr>,
	pub args: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct Index {
	pub target: Box<ValExpr>,
	pub index: Box<ValExpr>,
}

#[derive(Debug, Clone)]
pub struct Access {
	pub target: Box<Expr>,
	pub field: Ident,
}

#[derive(Debug, Clone)]
pub struct Unary {
	pub op: UnOp,
	pub expr: Box<ValExpr>,
}

#[derive(Debug, Clone)]
pub struct Binary {
	pub lhs: Box<ValExpr>,
	pub op: BinOp,
	pub rhs: Box<ValExpr>,
}

#[derive(Debug, Clone)]
pub struct If {
	pub cond: Box<ValExpr>,
	pub then: Block,
	pub else_: Option<Box<Expr>>,
}

#[derive(Debug, Clone)]
pub struct Loop {
	pub block: Block,
	pub while_: Option<Box<ValExpr>>,
}

#[derive(Debug, Clone)]
pub struct While {
	pub cond: Box<ValExpr>,
	pub block: Block,
}

#[derive(Debug, Clone)]
pub struct For {
	pub pat: Pat,
	pub iter: Box<ValExpr>,
	pub block: Block,
}
