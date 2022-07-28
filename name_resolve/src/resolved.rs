use diag::Span;
pub use parse::{
	ast::{BinOp, Ident, Spanned, UnOp, Visibility},
	Spur,
};

use crate::ctx::{LocalRef, ValRef};

pub type Path = Vec<Ident>;

#[derive(Debug)]
pub struct ValDef {
	pub path: Path,
	pub kind: ValDefKind,
	pub span: Span,
}

#[derive(Debug)]
pub enum ValDefKind {
	Static(GlobalLet),
	Const(GlobalLet),
	Fn(Fn),
	Struct(Struct),
}

#[derive(Debug, Clone)]
pub struct Struct {
	pub fields: Vec<Field>,
}

#[derive(Debug, Clone)]
pub struct Field {
	pub visibility: Visibility,
	pub name: Ident,
	pub ty: Expr,
	pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Fn {
	pub args: Vec<Arg>,
	pub ret: Option<Box<Expr>>,
	pub block: Block,
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
	pub ty: Expr,
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
	Expr(ExprKind),
	Semi(ExprKind),
	Err,
}

pub type Expr = Spanned<ExprKind>;
#[derive(Debug, Clone)]
pub enum ExprKind {
	ValRef(ValRef),
	LocalRef(LocalRef),
	Type,
	TypeOf(Box<Expr>),
	Ptr(Ptr),
	Tuple(Vec<Expr>),
	Lit(Lit),
	Block(Block),
	Let(Let),
	List(Vec<Expr>),
	Array(Array),
	Cast(Cast),
	Fn(Fn),
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
	Infer,
	Err,
}

#[derive(Debug, Copy, Clone)]
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
	pub ty: Option<Box<Expr>>,
	pub expr: Option<Box<Expr>>,
	pub span: Span,
}

#[derive(Debug, Clone)]
pub struct GlobalLet {
	pub ty: Option<Expr>,
	pub expr: Expr,
}

#[derive(Debug, Clone)]
pub struct Array {
	pub expr: Box<Expr>,
	pub count: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Cast {
	pub expr: Box<Expr>,
	pub ty: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Ptr {
	pub mutability: bool,
	pub to: Box<Expr>,
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

#[derive(Debug, Clone)]
pub struct While {
	pub cond: Box<Expr>,
	pub block: Block,
}

#[derive(Debug, Clone)]
pub struct For {
	pub pat: Pat,
	pub iter: Box<Expr>,
	pub block: Block,
}
