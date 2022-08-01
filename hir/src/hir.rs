use std::hash::Hash;

use diag::Span;
pub use parse::{
	ast::{Abi, BinOp, Ident, Spanned, UnOp, Visibility},
	Spur,
};

use crate::{
	ctx::{LocalRef, ValRef},
	types::Type,
};

#[derive(Default, Clone, Eq)]
pub struct Path(Vec<Ident>);

impl PartialEq for Path {
	fn eq(&self, other: &Self) -> bool {
		if self.0.len() != other.0.len() {
			return false;
		}

		for (a, b) in self.0.iter().zip(other.0.iter()) {
			if a.node != b.node {
				return false;
			}
		}

		true
	}
}

impl Hash for Path {
	fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
		for ident in self.0.iter() {
			ident.node.hash(state);
		}
	}
}

impl Path {
	pub fn from_ident(ident: Ident) -> Self { Self(vec![ident]) }

	pub fn push(&mut self, ident: Ident) { self.0.push(ident); }

	pub fn with(&self, ident: Ident) -> Self {
		let mut path = self.clone();
		path.push(ident);
		path
	}

	pub fn is_child_of(&self, parent: &Path) -> bool {
		for (child, parent) in self.0.iter().zip(parent.0.iter()) {
			if child != parent {
				return false;
			}
		}

		true
	}
	
	pub fn iter(&self) -> impl Iterator<Item = &Ident> { self.0.iter() }

	pub fn ident(&self) -> &Ident { self.0.last().unwrap() }
}

pub struct ValDef {
	pub path: Path,
	pub kind: ValDefKind,
	pub span: Span,
}

pub enum ValDefKind {
	Static(GlobalLet),
	Const(GlobalLet),
	Fn(Fn),
	FnDecl(FnSignature),
	Struct(Struct),
}

#[derive(Clone)]
pub struct Struct {
	pub fields: Vec<Field>,
}

#[derive(Clone)]
pub struct Field {
	pub visibility: Visibility,
	pub name: Ident,
	pub ty: Expr,
	pub span: Span,
}

#[derive(Clone)]
pub struct Fn {
	pub sig: FnSignature,
	pub block: Block,
}

#[derive(Clone)]
pub struct FnSignature {
	pub abi: Abi,
	pub args: Vec<Arg>,
	pub ret_expr: Option<Box<Expr>>,
	pub ret: Type,
}

pub type Pat = Spanned<PatKind>;
#[derive(Clone)]
pub enum PatKind {
	Binding(Binding),
}

#[derive(Clone)]
pub struct Binding {
	pub mutability: bool,
	pub binding: LocalRef,
	pub ty: Type,
}

#[derive(Clone)]
pub struct Arg {
	pub is_const: bool,
	pub pat: Pat,
	pub ty: Expr,
	pub span: Span,
}

#[derive(Clone)]
pub struct Block {
	pub is_const: bool,
	pub stmts: Vec<Stmt>,
	pub ty: Type,
	pub span: Span,
}

pub type Stmt = Spanned<StmtKind>;
#[derive(Clone)]
pub enum StmtKind {
	Expr(ExprData),
	Semi(ExprData),
	Err,
}

pub type Expr = Spanned<ExprData>;
#[derive(Clone)]
pub struct ExprData {
	pub kind: ExprKind,
	pub ty: Type,
}

#[derive(Clone)]
pub enum ExprKind {
	ValRef(ValRef),
	LocalRef(LocalRef),
	Never,
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
	Continue,
	Return(Option<Box<Expr>>),
	If(If),
	Loop(Loop),
	While(While),
	For(For),
	Infer,
	Err,
}

#[derive(Copy, Clone)]
pub enum Lit {
	Bool(bool),
	Char(char),
	Float(f64),
	Int(i128),
	String(Spur),
}

#[derive(Clone)]
pub struct Let {
	pub pat: Pat,
	pub ty_expr: Option<Box<Expr>>,
	pub ty: Type,
	pub expr: Option<Box<Expr>>,
	pub span: Span,
}

#[derive(Clone)]
pub struct GlobalLet {
	pub ty_expr: Option<Expr>,
	pub ty: Type,
	pub expr: Expr,
}

#[derive(Clone)]
pub struct Array {
	pub expr: Box<Expr>,
	pub count: Box<Expr>,
}

#[derive(Clone)]
pub struct Cast {
	pub expr: Box<Expr>,
	pub ty: Box<Expr>,
}

#[derive(Clone)]
pub struct Ptr {
	pub mutability: bool,
	pub to: Box<Expr>,
}

#[derive(Clone)]
pub struct Call {
	pub target: Box<Expr>,
	pub args: Vec<Expr>,
}

#[derive(Clone)]
pub struct Index {
	pub target: Box<Expr>,
	pub index: Box<Expr>,
}

#[derive(Clone)]
pub struct Access {
	pub target: Box<Expr>,
	pub field: Ident,
}

#[derive(Clone)]
pub struct Unary {
	pub op: UnOp,
	pub expr: Box<Expr>,
}

#[derive(Clone)]
pub struct Binary {
	pub lhs: Box<Expr>,
	pub op: BinOp,
	pub rhs: Box<Expr>,
}

#[derive(Clone)]
pub struct If {
	pub cond: Box<Expr>,
	pub then: Block,
	pub else_: Option<Box<Expr>>,
}

#[derive(Clone)]
pub struct Loop {
	pub block: Block,
	pub while_: Option<Box<Expr>>,
}

#[derive(Clone)]
pub struct While {
	pub cond: Box<Expr>,
	pub block: Block,
}

#[derive(Clone)]
pub struct For {
	pub pat: Pat,
	pub iter: Box<Expr>,
	pub block: Block,
}
