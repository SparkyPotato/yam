use std::ops::{Add, Index as SIndex, Range};

use lasso::Spur;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
	pub start: u32,
	pub end: u32,
	pub file: Spur,
}

impl Add for Span {
	type Output = Span;

	fn add(self, other: Span) -> Span {
		debug_assert_eq!(self.file, other.file, "Cannot merge spans in different files");

		Span {
			start: self.start.min(other.start),
			end: self.end.max(other.end),
			file: self.file,
		}
	}
}

impl SIndex<Span> for str {
	type Output = str;

	fn index(&self, span: Span) -> &Self::Output { &self[span.start as usize..span.end as usize] }
}

impl ariadne::Span for Span {
	type SourceId = Spur;

	fn source(&self) -> &Self::SourceId { &self.file }

	fn start(&self) -> usize { self.start as _ }

	fn end(&self) -> usize { self.end as _ }
}

impl chumsky::Span for Span {
	type Context = Spur;
	type Offset = u32;

	fn new(file: Self::Context, range: Range<Self::Offset>) -> Self {
		Span {
			start: range.start,
			end: range.end,
			file,
		}
	}

	fn context(&self) -> Self::Context { self.file }

	fn start(&self) -> Self::Offset { self.start }

	fn end(&self) -> Self::Offset { self.end }
}

#[derive(Debug)]
pub struct Module {
	pub stmts: Vec<Stmt>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Spanned<T> {
	pub node: T,
	pub span: Span,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Visibility {
	Public,
	Private,
}

pub type Pat = Spanned<PatKind>;
#[derive(Debug, Clone)]
pub enum PatKind {
	Binding(Spur),
	Ignore,
}

pub type Expr = Spanned<ExprKind>;
#[derive(Debug, Clone)]
pub enum ExprKind {
	Lit(Lit),
	Block(Block),
	Ident(Ident),
	Const(Let),
	Let(Let),
	List(Vec<Expr>),
	Array(Array),
	Type,
	Struct(Struct),
	Fn(Fn),
	Call(Call),
	Index(Index),
	Access(Access),
	Unary(Unary),
	Binary(Binary),
	Error,
}

#[derive(Debug, Clone)]
pub struct Lit {
	pub kind: LitKind,
	pub sym: Spur,
}

#[derive(Debug, Clone)]
pub enum LitKind {
	Bool,
	Char,
	Float,
	Int,
	String,
}

#[derive(Debug, Clone)]
pub struct Let {
	pub visibility: Visibility,
	pub pat: Pat,
	pub ty: Option<Box<Expr>>,
	pub expr: Option<Box<Expr>>,
}

#[derive(Debug, Clone)]
pub struct Array {
	pub expr: Box<Expr>,
	pub count: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Struct {
	pub generics: Vec<Arg>,
	pub where_clause: Vec<WhereClause>,
	pub fields: Vec<Arg>,
}

#[derive(Debug, Clone)]
pub struct Fn {
	pub generics: Vec<Arg>,
	pub args: Vec<Arg>,
	pub ret: Option<Box<Expr>>,
	pub where_clause: Vec<WhereClause>,
	pub block: Block,
}

#[derive(Debug, Clone)]
pub struct Arg {
	pub visibility: Visibility,
	pub is_const: bool,
	pub pat: Pat,
	pub bounds: Option<Expr>,
	pub span: Span,
}

#[derive(Debug, Clone)]
pub struct WhereClause {
	pub on: Expr,
	pub bounds: Option<Expr>,
	pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Call {
	pub target: Box<Expr>,
	pub args: Vec<CallArg>,
}

#[derive(Debug, Clone)]
pub struct CallArg {
	pub pat: Option<Pat>,
	pub expr: Expr,
	pub span: Span,
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
pub enum UnOp {
	Not,
	Neg,
	Addr,
	AddrMut,
	Deref,
}

#[derive(Debug, Clone)]
pub struct Unary {
	pub op: UnOp,
	pub expr: Box<Expr>,
}

#[derive(Debug, Clone)]
pub enum BinOp {
	Add,
	Sub,
	Mul,
	Div,
	Rem,
	Shl,
	Shr,
	Lt,
	Gt,
	Leq,
	Geq,
	Eq,
	Neq,
	BitAnd,
	BitOr,
	BitXor,
	And,
	Or,
	Assign,
	AddAssign,
	SubAssign,
	MulAssign,
	DivAssign,
	RemAssign,
	BitAndAssign,
	BitOrAssign,
	BitXorAssign,
	ShlAssign,
	ShrAssign,
}

#[derive(Debug, Clone)]
pub struct Binary {
	pub lhs: Box<Expr>,
	pub op: BinOp,
	pub rhs: Box<Expr>,
}

pub type Stmt = Spanned<StmtKind>;
#[derive(Debug, Clone)]
pub enum StmtKind {
	Expr(ExprKind),
	Semi(ExprKind),
}

#[derive(Debug, Clone)]
pub struct Block {
	pub is_const: bool,
	pub stmts: Vec<Stmt>,
	pub span: Span,
}

pub type Ident = Spanned<Spur>;
