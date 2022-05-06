use std::ops::{Index, Range};

use lasso::Spur;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
	pub start: u32,
	pub end: u32,
}

impl From<Range<u32>> for Span {
	fn from(range: Range<u32>) -> Self {
		Span {
			start: range.start,
			end: range.end,
		}
	}
}

impl From<Range<usize>> for Span {
	fn from(range: Range<usize>) -> Self {
		Span {
			start: range.start as _,
			end: range.end as _,
		}
	}
}

impl Index<Span> for str {
	type Output = str;

	fn index(&self, span: Span) -> &Self::Output { &self[span.start as usize..span.end as usize] }
}

impl chumsky::Span for Span {
	type Context = ();
	type Offset = u32;

	fn new(_: Self::Context, range: Range<Self::Offset>) -> Self { range.into() }

	fn context(&self) -> Self::Context { () }

	fn start(&self) -> Self::Offset { self.start }

	fn end(&self) -> Self::Offset { self.end }
}

pub struct Module {
	pub items: Vec<Item>,
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

pub struct Item {
	pub visibility: Visibility,
	pub kind: ItemKind,
	pub span: Span,
}

pub enum ItemKind {
	Fn(Fn),
}

pub struct Fn {
	pub ident: Ident,
	pub generics: Vec<Arg>,
	pub args: Vec<Arg>,
	pub ret: Option<Expr>,
	pub where_clause: Vec<WhereClause>,
	pub block: Block,
}

pub struct Arg {
	pub pat: Pat,
	pub bounds: Option<Expr>,
	pub span: Span,
}

pub struct WhereClause {
	pub on: Expr,
	pub bounds: Option<Expr>,
	pub span: Span,
}

pub type Pat = Spanned<PatKind>;
pub enum PatKind {
	Binding(Spur),
	Ignore,
}

pub type Expr = Spanned<ExprKind>;
pub enum ExprKind {}

pub type Stmt = Spanned<StmtKind>;
pub enum StmtKind {
	Expr(Expr),
	Semi(Expr),
	Item(Visibility, ItemKind),
}

pub type Block = Spanned<Vec<Stmt>>;

pub type Ident = Spanned<Spur>;
