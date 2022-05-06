use std::ops::{Index, Range};

use lasso::Spur;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
	pub start: u32,
	pub end: u32,
	pub file: Spur,
}

impl Index<Span> for str {
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

#[derive(Debug)]
pub struct Item {
	pub visibility: Visibility,
	pub kind: ItemKind,
	pub span: Span,
}

#[derive(Debug)]
pub enum ItemKind {
	Fn(Fn),
}

#[derive(Debug)]
pub struct Fn {
	pub ident: Ident,
	pub generics: Vec<Arg>,
	pub args: Vec<Arg>,
	pub ret: Option<Expr>,
	pub where_clause: Vec<WhereClause>,
	pub block: Block,
}

#[derive(Debug)]
pub struct Arg {
	pub pat: Pat,
	pub bounds: Option<Expr>,
	pub span: Span,
}

#[derive(Debug)]
pub struct WhereClause {
	pub on: Expr,
	pub bounds: Option<Expr>,
	pub span: Span,
}

pub type Pat = Spanned<PatKind>;
#[derive(Debug)]
pub enum PatKind {
	Binding(Spur),
	Ignore,
}

pub type Expr = Spanned<ExprKind>;
#[derive(Debug)]
pub enum ExprKind {
	Lit(Lit),
}

#[derive(Debug)]
pub struct Lit {
	pub kind: LitKind,
	pub spur: Spur,
}

#[derive(Debug)]
pub enum LitKind {
	Bool,
	Char,
	Float,
	Int,
	String,
}

pub type Stmt = Spanned<StmtKind>;
#[derive(Debug)]
pub enum StmtKind {
	Expr(ExprKind),
	Semi(ExprKind),
	Item(Visibility, ItemKind),
}

pub type Block = Spanned<Vec<Stmt>>;

pub type Ident = Spanned<Spur>;
