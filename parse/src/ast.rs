use diag::Span;
use lasso::Spur;

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
	Binding(Binding),
	Scoped(Box<Pat>),
	Union(Vec<Pat>),
	Ignore,
	IgnoreAll,
}

#[derive(Debug, Clone)]
pub struct Binding {
	pub mutability: bool,
	pub binding: Spur,
}

pub type Expr = Spanned<ExprKind>;
#[derive(Debug, Clone)]
pub enum ExprKind {
	Lit(Lit),
	Block(Block),
	Ident(Spur),
	Const(Let),
	Let(Let),
	Static(Let),
	List(Vec<Expr>),
	Array(Array),
	Cast(Cast),
	Type,
	TypeOf(Box<Expr>),
	Ptr(Ptr),
	Struct(Struct),
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
pub struct Struct {
	pub generics: Vec<Arg>,
	pub where_clause: Vec<WhereClause>,
	pub fields: Vec<Arg>,
}

#[derive(Debug, Clone)]
pub struct Enum {
	pub generics: Vec<Arg>,
	pub where_clause: Vec<WhereClause>,
	pub variants: Vec<Variant>,
}

#[derive(Debug, Clone)]
pub struct Variant {
	pub ident: Ident,
	pub fields: Vec<Field>,
}

#[derive(Debug, Clone)]
pub struct Field {
	pub pat: Option<Pat>,
	pub ty: Expr,
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
	DoubleAddr,
	AddrMut,
	DoubleAddrMut,
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
	PlaceConstruct,
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
pub struct Match {
	pub expr: Box<Expr>,
	pub arms: Vec<MatchArm>,
}

#[derive(Debug, Clone)]
pub struct MatchArm {
	pub pat: Pat,
	pub expr: Expr,
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

pub type Stmt = Spanned<StmtKind>;
#[derive(Debug, Clone)]
pub enum StmtKind {
	Import(Import),
	Expr(ExprKind),
	Semi(ExprKind),
	Err,
}

#[derive(Debug, Clone)]
pub struct Import {
	pub prefix: Vec<Ident>,
	pub tree: ImportTree,
}

#[derive(Debug, Clone)]
pub enum ImportTree {
	None,
	Wildcard,
	List(Vec<Import>),
}

#[derive(Debug, Clone)]
pub struct Block {
	pub is_const: bool,
	pub stmts: Vec<Stmt>,
	pub span: Span,
}

pub type Ident = Spanned<Spur>;
