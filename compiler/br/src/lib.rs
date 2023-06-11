use arena::{Arena, Ix};
use diagnostics::Diagnostic;
use syntax::{
	ast as a,
	token::{Ident, StringLit},
};
use text::Text;
use verde::{storage, Db, Id, Interned, Tracked};

pub use crate::ast::ModuleMap;
use crate::ast::{AstId, ErasedAstId};

pub mod ast;

#[storage]
pub struct Storage(AbsolutePath, Path, Diagnostic<ErasedAstId>);

pub struct BrModule {
	pub module: Id<Module>,
	pub map: ModuleMap,
}

/// A path, optionally preceded by a `.`, indicating that name resolution should start from the global scope.
#[derive(Interned, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Path {
	pub prec: Option<Id<Path>>,
	/// Can be `.` if this is a root path.
	pub ident: Text,
}

/// The identifier of a package. A package is a compilation unit, and as such, packages form a dependency DAG.
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct PackageId(u32);

/// The absolute path of a definition.
#[derive(Interned, Copy, Clone, PartialEq, Eq, Hash)]
pub struct AbsolutePath {
	pub package: PackageId,
	pub path: Id<Path>,
}

impl Path {
	pub fn stringify(db: &dyn Db, this: Id<Self>) -> String {
		let this = db.geti(this);
		let mut path = this.prec.map(|prec| Self::stringify(db, prec)).unwrap_or_default();
		let ident = this.ident.as_str();
		path.reserve(ident.len() + 1);
		if !matches!(path.chars().last(), Some('.')) {
			path.push_str(".");
		}
		path.push_str(ident);
		path
	}

	pub fn from_ast(db: &mut dyn Db, path: a::Path) -> Option<Id<Self>> {
		let prec = path.qualifier().map(|prec| Self::from_ast(db, prec)).unwrap_or(None);
		let this = match path.segment() {
			Some(a::PathSegment::Name(name)) => name.text().map(|ident| Self { prec, ident }),
			Some(a::PathSegment::Dot(_)) => Some(Self {
				prec,
				ident: Text::new("."),
			}),
			None => None,
		};
		this.map(|this| db.add(this))
	}
}

#[derive(Tracked, PartialEq, Eq)]
pub struct Module {
	#[id]
	pub path: Id<AbsolutePath>,
	pub items: Vec<Item>,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Name {
	pub name: Text,
	pub id: AstId<Ident>,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum LangItem {
	// Primitives.
	U8,
	U16,
	U32,
	U64,
	U128,
	I8,
	I16,
	I32,
	I64,
	I128,
	Bool,
	Char,
	F32,
	F64,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Attr {
	LangItem(LangItem),
}

#[derive(Clone, PartialEq, Eq)]
pub struct Item {
	pub name: Name,
	pub attrs: Vec<Attr>,
	pub kind: ItemKind,
	pub exprs: Arena<Expr>,
	pub types: Arena<Type>,
	pub locals: Arena<Local>,
}

#[derive(Clone, PartialEq, Eq)]
pub enum ItemKind {
	Struct(Struct),
	Enum(Enum),
	Fn(Fn),
	Static(Static),
}

#[derive(Clone, PartialEq, Eq)]
pub struct Fn {
	pub abi: Option<Abi>,
	pub name: Name,
	pub params: Vec<Param>,
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
	pub name: Name,
	pub fields: Vec<Param>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Enum {
	pub variants: Arena<Variant>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Variant(pub Name);

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
	Infer,
	Array(ArrayType),
	Fn(FnType),
	Path(Id<Path>),
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
	Path(Id<Path>),
	Local(Ix<Local>),
	EnumVariant(VariantExpr),
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
	pub name: Name,
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
	pub field: Name,
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
pub struct VariantExpr {
	pub en: Ix<Enum>,
	pub variant: Ix<Variant>,
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
	pub value: Option<Ix<Expr>>,
}
