use arena::dense::DenseMap;
use hir::ident::AbsPath;
use verde::{storage, Id, Interned, Tracked};

#[storage]
pub struct Storage(Item, Type);

#[derive(Tracked, Clone, PartialEq, Eq, Debug)]
pub struct Item {
	#[id]
	pub path: Id<AbsPath>,
	pub locals: DenseMap<hir::Local, Id<Type>>,
	pub exprs: DenseMap<hir::Expr, Id<Type>>,
	pub kind: ItemKind,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ItemKind {
	Struct(Struct),
	Enum(Enum),
	Fn(Fn),
	TypeAlias(TypeAlias),
	Static(Static),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Struct {
	pub fields: DenseMap<hir::Param, Id<Type>>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Enum {
	pub repr: hir::LangItem,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Fn {
	pub params: DenseMap<hir::Param, Id<Type>>,
	pub ret: Id<Type>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct TypeAlias {
	pub ty: Id<Type>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Static {
	pub ty: Id<Type>,
}

#[derive(Clone, PartialEq, Eq, Hash, Interned, Debug)]
pub enum Type {
	Array(ArrayType),
	Fn(FnType),
	Struct(Id<AbsPath>),
	Enum(Id<AbsPath>),
	Ptr(PtrType),
	LangItem(hir::LangItem),
	Error,
	Void,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct ArrayType {
	pub ty: Id<Type>,
	pub len: u64,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct FnType {
	pub params: Vec<Id<Type>>,
	pub ret: Id<Type>,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct PtrType {
	pub mutable: bool,
	pub ty: Id<Type>,
}
