use arena::dense::DenseMap;
use hir::ident::AbsPath;
use verde::{storage, Id, Interned, Tracked};

#[storage]
pub struct Storage(Item, Type);

#[derive(Tracked, Clone, PartialEq, Eq)]
pub struct Item {
	#[id]
	pub path: Id<AbsPath>,
	pub kind: ItemKind,
}

#[derive(Clone, PartialEq, Eq)]
pub enum ItemKind {
	Struct(Struct),
	Enum(Enum),
	Fn(Fn),
	Static(Static),
}

#[derive(Clone, PartialEq, Eq)]
pub struct Struct {
	pub fields: DenseMap<hir::Param, Param>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Enum {
	pub repr: hir::LangItem,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Fn {
	pub params: DenseMap<hir::Param, Param>,
	pub ret: Option<Id<Type>>,
	pub locals: DenseMap<hir::Local, Id<Type>>,
	pub exprs: DenseMap<hir::Expr, Id<Type>>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Static {
	pub ty: Id<Type>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Param {
	pub ty: Id<Type>,
}

#[derive(Clone, PartialEq, Eq, Hash, Interned)]
pub enum Type {
	Array(ArrayType),
	Fn(FnType),
	Infer,
	Struct(Id<AbsPath>),
	Enum(Id<AbsPath>),
	Ptr(PtrType),
	LangItem(hir::LangItem),
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct ArrayType {
	pub ty: Id<Type>,
	pub len: usize,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct FnType {
	/// `None`: `fn`.
	pub abi: Option<Abi>,
	pub params: Vec<Id<Type>>,
	pub ret: Option<Id<Type>>,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct PtrType {
	pub mutable: bool,
	pub ty: Id<Type>,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Abi {
	/// `None`: `extern fn`.
	/// `Some(abi)`: `extern "abi" fn`.
	pub abi: Option<&'static str>,
}
