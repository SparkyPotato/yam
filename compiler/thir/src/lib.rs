use arena::dense::DenseMap;
use hir::ident::AbsPath;
use rustc_hash::FxHashMap;
use verde::{storage, Id, Interned, Tracked};

pub struct Thir {
	pub hir: FxHashMap<Id<AbsPath>, Id<hir::Item>>,
	pub decls: FxHashMap<Id<AbsPath>, Id<ItemDecl>>,
	pub items: FxHashMap<Id<AbsPath>, Id<Item>>,
}

#[storage]
pub struct Storage(ItemDecl, Item, Type);

#[derive(Tracked, Clone, PartialEq, Eq, Debug)]
pub struct ItemDecl {
	#[id]
	pub path: Id<AbsPath>,
	pub kind: ItemDeclKind,
}

#[derive(Tracked, Clone, PartialEq, Eq, Debug)]
pub struct Item {
	#[id]
	pub path: Id<AbsPath>,
	pub decl: Id<ItemDecl>,
	pub locals: DenseMap<hir::Local, Id<Type>>,
	pub exprs: DenseMap<hir::Expr, Id<Type>>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ItemDeclKind {
	Struct(StructDecl),
	Enum(EnumDecl),
	Fn(FnDecl),
	TypeAlias(TypeAliasDecl),
	Static(StaticDecl),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct StructDecl {
	pub fields: DenseMap<hir::Param, Id<Type>>,
	pub ty: Id<Type>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct EnumDecl {
	pub repr: hir::LangItem,
	pub ty: Id<Type>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct FnDecl {
	pub params: DenseMap<hir::Param, Id<Type>>,
	pub ret: Id<Type>,
	pub ty: Id<Type>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct TypeAliasDecl {
	pub ty: Id<Type>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct StaticDecl {
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
