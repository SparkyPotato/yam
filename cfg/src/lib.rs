use std::collections::HashMap;

use diag::Span;
pub use name_resolve::{
	resolved::{BinOp, Ident, InbuiltType, Lit, Path, TyRef, UnOp, ValRef},
	Rodeo,
};

use crate::types::TypeId;

pub mod lower;
pub mod types;

pub type RCtx = Ctx<Type>;
pub type UCtx = Ctx<TypeId>;

pub struct Ctx<T> {
	pub types: HashMap<TyRef, Ty<T>>,
	pub globals: HashMap<ValRef, Val<T>>,
	pub inbuilt_types: HashMap<InbuiltType, TyRef>,
}

pub enum Ty<T> {
	Inbuilt(InbuiltType),
	Struct(Struct<T>),
}

impl<T> Ty<T> {
	pub fn to_string(&self, rodeo: &Rodeo) -> String {
		match self {
			Self::Inbuilt(i) => match i {
				InbuiltType::Int(x) => format!("i{}", x),
				InbuiltType::Uint(x) => format!("u{}", x),
				InbuiltType::Float(x) => format!("f{}", x),
				InbuiltType::Bool => "bool".to_string(),
			},
			Self::Struct(s) => format!(
				"{}",
				s.path
					.iter()
					.map(|x| rodeo.resolve(&x.node))
					.collect::<Vec<_>>()
					.join(".")
			),
		}
	}
}

pub enum Val<T> {
	Fn(Fn<T>),
}

pub enum Type {
	Void,
	Fn { args: Vec<Type>, ret: Box<Type> },
	TyRef(TyRef),
	Ptr { mutable: bool, to: Box<Type> },
	Err,
}

pub struct Struct<T> {
	pub path: Path,
	pub fields: Vec<Field<T>>,
	pub span: Span,
}

pub struct Field<T> {
	pub name: Ident,
	pub ty: T,
	pub span: Span,
}

pub struct Fn<T> {
	pub path: Path,
	pub ret: Box<T>,
	pub blocks: Vec<BasicBlock<T>>,
	pub span: Span,
}

pub struct Arg<T> {
	pub ident: Option<Ident>,
	pub ty: T,
}

pub struct BasicBlockId(pub(crate) u32);
pub struct InstrId(pub(crate) u32);
pub struct ArgId(pub(crate) u32);

pub struct BasicBlock<T> {
	pub args: Vec<Arg<T>>,
	pub instrs: Vec<Instr<T>>,
}

pub struct Instr<T> {
	pub kind: InstrKind,
	pub span: Span,
	pub ty: T,
}

pub enum InstrKind {
	Literal(Lit),
	Global(ValRef),
	Arg(ArgId),
	Call {
		target: InstrId,
		args: Vec<InstrId>,
	},
	Cast(InstrId),
	Unary {
		op: UnOp,
		value: InstrId,
	},
	Binary {
		op: BinOp,
		left: InstrId,
		right: InstrId,
	},
	Jmp {
		to: BasicBlockId,
		args: Vec<InstrId>,
	},
	CondJmp {
		if_: InstrId,
		to: BasicBlockId,
		args: Vec<InstrId>,
	},
	Ret(Option<InstrId>),
}
