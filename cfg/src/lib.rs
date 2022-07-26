use std::collections::HashMap;

use diag::Span;
pub use name_resolve::{
	resolved::{BinOp, Ident, InbuiltType, Lit, Path, TyRef, UnOp, ValRef},
	Rodeo,
};

use crate::types::TypeId;

pub mod lower;
pub mod types;

pub struct Ctx {
	pub types: HashMap<TyRef, Ty>,
	pub globals: HashMap<ValRef, Val>,
	pub inbuilt_types: HashMap<InbuiltType, TyRef>,
}

pub enum Ty {
	Inbuilt(InbuiltType),
	Struct(Struct),
}

impl Ty {
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

pub enum Val {
	Fn(Fn),
}

pub enum Type {
	Void,
	Fn { args: Vec<Type>, ret: Box<Type> },
	TyRef(TyRef),
	Ptr { mutable: bool, to: Box<Type> },
	Err,
}

pub struct Struct {
	pub path: Path,
	pub fields: Vec<Field>,
	pub span: Span,
}

pub struct Field {
	pub name: Ident,
	pub ty: Type,
	pub span: Span,
}

pub struct Fn {
	pub path: Path,
	pub ret: Type,
	pub blocks: Vec<BasicBlock>,
	pub span: Span,
}

pub struct Arg {
	pub ident: Span,
	pub ty: Type,
}

pub struct BasicBlockId(pub(crate) u32);
pub struct InstrId(pub(crate) u32);
pub struct ArgId(pub(crate) u32);

pub struct BasicBlock {
	pub args: Vec<Arg>,
	pub instrs: Vec<Instr>,
}

pub struct Instr {
	pub kind: InstrKind,
	pub span: Span,
	pub ty: Type,
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
