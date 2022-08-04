use std::fmt::{Debug, Display, Formatter};

use diag::Span;
use hir::{
	ctx::ValRef,
	hir::{Abi, FnSignature, Lit, Path, Spur, Struct},
	lang_item::LangItem,
	types::Type,
	Rodeo,
};
use id::{DenseMapBuilder, Id, SparseMap};

use crate::pretty::SsirWriter;

pub struct Ssir {
	pub rodeo: Rodeo,
	pub values: SparseMap<ValRef, ValDef>,
	pub tys: SparseMap<ValRef, TyDef>,
}

impl Debug for Ssir {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result { SsirWriter::new(self, f).write() }
}

impl Ssir {
	pub fn rodeo(&self) -> &Rodeo { &self.rodeo }

	pub fn resolve_intern(&self, spur: Spur) -> &str { self.rodeo.resolve(&spur) }
}

pub struct ValDef {
	pub path: Path,
	pub kind: ValDefKind,
	pub span: Span,
}

pub enum ValDefKind {
	Fn(Fn),
	FnDecl(FnSignature),
}

pub struct TyDef {
	pub path: Path,
	pub kind: TyDefKind,
	pub span: Span,
}

pub enum TyDefKind {
	Struct(Struct),
	LangItem(LangItem),
}

pub struct Fn {
	pub abi: Abi,
	pub ret: Type,
	pub blocks: DenseMapBuilder<Block, BasicBlock>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct Block(u32);

impl Block {
	pub const UNKNOWN: Self = Self(u32::MAX);
}

impl Id for Block {
	fn id(self) -> u32 { self.0 }

	fn from_id(id: u32) -> Self { Self(id) }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct Value(u32);

impl Value {
	pub const UNKNOWN: Self = Self(u32::MAX / 2);
}

impl Id for Value {
	fn id(self) -> u32 { self.0 }

	fn from_id(id: u32) -> Self { Self(id) }
}

impl Value {
	pub fn is_resolved(self) -> bool { self.0 < Self::UNKNOWN.0 }

	pub fn unresolved(value: u32) -> Self { Self(Self::UNKNOWN.0 + value) }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct InstrId(u32);

impl Id for InstrId {
	fn id(self) -> u32 { self.0 }

	fn from_id(id: u32) -> Self { Self(id) }
}

enum InstrIdOrArg {
	Id(InstrId),
	Arg(u32),
}

pub struct BasicBlock {
	args: Vec<(Type, Value)>,
	instrs: DenseMapBuilder<InstrId, Instr>,
	val_map: DenseMapBuilder<Value, InstrIdOrArg>,
}

impl BasicBlock {
	pub fn new() -> Self {
		Self {
			args: Vec::new(),
			instrs: DenseMapBuilder::new(),
			val_map: DenseMapBuilder::new(),
		}
	}

	pub fn arg(&mut self, ty: Type) -> Value {
		let value = self.val_map.add(InstrIdOrArg::Arg(self.args.len() as u32));
		self.args.push((ty, value));
		value
	}

	pub fn value_instr(&mut self, instr: ValueInstr, ty: Type) -> Value {
		let value = self.val_map.add(InstrIdOrArg::Id(InstrId(0)));
		let id = self.instrs.add(Instr::Value { value, instr, ty });
		self.val_map[value] = InstrIdOrArg::Id(id);

		value
	}

	pub fn non_value_instr(&mut self, instr: NonValueInstr) -> InstrId { self.instrs.add(Instr::NonValue(instr)) }

	pub fn args(&self) -> impl Iterator<Item = (Value, &Type)> { self.args.iter().map(|x| (x.1, &x.0)) }

	pub fn instrs(&self) -> impl Iterator<Item = (InstrId, &Instr)> { self.instrs.iter() }

	pub fn instrs_mut(&mut self) -> impl Iterator<Item = (InstrId, &mut Instr)> { self.instrs.iter_mut() }

	pub fn clone_instrs(&self) -> DenseMapBuilder<InstrId, Instr> { self.instrs.clone() }

	pub fn value_to_instr(&self, value: Value) -> InstrId {
		match self.val_map[value] {
			InstrIdOrArg::Id(id) => id,
			_ => unreachable!("expected value to be an instruction"),
		}
	}
}

#[derive(Clone)]
pub enum Instr {
	Value { value: Value, instr: ValueInstr, ty: Type },
	NonValue(NonValueInstr),
}

#[derive(Clone)]
pub enum ValueInstr {
	Literal(Lit),
	Global(ValRef),
	Call { target: Value, args: Vec<Value> },
	Cast(Value),
	Unary { op: UnOp, value: Value },
	Binary { op: BinOp, left: Value, right: Value },
}

#[derive(Clone)]
pub enum NonValueInstr {
	Jump { to: Block, args: Vec<Value> },
	JumpIf { cond: Value, to: Block, args: Vec<Value> },
	Ret(Option<Value>),
}

#[derive(Debug, Copy, Clone)]
pub enum UnOp {
	Not,
	Neg,
	Addr,
	AddrMut,
	Deref,
}

impl Display for UnOp {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		match self {
			UnOp::Not => write!(f, "!"),
			UnOp::Neg => write!(f, "-"),
			UnOp::Addr => write!(f, "&"),
			UnOp::AddrMut => write!(f, "&mut"),
			UnOp::Deref => write!(f, "*"),
		}
	}
}

#[derive(Debug, Copy, Clone)]
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
}

impl Display for BinOp {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		match self {
			BinOp::Add => write!(f, "+"),
			BinOp::Sub => write!(f, "-"),
			BinOp::Mul => write!(f, "*"),
			BinOp::Div => write!(f, "/"),
			BinOp::Rem => write!(f, "%"),
			BinOp::Shl => write!(f, "<<"),
			BinOp::Shr => write!(f, ">>"),
			BinOp::Lt => write!(f, "<"),
			BinOp::Gt => write!(f, ">"),
			BinOp::Leq => write!(f, "<="),
			BinOp::Geq => write!(f, ">="),
			BinOp::Eq => write!(f, "=="),
			BinOp::Neq => write!(f, "!="),
			BinOp::BitAnd => write!(f, "&"),
			BinOp::BitOr => write!(f, "|"),
			BinOp::BitXor => write!(f, "^"),
			BinOp::And => write!(f, "&&"),
			BinOp::Or => write!(f, "||"),
		}
	}
}
