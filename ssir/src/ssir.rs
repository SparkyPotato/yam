use std::fmt::{Debug, Display, Formatter};

use diag::Span;
use hir::{
	ctx::ValRef,
	hir::{Abi, FnSignature, Lit, Path, Spur, Struct},
	lang_item::LangItem,
	types::Type,
	Rodeo,
};
use id::{DenseMap, DenseMapBuilder, Id, SparseMap};

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
	pub blocks: DenseMap<Block, BasicBlock>,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct Block(u32);

impl Block {
	pub const UNKNOWN: Self = Self(u32::MAX);
}

impl Id for Block {
	fn id(self) -> u32 { self.0 }

	fn from_id(id: u32) -> Self { Self(id) }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct Value(u32);

impl Value {
	pub const UNKNOWN: Self = Self(u32::MAX);
}

impl Id for Value {
	fn id(self) -> u32 { self.0 }

	fn from_id(id: u32) -> Self { Self(id) }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
struct InstrId(u32);
impl Id for InstrId {
	fn id(self) -> u32 { self.0 }

	fn from_id(id: u32) -> Self { Self(id) }
}

pub struct BasicBlock {
	args: Vec<Type>,
	instrs: DenseMap<InstrId, Instr>,
}

impl BasicBlock {
	pub fn args(&self) -> impl Iterator<Item = (Value, &Type)> {
		self.args.iter().enumerate().map(|(i, x)| (Value(i as _), x))
	}

	pub fn instrs(&self) -> impl Iterator<Item = (Value, &Instr)> + '_ {
		self.instrs.iter().map(|x| (self.instr_id_to_val(x.0), x.1))
	}

	fn instr_id_to_val(&self, id: InstrId) -> Value { Value(id.0 + self.args.len() as u32) }

	fn val_to_instr_id(&self, id: Value) -> InstrId { InstrId(id.0 - self.args.len() as u32) }
}

pub struct BlockBuilder {
	args: Vec<Type>,
	instrs: DenseMapBuilder<InstrId, Instr>,
	added_args: u32,
}

impl BlockBuilder {
	pub fn new() -> Self {
		Self {
			args: Vec::new(),
			instrs: DenseMapBuilder::new(),
			added_args: 0,
		}
	}

	pub fn instr(&mut self, instr: Instr) -> Value {
		let id = self.instrs.add(instr);
		self.instr_id_to_val(id)
	}

	pub fn build(self) -> BasicBlock {
		assert_eq!(self.added_args, 0, "BlockBuilder::finalize_args not called");

		BasicBlock {
			args: self.args,
			instrs: self.instrs.build(),
		}
	}

	pub fn add_arg(&mut self, ty: Type) -> Value {
		let id = self.args.len() as u32;
		self.added_args += 1;

		self.args.push(ty);

		Value(id)
	}

	pub fn finalize_args(&mut self) {
		for (id, instr) in self.instrs.iter_mut() {
			match &mut instr.kind {
				InstrKind::Void => {},
				InstrKind::Literal(_) => {},
				InstrKind::Global(_) => {},
				InstrKind::Call { target, args } => {
					*target = Value(id.0 + self.added_args);
					for arg in args {
						*arg = Value(id.0 + self.added_args);
					}
				},
				InstrKind::Cast(c) => {
					*c = Value(id.0 + self.added_args);
				},
				InstrKind::Unary { value, .. } => {
					*value = Value(id.0 + self.added_args);
				},
				InstrKind::Binary { left, right, .. } => {
					*left = Value(id.0 + self.added_args);
					*right = Value(id.0 + self.added_args);
				},
				InstrKind::Jump { args, .. } => {
					for arg in args {
						*arg = Value(id.0 + self.added_args);
					}
				},
				InstrKind::JumpIf { cond, args, .. } => {
					*cond = Value(id.0 + self.added_args);
					for arg in args {
						*arg = Value(id.0 + self.added_args);
					}
				},
				InstrKind::Ret(r) => *r = Value(id.0 + self.added_args),
			}
		}

		self.added_args = 0;
	}

	pub fn instrs(&mut self) -> impl Iterator<Item = (Value, &mut Instr)> + '_ {
		let len = self.args.len() as u32;
		self.instrs
			.iter_mut()
			.map(move |(id, instr)| (Value(id.0 + len), instr))
	}

	pub fn get_instr(&mut self, id: Value) -> &mut Instr {
		let id = self.val_to_instr_id(id);
		&mut self.instrs[id]
	}

	fn instr_id_to_val(&self, id: InstrId) -> Value { Value(id.0 + self.args.len() as u32) }

	fn val_to_instr_id(&self, id: Value) -> InstrId { InstrId(id.0 - self.args.len() as u32) }
}

pub struct Instr {
	pub kind: InstrKind,
	pub ty: Type,
}

pub enum InstrKind {
	Void,
	Literal(Lit),
	Global(ValRef),
	Call { target: Value, args: Vec<Value> },
	Cast(Value),
	Unary { op: UnOp, value: Value },
	Binary { op: BinOp, left: Value, right: Value },
	Jump { to: Block, args: Vec<Value> },
	JumpIf { cond: Value, to: Block, args: Vec<Value> },
	Ret(Value),
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
