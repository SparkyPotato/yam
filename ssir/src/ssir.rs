use hir::{
	ctx::ValRef,
	hir::{Abi, FnSignature, Lit},
	types::Type,
	Rodeo,
};
use id::{DenseMap, DenseMapBuilder, Id, SparseMap};

pub struct Ssir {
	rodeo: Rodeo,
	pub values: SparseMap<ValRef, ValDefKind>,
}

pub enum ValDefKind {
	Fn(Fn),
	FnDecl(FnSignature),
}

pub struct Fn {
	pub abi: Abi,
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
	fn instr_id_to_val(&self, id: InstrId) -> Value { Value(id.0 + self.args.len() as u32) }

	fn val_to_instr_id(&self, id: Value) -> InstrId { InstrId(id.0 - self.args.len() as u32) }
}

pub struct BlockBuilder {
	args: Vec<Type>,
	instrs: DenseMapBuilder<InstrId, Instr>,
}

impl BlockBuilder {
	pub fn new() -> Self {
		Self {
			args: Vec::new(),
			instrs: DenseMapBuilder::new(),
		}
	}

	pub fn build(self) -> BasicBlock {
		BasicBlock {
			args: self.args,
			instrs: self.instrs.build(),
		}
	}
}

pub struct Instr {
	kind: InstrKind,
	ty: Type,
}

pub enum InstrKind {
	Literal(Lit),
	Global(ValRef),
	Call { target: Value, args: Vec<Value> },
	Cast(Value),
	Unary { op: UnOp, value: Value },
	Binary { op: BinOp, left: InstrId, right: InstrId },
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
