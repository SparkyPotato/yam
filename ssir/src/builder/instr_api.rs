use hir::{ctx::ValRef, hir::Lit, types::Type};

use crate::{builder::FnBuilder, ssir::*};

impl FnBuilder {
	pub fn lit(&mut self, lit: Lit, ty: Type) -> Value { self.curr_block().value_instr(ValueInstr::Literal(lit), ty) }

	pub fn global(&mut self, val: ValRef, ty: Type) -> Value {
		self.curr_block().value_instr(ValueInstr::Global(val), ty)
	}

	pub fn call(&mut self, target: Value, args: Vec<Value>, ty: Type) -> Value {
		self.curr_block().value_instr(ValueInstr::Call { target, args }, ty)
	}

	pub fn cast(&mut self, value: Value, ty: Type) -> Value {
		self.curr_block().value_instr(ValueInstr::Cast(value), ty)
	}

	pub fn unary(&mut self, op: UnOp, value: Value, ty: Type) -> Value {
		self.curr_block().value_instr(ValueInstr::Unary { op, value }, ty)
	}

	pub fn binary(&mut self, left: Value, op: BinOp, right: Value, ty: Type) -> Value {
		self.curr_block()
			.value_instr(ValueInstr::Binary { left, op, right }, ty)
	}

	pub fn jump(&mut self, to: Block, args: Vec<Value>) {
		self.metadata[to].jumped_to_from.push(self.curr_block);

		self.curr_block().non_value_instr(NonValueInstr::Jump { to, args });
	}

	pub fn jump_if(&mut self, cond: Value, to: Block, args: Vec<Value>) {
		self.metadata[to].jumped_to_from.push(self.curr_block);

		self.curr_block()
			.non_value_instr(NonValueInstr::JumpIf { cond, to, args });
	}

	pub fn ret(&mut self, value: Option<Value>) { self.curr_block().non_value_instr(NonValueInstr::Ret(value)); }

	fn curr_block(&mut self) -> &mut BasicBlock {
		debug_assert!(self.curr_block != Block::UNKNOWN);
		&mut self.blocks[self.curr_block]
	}
}
