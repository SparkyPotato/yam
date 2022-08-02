use hir::{ctx::LocalRef, types::Type};
use id::{DenseMapBuilder, Id};

use crate::ssir::*;

pub struct FnBuilder {
	curr_block: Block,
	blocks: DenseMapBuilder<Block, (BlockBuilder, bool)>,
	vars: DenseMapBuilder<LocalRef, (Block, Value)>,
}

impl FnBuilder {
	pub fn new() -> Self {
		Self {
			curr_block: Block::UNKNOWN,
			blocks: DenseMapBuilder::new(),
			vars: DenseMapBuilder::new(),
		}
	}

	pub fn add_block(&mut self) -> Block { self.blocks.add((BlockBuilder::new(), false)) }

	pub fn set_block(&mut self, block: Block) { self.curr_block = block; }

	/// Finalize all jumps into a block and its args
	pub fn finalize_block(&mut self, block: Block) { self.blocks[block].1 = true; }

	pub fn add_var(&mut self, var: LocalRef, value: Value) { self.vars.insert_at(var, (self.curr_block, value)); }

	pub fn mutate_var(&mut self, var: LocalRef, value: Value) { self.add_var(var, value); }

	pub fn get_var(&mut self, var: LocalRef) -> Value {
		let (block, val) = self.vars[var];
		if block == self.curr_block {
			val
		} else {
			Value::UNKNOWN
		}
	}
}
