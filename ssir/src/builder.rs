use std::collections::HashMap;

use hir::{
	ctx::LocalRef,
	hir::{Arg, PatKind},
	types::Type,
};
use id::{DenseMap, DenseMapBuilder, Id};

use crate::ssir::*;

struct BlockMeta {
	jumped_to_from: Vec<(Block, Value)>,
	external_var_usages: HashMap<Value, (Block, Value)>,
	unknown_vars: u32,
}

pub struct FnBuilder {
	curr_block: Block,
	blocks: DenseMapBuilder<Block, (BlockBuilder, bool)>,
	metadata: DenseMapBuilder<Block, BlockMeta>,
	vars: DenseMapBuilder<LocalRef, (Block, Value)>,
}

impl FnBuilder {
	pub fn new() -> Self {
		Self {
			curr_block: Block::UNKNOWN,
			blocks: DenseMapBuilder::new(),
			metadata: DenseMapBuilder::new(),
			vars: DenseMapBuilder::new(),
		}
	}

	pub fn reset(&mut self) {
		self.curr_block = Block::UNKNOWN;
		self.blocks.reset();
		self.metadata.reset();
		self.vars.reset();
	}

	pub fn add_block(&mut self) -> Block {
		let id = self.blocks.add((BlockBuilder::new(), false));
		self.metadata.insert_at(
			id,
			BlockMeta {
				jumped_to_from: Vec::new(),
				external_var_usages: HashMap::new(),
				unknown_vars: 0,
			},
		);
		id
	}

	pub fn set_block(&mut self, block: Block) { self.curr_block = block; }

	/// Finalize all jumps into a block and its args
	pub fn finalize_block(&mut self, block: Block) {
		let b = &mut self.blocks[block];
		let meta = &mut self.metadata[block];
		b.1 = true;

		for (_, instr) in b.0.instrs() {
			match &mut instr.kind {
				InstrKind::Void => {},
				InstrKind::Literal(_) => {},
				InstrKind::Global(_) => {},
				InstrKind::Call { .. } => {},
				InstrKind::Cast(_) => {},
				InstrKind::Unary { .. } => {},
				InstrKind::Binary { .. } => {},
				InstrKind::Jump { .. } => {},
				InstrKind::JumpIf { .. } => {},
				InstrKind::Ret(_) => {},
			}
		}
	}

	pub fn add_var(&mut self, var: LocalRef, value: Value) { self.vars.insert_at(var, (self.curr_block, value)); }

	pub fn mutate_var(&mut self, var: LocalRef, value: Value) { self.add_var(var, value); }

	pub fn get_var(&mut self, var: LocalRef) -> Value {
		let (block, val) = self.vars[var];
		if block == self.curr_block {
			val
		} else {
			let meta = &mut self.metadata[self.curr_block];

			let id = Value::from_id(Value::UNKNOWN.id() - meta.unknown_vars);
			meta.unknown_vars += 1;

			self.metadata[self.curr_block]
				.external_var_usages
				.insert(id, (block, val));
			id
		}
	}

	pub fn add_arg(&mut self, ty: Type) -> Value { self.blocks[self.curr_block].0.add_arg(ty) }

	pub fn instr(&mut self, instr: InstrKind, ty: Type) -> Value {
		debug_assert!(self.curr_block != Block::UNKNOWN);

		let block = &mut self.blocks[self.curr_block];

		debug_assert!(!block.1, "Cannot add instructions to finalized block");

		let jumping_to = match &instr {
			InstrKind::Jump { to, .. } | InstrKind::JumpIf { to, .. } => Some(*to),
			_ => None,
		};

		let value = block.0.instr(Instr { kind: instr, ty });

		if let Some(to) = jumping_to {
			if self.blocks[to].1 {
				panic!("Cannot jump to finalized block");
			}
			self.metadata[to].jumped_to_from.push((self.curr_block, value));
		}

		value
	}

	pub fn fn_init_block(&mut self, args: impl IntoIterator<Item = Arg>) -> Block {
		let block = self.add_block();

		let b = &mut self.blocks[block].0;

		for arg in args {
			let value = b.add_arg(arg.ty.node.ty);
			match arg.pat.node {
				PatKind::Binding(b) => {
					self.vars.insert_at(b.binding, (block, value));
				},
			}
		}

		b.finalize_args();

		block
	}

	pub fn build(&mut self) -> DenseMap<Block, BasicBlock> {
		let blocks = std::mem::take(&mut self.blocks);
		blocks
			.into_iter()
			.map(|(block, (mut block_builder, finalized))| {
				(block, {
					assert!(finalized, "block not finalized");

					block_builder.finalize_args();
					block_builder.build()
				})
			})
			.collect()
	}
}
