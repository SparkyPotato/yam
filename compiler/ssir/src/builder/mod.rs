use std::collections::HashMap;

use hir::{
	ctx::LocalRef,
	hir::{Arg, PatKind},
	types::Type,
};
use id::{DenseMapBuilder, Id};

use crate::ssir::*;

mod instr_api;
mod resolve;

#[derive(Debug)]
struct LocalStack {
	values: Vec<(InstrId, Value)>,
}

#[derive(Debug)]
struct BlockMeta {
	jumped_to_from: Vec<Block>,
	vars: HashMap<LocalRef, LocalStack>,
	unresolved_values: HashMap<Value, LocalRef>,
	temp_args_locals: Vec<LocalRef>,
	unknown_vars: u32,
}

pub struct FnBuilder {
	curr_block: Block,
	blocks: DenseMapBuilder<Block, BasicBlock>,
	metadata: DenseMapBuilder<Block, BlockMeta>,
	locals: DenseMapBuilder<LocalRef, Type>,
}

impl FnBuilder {
	pub fn new() -> Self {
		Self {
			curr_block: Block::UNKNOWN,
			blocks: DenseMapBuilder::new(),
			metadata: DenseMapBuilder::new(),
			locals: DenseMapBuilder::new(),
		}
	}

	pub fn reset(&mut self) {
		self.curr_block = Block::UNKNOWN;
		self.blocks.reset();
		self.metadata.reset();
		self.locals.reset();
	}

	pub fn add_block(&mut self) -> Block {
		let id = self.blocks.add(BasicBlock::new());
		self.metadata.insert_at(
			id,
			BlockMeta {
				jumped_to_from: Vec::new(),
				vars: HashMap::new(),
				unresolved_values: HashMap::new(),
				temp_args_locals: Vec::new(),
				unknown_vars: 0,
			},
		);
		id
	}

	pub fn set_block(&mut self, block: Block) { self.curr_block = block; }

	pub fn add_var(&mut self, var: LocalRef, ty: Type) {
		let meta = &mut self.metadata[self.curr_block];
		meta.vars.insert(var, LocalStack { values: Vec::new() });

		self.locals.insert_at(var, ty);
	}

	pub fn set_var(&mut self, var: LocalRef, value: Option<Value>) {
		if let Some(value) = value {
			let meta = &mut self.metadata[self.curr_block];
			let stack = meta.vars.get_mut(&var).unwrap();
			stack
				.values
				.push((self.blocks[self.curr_block].value_to_instr(value), value));
		}
	}

	pub fn get_var(&mut self, var: LocalRef) -> Option<Value> {
		let meta = &mut self.metadata[self.curr_block];

		let stack = meta.vars.entry(var).or_insert_with(|| {
			let id = Value::unresolved(meta.unknown_vars);
			meta.unresolved_values.insert(id, var);
			meta.unknown_vars += 1;

			LocalStack {
				values: vec![(InstrId::from_id(0), id)],
			}
		});

		Some(stack.values.last().unwrap().1)
	}

	pub fn add_arg(&mut self, ty: Type) -> Value { self.blocks[self.curr_block].arg(ty) }

	pub fn fn_init_block(&mut self, args: impl IntoIterator<Item = Arg>) -> Block {
		let block = self.add_block();

		let meta = &mut self.metadata[block];
		let b = &mut self.blocks[block];

		for arg in args {
			let value = b.arg(arg.ty.node.ty.clone());
			match arg.pat.node {
				PatKind::Binding(b) => {
					self.locals.insert_at(b.binding, arg.ty.node.ty);
					meta.vars.insert(
						b.binding,
						LocalStack {
							values: vec![(InstrId::from_id(0), value)],
						},
					);
				},
			}
		}

		block
	}

	pub fn build(&mut self) -> DenseMapBuilder<Block, BasicBlock> {
		let blocks: Vec<_> = self.blocks.iter().map(|x| x.0).collect();
		for &block in blocks.iter() {
			self.create_block_args(block);
		}

		for &block in blocks.iter() {
			self.pass_args(block);
		}

		std::mem::take(&mut self.blocks)
	}
}
