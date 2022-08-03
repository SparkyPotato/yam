use std::collections::{HashMap, HashSet};

use hir::{
	ctx::LocalRef,
	hir::{Arg, PatKind},
	types::Type,
};
use id::{DenseMap, DenseMapBuilder, Id};

use crate::ssir::*;

#[derive(Debug)]
struct BlockMeta {
	jumped_to_from: Vec<Block>,
	vars: HashMap<LocalRef, (Value, bool)>,
	unresolved_values: HashMap<Value, LocalRef>,
	temp_args_locals: Vec<LocalRef>,
	unknown_vars: u32,
}

pub struct FnBuilder {
	curr_block: Block,
	blocks: DenseMapBuilder<Block, BlockBuilder>,
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
		let id = self.blocks.add(BlockBuilder::new());
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

	pub fn add_var(&mut self, var: LocalRef, ty: Type, value: Value) {
		let meta = &mut self.metadata[self.curr_block];
		meta.vars.insert(var, (value, false));

		self.locals.insert_at(var, ty);
	}

	pub fn mutate_var(&mut self, var: LocalRef, value: Value) {
		let meta = &mut self.metadata[self.curr_block];
		meta.vars.insert(var, (value, false));
	}

	pub fn get_var(&mut self, var: LocalRef) -> Value {
		let meta = &mut self.metadata[self.curr_block];

		meta.vars
			.entry(var)
			.or_insert_with(|| {
				let id = Value::unresolved(meta.unknown_vars);
				meta.unresolved_values.insert(id, var);
				meta.unknown_vars += 1;
				(id, false)
			})
			.0
	}

	pub fn add_arg(&mut self, ty: Type) -> Value { self.blocks[self.curr_block].add_arg(ty) }

	pub fn instr(&mut self, instr: InstrKind, ty: Type) -> Value {
		debug_assert!(self.curr_block != Block::UNKNOWN);

		let block = &mut self.blocks[self.curr_block];

		let jumping_to = match &instr {
			InstrKind::Jump { to, .. } | InstrKind::JumpIf { to, .. } => Some(*to),
			_ => None,
		};

		let value = block.instr(Instr { kind: instr, ty });

		if let Some(to) = jumping_to {
			self.metadata[to].jumped_to_from.push(self.curr_block);
		}

		value
	}

	pub fn fn_init_block(&mut self, args: impl IntoIterator<Item = Arg>) -> Block {
		let block = self.add_block();

		let meta = &mut self.metadata[block];
		let b = &mut self.blocks[block];

		for arg in args {
			let value = b.add_arg(arg.ty.node.ty.clone());
			match arg.pat.node {
				PatKind::Binding(b) => {
					self.locals.insert_at(b.binding, arg.ty.node.ty);
					meta.vars.insert(b.binding, (value, false));
				},
			}
		}

		block
	}

	pub fn build(&mut self) -> DenseMap<Block, BasicBlock> {
		let blocks: Vec<_> = self.blocks.iter().map(|x| x.0).collect();
		for &block in blocks.iter() {
			self.create_block_args(block);
		}

		for (block, b) in self.blocks.iter_mut() {
			let added_args = b.added_args;
			b.finalize_args();
			for (_, (value, ignore)) in self.metadata[block].vars.iter_mut() {
				if value.is_resolved() && !*ignore {
					*value = Value::from_id(value.id() + added_args);
				}
			}
		}

		for &block in blocks.iter() {
			self.pass_args(block);
		}

		let blocks = std::mem::take(&mut self.blocks);
		blocks
			.into_iter()
			.map(|(block, block_builder)| (block, block_builder.build()))
			.collect()
	}

	fn create_block_args(&mut self, block: Block) {
		fn finalize(
			block: Block, locals: &DenseMapBuilder<LocalRef, Type>, value: Value,
			meta: &mut DenseMapBuilder<Block, BlockMeta>, blocks: &mut DenseMapBuilder<Block, BlockBuilder>,
		) {
			fn add_arg_to_prec(
				stack: &mut HashSet<Block>, r: LocalRef, ty: Type, block: Block,
				meta: &mut DenseMapBuilder<Block, BlockMeta>, blocks: &mut DenseMapBuilder<Block, BlockBuilder>,
			) {
				for b in meta[block].jumped_to_from.clone() {
					if stack.contains(&b) {
						return;
					}
					stack.insert(b);

					let m = &mut meta[b];
					if m.vars.get(&r).is_none() {
						let value = blocks[b].add_arg_unfinalized(ty.clone());
						m.vars.insert(r, (value, true));
						m.temp_args_locals.push(r);
					}

					add_arg_to_prec(stack, r, ty.clone(), block, meta, blocks);
				}
			}

			if !value.is_resolved() {
				let m = &mut meta[block];
				let l = m.unresolved_values[&value];

				if !m.vars[&l].0.is_resolved() {
					let ty = locals[l].clone();

					let value = blocks[block].add_arg_unfinalized(ty.clone());
					m.temp_args_locals.push(l);
					m.vars.insert(l, (value, true));

					let mut stack = HashSet::new();
					stack.insert(block);
					add_arg_to_prec(&mut stack, l, ty, block, meta, blocks);
				}
			}
		}

		for (_, instr) in self.blocks[block].instrs.clone().into_iter() {
			match instr.kind {
				InstrKind::Void => {},
				InstrKind::Literal(_) => {},
				InstrKind::Global(_) => {},
				InstrKind::Call { target, args } => {
					finalize(block, &self.locals, target, &mut self.metadata, &mut self.blocks);
					for arg in args {
						finalize(block, &self.locals, arg, &mut self.metadata, &mut self.blocks);
					}
				},
				InstrKind::Cast(value) => {
					finalize(block, &self.locals, value, &mut self.metadata, &mut self.blocks);
				},
				InstrKind::Unary { value, .. } => {
					finalize(block, &self.locals, value, &mut self.metadata, &mut self.blocks);
				},
				InstrKind::Binary { left, right, .. } => {
					finalize(block, &self.locals, left, &mut self.metadata, &mut self.blocks);
					finalize(block, &self.locals, right, &mut self.metadata, &mut self.blocks);
				},
				InstrKind::Jump { args, .. } => {
					for arg in args {
						finalize(block, &self.locals, arg, &mut self.metadata, &mut self.blocks);
					}
				},
				InstrKind::JumpIf { cond, args, .. } => {
					finalize(block, &self.locals, cond, &mut self.metadata, &mut self.blocks);
					for arg in args {
						finalize(block, &self.locals, arg, &mut self.metadata, &mut self.blocks);
					}
				},
				InstrKind::Ret(value) => {
					finalize(block, &self.locals, value, &mut self.metadata, &mut self.blocks);
				},
			}
		}
	}

	fn pass_args(&mut self, block: Block) {
		fn resolve(block: Block, value: &mut Value, meta: &DenseMapBuilder<Block, BlockMeta>) {
			if !value.is_resolved() {
				let meta = &meta[block];
				let l = meta.unresolved_values[&value];
				*value = meta.vars[&l].0;
			}
		}

		fn add_args(from: Block, to: Block, args: &mut Vec<Value>, meta: &DenseMapBuilder<Block, BlockMeta>) {
			let from_meta = &meta[from];
			let to_meta = &meta[to];
			args.extend(to_meta.temp_args_locals.iter().map(|x| from_meta.vars[x].0));
		}

		for (_, instr) in self.blocks[block].instrs.iter_mut() {
			match &mut instr.kind {
				InstrKind::Void => {},
				InstrKind::Literal(_) => {},
				InstrKind::Global(_) => {},
				InstrKind::Call { target, args } => {
					resolve(block, target, &self.metadata);
					for arg in args {
						resolve(block, arg, &self.metadata);
					}
				},
				InstrKind::Cast(value) => resolve(block, value, &self.metadata),
				InstrKind::Unary { value, .. } => resolve(block, value, &self.metadata),
				InstrKind::Binary { left, right, .. } => {
					resolve(block, left, &self.metadata);
					resolve(block, right, &self.metadata);
				},
				InstrKind::Jump { to, args, .. } => {
					for arg in args.iter_mut() {
						resolve(block, arg, &self.metadata);
					}

					add_args(block, *to, args, &self.metadata);
				},
				InstrKind::JumpIf { to, cond, args, .. } => {
					resolve(block, cond, &self.metadata);
					for arg in args.iter_mut() {
						resolve(block, arg, &self.metadata);
					}

					add_args(block, *to, args, &self.metadata);
				},
				InstrKind::Ret(value) => {
					resolve(block, value, &self.metadata);
				},
			}
		}
	}
}
