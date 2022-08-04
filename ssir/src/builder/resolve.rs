use std::collections::HashSet;

use hir::{ctx::LocalRef, types::Type};
use id::{DenseMapBuilder, Id};

use crate::{
	builder::{BlockMeta, FnBuilder, LocalStack},
	ssir::*,
};

impl FnBuilder {
	pub fn create_block_args(&mut self, block: Block) {
		fn finalize(
			block: Block, id: InstrId, locals: &DenseMapBuilder<LocalRef, Type>, value: Value,
			meta: &mut DenseMapBuilder<Block, BlockMeta>, blocks: &mut DenseMapBuilder<Block, BasicBlock>,
		) {
			fn add_arg_to_prec(
				stack: &mut HashSet<Block>, r: LocalRef, ty: Type, block: Block,
				meta: &mut DenseMapBuilder<Block, BlockMeta>, blocks: &mut DenseMapBuilder<Block, BasicBlock>,
			) {
				for b in meta[block].jumped_to_from.clone() {
					if stack.contains(&b) {
						return;
					}
					stack.insert(b);

					let m = &mut meta[b];
					if m.vars.get(&r).is_none() {
						let value = blocks[b].arg(ty.clone());
						m.vars.insert(
							r,
							LocalStack {
								values: vec![(InstrId::from_id(0), value)],
							},
						);
						m.temp_args_locals.push(r);
					}

					add_arg_to_prec(stack, r, ty.clone(), block, meta, blocks);
				}
			}

			if !value.is_resolved() {
				let m = &mut meta[block];
				let l = m.unresolved_values[&value];

				let stack = m.vars.get_mut(&l).unwrap();
				let (index, (_, var)) = stack
					.values
					.iter()
					.enumerate()
					.rev()
					.find(|(_, (x, _))| x.id() < id.id())
					.unwrap();
				if !var.is_resolved() {
					let ty = locals[l].clone();

					let value = blocks[block].arg(ty.clone());
					m.temp_args_locals.push(l);
					stack.values[index].1 = value;

					let mut stack = HashSet::new();
					stack.insert(block);
					add_arg_to_prec(&mut stack, l, ty, block, meta, blocks);
				}
			}
		}

		for (id, instr) in self.blocks[block].clone_instrs().into_iter() {
			match instr {
				Instr::Value { instr, .. } => match instr {
					ValueInstr::Literal(_) => {},
					ValueInstr::Global(_) => {},
					ValueInstr::Call { target, args } => {
						finalize(block, id, &self.locals, target, &mut self.metadata, &mut self.blocks);
						for arg in args {
							finalize(block, id, &self.locals, arg, &mut self.metadata, &mut self.blocks);
						}
					},
					ValueInstr::Cast(value) => {
						finalize(block, id, &self.locals, value, &mut self.metadata, &mut self.blocks);
					},
					ValueInstr::Unary { value, .. } => {
						finalize(block, id, &self.locals, value, &mut self.metadata, &mut self.blocks);
					},
					ValueInstr::Binary { left, right, .. } => {
						finalize(block, id, &self.locals, left, &mut self.metadata, &mut self.blocks);
						finalize(block, id, &self.locals, right, &mut self.metadata, &mut self.blocks);
					},
				},
				Instr::NonValue(instr) => match instr {
					NonValueInstr::Jump { args, .. } => {
						for arg in args {
							finalize(block, id, &self.locals, arg, &mut self.metadata, &mut self.blocks);
						}
					},
					NonValueInstr::JumpIf { cond, args, .. } => {
						finalize(block, id, &self.locals, cond, &mut self.metadata, &mut self.blocks);
						for arg in args {
							finalize(block, id, &self.locals, arg, &mut self.metadata, &mut self.blocks);
						}
					},
					NonValueInstr::Ret(value) => {
						if let Some(value) = value {
							finalize(block, id, &self.locals, value, &mut self.metadata, &mut self.blocks);
						}
					},
				},
			}
		}
	}

	pub fn pass_args(&mut self, block: Block) {
		fn resolve(block: Block, id: InstrId, value: &mut Value, meta: &DenseMapBuilder<Block, BlockMeta>) {
			if !value.is_resolved() {
				let meta = &meta[block];
				let l = meta.unresolved_values[&value];
				*value = meta.vars[&l]
					.values
					.iter()
					.rev()
					.find(|x| x.0.id() < id.id())
					.unwrap()
					.1;
			}
		}

		fn add_args(
			from: Block, to: Block, id: InstrId, args: &mut Vec<Value>, meta: &DenseMapBuilder<Block, BlockMeta>,
		) {
			let from_meta = &meta[from];
			let to_meta = &meta[to];
			args.extend(to_meta.temp_args_locals.iter().map(|x| {
				from_meta.vars[x]
					.values
					.iter()
					.rev()
					.find(|x| x.0.id() < id.id())
					.unwrap()
					.1
			}));
		}

		for (id, instr) in self.blocks[block].instrs_mut() {
			match instr {
				Instr::Value { instr, .. } => match instr {
					ValueInstr::Literal(_) => {},
					ValueInstr::Global(_) => {},
					ValueInstr::Call { target, args } => {
						resolve(block, id, target, &self.metadata);
						for arg in args {
							resolve(block, id, arg, &self.metadata);
						}
					},
					ValueInstr::Cast(value) => resolve(block, id, value, &self.metadata),
					ValueInstr::Unary { value, .. } => resolve(block, id, value, &self.metadata),
					ValueInstr::Binary { left, right, .. } => {
						resolve(block, id, left, &self.metadata);
						resolve(block, id, right, &self.metadata);
					},
				},
				Instr::NonValue(instr) => match instr {
					NonValueInstr::Jump { to, args } => {
						for arg in args.iter_mut() {
							resolve(block, id, arg, &self.metadata);
						}

						add_args(block, *to, id, args, &self.metadata);
					},
					NonValueInstr::JumpIf { to, cond, args } => {
						resolve(block, id, cond, &self.metadata);
						for arg in args.iter_mut() {
							resolve(block, id, arg, &self.metadata);
						}

						add_args(block, *to, id, args, &self.metadata);
					},
					NonValueInstr::Ret(value) => {
						if let Some(value) = value {
							resolve(block, id, value, &self.metadata);
						}
					},
				},
			}
		}
	}
}
