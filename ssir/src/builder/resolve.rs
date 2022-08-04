use crate::builder::FnBuilder;

impl FnBuilder {
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
