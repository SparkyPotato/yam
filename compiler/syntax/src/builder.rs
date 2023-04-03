use std::marker::PhantomData;

use cstree::{GreenNode, GreenNodeBuilder, NodeCache};

use crate::SyntaxKind;

#[repr(transparent)]
pub struct TreeBuilderContext {
	cache: NodeCache<'static, &'static text::Interner>,
}

impl TreeBuilderContext {
	pub fn new() -> Self {
		Self {
			cache: NodeCache::from_interner(text::get_interner()),
		}
	}
}

impl Default for TreeBuilderContext {
	fn default() -> Self { Self::new() }
}

#[repr(transparent)]
pub struct TreeBuilder<'c> {
	builder: GreenNodeBuilder<'c, 'static, &'static text::Interner>,
}

impl<'c> TreeBuilder<'c> {
	pub fn new(context: &'c mut TreeBuilderContext) -> Self {
		Self {
			builder: GreenNodeBuilder::with_cache(&mut context.cache),
		}
	}
}

impl TreeBuilder<'_> {
	pub fn token(&mut self, kind: SyntaxKind, text: &str) { self.builder.token(kind.into(), text) }

	pub fn start_node(&mut self, kind: SyntaxKind) -> Branch {
		self.builder.start_node(kind.into());
		Branch(PhantomData)
	}

	pub fn finish_node(&mut self, branch: Branch) {
		// Defuse the drop bomb
		std::mem::forget(branch);
		self.builder.finish_node();
	}

	pub fn checkpoint(&self) -> Checkpoint { Checkpoint(self.builder.checkpoint()) }

	pub fn start_node_at(&mut self, checkpoint: Checkpoint, kind: SyntaxKind) -> Branch {
		self.builder.start_node_at(checkpoint.0, kind.into());
		Branch(PhantomData)
	}

	pub fn finish(self) -> GreenNode { self.builder.finish().0 }
}

pub struct Branch(PhantomData<()>);

impl Drop for Branch {
	fn drop(&mut self) {
		panic!("TreeBuilder::end_node() was never called in this scope");
	}
}

#[repr(transparent)]
#[derive(Clone, Copy, Debug)]
pub struct Checkpoint(cstree::Checkpoint);
