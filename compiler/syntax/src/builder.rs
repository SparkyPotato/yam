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

pub struct TreeBuilder<'c> {
	builder: GreenNodeBuilder<'c, 'static, &'static text::Interner>,
	node_depth: usize,
}

impl<'c> TreeBuilder<'c> {
	pub fn new(context: &'c mut TreeBuilderContext) -> Self {
		Self {
			builder: GreenNodeBuilder::with_cache(&mut context.cache),
			node_depth: 0,
		}
	}
}

impl TreeBuilder<'_> {
	pub fn token(&mut self, kind: SyntaxKind, text: &str) { self.builder.token(kind.into(), text) }

	pub fn node_depth(&self) -> usize { self.node_depth }

	pub fn start_node(&mut self, kind: SyntaxKind) {
		self.builder.start_node(kind.into());
		self.node_depth += 1;
	}

	pub fn finish_node(&mut self) {
		self.node_depth -= 1;
		self.builder.finish_node();
	}

	pub fn finish_node_at(&mut self, node_depth: usize) {
		while self.node_depth > node_depth {
			self.finish_node();
		}
	}

	pub fn checkpoint(&self) -> Checkpoint { Checkpoint(self.builder.checkpoint()) }

	pub fn start_node_at(&mut self, checkpoint: Checkpoint, kind: SyntaxKind) {
		self.builder.start_node_at(checkpoint.0, kind.into());
		self.node_depth += 1;
	}

	pub fn finish(self) -> GreenNode { self.builder.finish().0 }
}

#[repr(transparent)]
#[derive(Clone, Copy, Debug)]
pub struct Checkpoint(cstree::Checkpoint);
