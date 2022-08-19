use std::marker::PhantomData;

pub use cstree::{GreenNode, GreenToken};
use cstree::{GreenNodeBuilder, NodeCache};

use crate::{intern::TextIntern, kind::SyntaxKind};

#[repr(transparent)]
pub struct TreeBuilderContext<'i> {
	cache: NodeCache<'i, TextIntern>,
}

impl TreeBuilderContext<'static> {
	pub fn new() -> Self {
		Self {
			cache: NodeCache::from_interner(TextIntern::new()),
		}
	}

	pub fn from_intern(interner: TextIntern) -> Self {
		Self {
			cache: NodeCache::from_interner(interner),
		}
	}

	pub fn finalize(self) -> TextIntern { self.cache.into_interner().unwrap() }
}

impl<'i> TreeBuilderContext<'i> {
	pub fn with_intern(interner: &'i mut TextIntern) -> Self {
		Self {
			cache: NodeCache::with_interner(interner),
		}
	}
}

impl TreeBuilderContext<'_> {
	#[inline]
	pub fn intern(&mut self) -> &mut TextIntern { self.cache.interner_mut() }
}

#[repr(transparent)]
pub struct TreeBuilder<'c, 'i> {
	builder: GreenNodeBuilder<'c, 'i, TextIntern>,
}

impl<'c, 'i> TreeBuilder<'c, 'i> {
	pub fn new(context: &'c mut TreeBuilderContext<'i>) -> Self {
		Self {
			builder: GreenNodeBuilder::with_cache(&mut context.cache),
		}
	}
}

impl TreeBuilder<'_, '_> {
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
