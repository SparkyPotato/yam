use cstree::GreenNode;
pub use cstree::{TextRange, TextSize};
use petgraph::{graph::NodeIndex, Graph};

use crate::builder::TreeBuilder;

pub mod builder;
pub mod intern;
pub mod kind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileId(NodeIndex);

pub struct Files {
	graph: Graph<GreenNode, ()>,
	root: FileId,
}

impl Files {
	pub fn new() -> Self {
		Self {
			graph: Graph::new(),
			root: FileId(NodeIndex::default()),
		}
	}

	pub fn add(&mut self, builder: TreeBuilder, parent: Option<FileId>) -> FileId {
		let node = builder.finish();
		let node_index = self.graph.add_node(node);

		if let Some(parent) = parent {
			self.graph.add_edge(parent.0, node_index, ());
		} else {
			assert_eq!(self.graph.node_count(), 1, "root already exists");
			self.root = FileId(node_index);
		}

		FileId(node_index)
	}
}
