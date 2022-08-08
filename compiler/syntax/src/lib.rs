use ::intern::Id;
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
	file_names: Vec<Id<str>>,
	root: FileId,
}

impl Files {
	pub fn new() -> Self {
		Self {
			graph: Graph::new(),
			file_names: Vec::new(),
			root: FileId(NodeIndex::default()),
		}
	}

	pub fn add(&mut self, name: Id<str>, builder: TreeBuilder, parent: Option<FileId>) -> FileId {
		let node = builder.finish();
		let node_index = self.graph.add_node(node);

		if let Some(parent) = parent {
			self.graph.add_edge(parent.0, node_index, ());
		} else {
			assert_eq!(self.graph.node_count(), 1, "root already exists");
			self.root = FileId(node_index);
		}

		let id = self.file_names.len();
		self.file_names.push(name);
		assert_eq!(node_index.index(), id, "petgraph changed index generation");

		FileId(node_index)
	}
}
