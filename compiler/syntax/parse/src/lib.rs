pub use ast;
use ast::Module;
use diag::Diagnostics;
use intern::Id;
pub use syntax;
use syntax::{builder::TreeBuilderContext, SyntaxNode};

use crate::{api::Api, parse::Parser};

mod api;
mod helpers;
mod parse;
#[cfg(test)]
mod tests;

#[derive(Clone)]
pub struct Cst {
	node: SyntaxNode,
	file: Id<str>,
}

impl Cst {
	pub fn parse(builder: &mut TreeBuilderContext, file: Id<str>, source: &str, diags: &mut Diagnostics) -> Self {
		let parser = Parser {
			api: Api::new(file, source, builder),
			diags,
			silent: false,
		};

		let builder = parser.parse();

		Self {
			node: SyntaxNode::new_root(builder.finish()),
			file,
		}
	}

	pub fn to_module(&self) -> Module { Module::new(self.node.clone(), self.file) }
}
