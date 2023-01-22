use diagnostics::DiagSink;
pub use syntax;
use syntax::{ast::File, builder::TreeBuilderContext, AstNode, SyntaxNode};

use crate::parse::Parser;

mod api;
mod helpers;
mod parse;
#[cfg(test)]
mod tests;

pub struct ParseContext {
	ctx: TreeBuilderContext,
}

impl ParseContext {
	pub fn new() -> Self {
		Self {
			ctx: TreeBuilderContext::new(),
		}
	}

	pub fn parse(&mut self, source: &str) -> (File, DiagSink<()>) {
		let (builder, diags) = Parser::new(source, &mut self.ctx).parse();
		let root = builder.finish();
		let file = File::cast(SyntaxNode::new_root(root)).unwrap();
		(file, diags)
	}
}
