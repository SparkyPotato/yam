use diagnostics::FileDiagnostic;
pub use syntax;
use syntax::{ast::File, builder::TreeBuilderContext, AstElement, SyntaxNode};

use crate::parse::Parser;

mod api;
mod helpers;
mod parse;
#[cfg(test)]
mod tests;

#[derive(Default)]
pub struct ParseContext {
	ctx: TreeBuilderContext,
}

impl ParseContext {
	pub fn new() -> Self { Self::default() }

	pub fn parse_file(&mut self, source: &str) -> (File, Vec<FileDiagnostic>) {
		let (builder, diagnostics) = Parser::new(source, &mut self.ctx).parse();
		let root = builder.finish();
		let file = File::cast(SyntaxNode::new_root(root).into()).unwrap();
		(file, diagnostics)
	}
}
