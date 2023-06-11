#![feature(try_trait_v2)]

use diagnostics::FileDiagnostic;
pub use syntax;
use syntax::{ast::File, builder::TreeBuilderContext, AstElement, SyntaxNode};
use tracing::{span, Level};

use crate::parse::Parser;

mod api;
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
		let s = span!(Level::TRACE, "parse_file");
		let _e = s.enter();

		let (builder, diagnostics) = Parser::new(source, &mut self.ctx).parse();
		let root = builder.finish();
		let file = File::cast(SyntaxNode::new_root(root).into()).unwrap();
		(file, diagnostics)
	}
}
