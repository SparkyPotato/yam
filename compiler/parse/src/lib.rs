use diagnostics::{Diagnostic, FilePath};
pub use syntax;
use syntax::{ast::File, builder::TreeBuilderContext, AstNode, SyntaxNode};
use verde::{query, storage, Ctx, Id, Tracked};

use crate::parse::Parser;

mod api;
mod helpers;
mod parse;
#[cfg(test)]
mod tests;

#[storage]
pub struct Storage(FileData, ParseResult, Diagnostic<()>, parse_file);

#[derive(Tracked, Clone)]
pub struct FileData {
	#[id]
	pub path: FilePath,
	pub source: String,
}

impl PartialEq for FileData {
	fn eq(&self, _: &Self) -> bool { false }
}

impl Eq for FileData {}

#[derive(Tracked, PartialEq, Eq, Hash)]
pub struct ParseResult {
	#[id]
	pub path: FilePath,
	pub file: File,
}

pub struct ParseContext {
	ctx: TreeBuilderContext,
}

impl ParseContext {
	pub fn new() -> Self {
		Self {
			ctx: TreeBuilderContext::new(),
		}
	}
}

#[query]
pub fn parse_file(db: &Ctx, #[ignore] ctx: &mut ParseContext, source: Id<FileData>) -> ParseResult {
	let source = db.get(source);
	let builder = Parser::new(&source.source, &mut ctx.ctx, db).parse();
	let root = builder.finish();
	let file = File::cast(SyntaxNode::new_root(root)).unwrap();
	ParseResult {
		path: source.path,
		file,
	}
}
