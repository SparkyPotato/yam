use diagnostics::{test::emit_test, FilePath};
use expect_test::Expect;
use pretty_assertions::assert_eq;
use syntax::{builder::TreeBuilderContext, ResolvedNode};

use crate::Parser;

mod happy;
mod recovery;

fn harness(source: &str, ast: Expect, diagnostics: Expect) {
	let mut ctx = TreeBuilderContext::new();
	let (builder, out) = Parser::new(source, &mut ctx).parse();
	let node = builder.finish();

	let resolved = ResolvedNode::new_root_with_resolver(node, text::get_interner());

	let text = resolved.text();
	assert_eq!(text, source, "CST is not lossless");

	let debug = fmt(&resolved);
	ast.assert_eq(&debug);

	let diags = emit_test(source, out, &FilePath::new("test"));
	diagnostics.assert_eq(&diags);
}

fn fmt(node: &ResolvedNode) -> String {
	let mut s = node.debug(node.resolver().as_ref(), true);
	s.pop();
	s
}
