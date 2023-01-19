use crate::{SyntaxKind, SyntaxNode, SyntaxToken};

pub mod ast;
pub mod kind;
pub mod token;

trait AstNode: Sized {
	fn can_cast(kind: SyntaxKind) -> bool;

	fn cast(node: SyntaxNode) -> Option<Self>;
}

trait AstToken: Sized {
	fn can_cast(kind: SyntaxKind) -> bool;

	fn cast(tok: SyntaxToken) -> Option<Self>;
}

fn token<T: AstToken>(node: &SyntaxNode) -> Option<T> {
	node.children_with_tokens()
		.find_map(|it| it.into_token().cloned().and_then(|x| T::cast(x.clone())))
}

fn node<T: AstNode>(node: &SyntaxNode) -> Option<T> { node.children().cloned().find_map(T::cast) }

fn node_children<'a, T: 'a + AstNode>(node: &'a SyntaxNode) -> impl Iterator<Item = T> + 'a {
	node.children().cloned().filter_map(T::cast)
}

#[allow(dead_code)]
fn token_children<'a, T: 'a + AstToken>(node: &'a SyntaxNode) -> impl Iterator<Item = T> + 'a {
	node.children_with_tokens()
		.filter_map(|it| it.into_token().cloned().and_then(T::cast))
}
