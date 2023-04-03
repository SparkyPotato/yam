use diagnostics::FileSpan;

use crate::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken};

pub mod ast;
pub mod kind;
pub mod token;

pub trait AstNode: Sized {
	fn cast(node: SyntaxNode) -> Option<Self>;
}

pub trait AstToken: Sized {
	fn cast(tok: SyntaxToken) -> Option<Self>;
}

pub trait AstElement: Sized {
	fn can_cast(kind: SyntaxKind) -> bool;

	fn cast(elem: SyntaxElement) -> Option<Self>;

	fn span(&self) -> FileSpan;
}

fn node_children<'a, T: 'a + AstNode>(node: &'a SyntaxNode) -> impl Iterator<Item = T> + 'a {
	node.children().cloned().filter_map(T::cast)
}

fn token_children<'a, T: 'a + AstToken>(node: &'a SyntaxNode) -> impl Iterator<Item = T> + 'a {
	node.children_with_tokens()
		.filter_map(|it| it.into_token().cloned().and_then(T::cast))
}

pub struct TokenTree(SyntaxNode);
impl AstNode for TokenTree {
	fn cast(node: SyntaxNode) -> Option<Self> {
		if Self::can_cast(node.kind()) {
			Some(Self(node))
		} else {
			None
		}
	}
}
impl AstElement for TokenTree {
	fn can_cast(kind: SyntaxKind) -> bool { kind == SyntaxKind::TokenTree }

	fn cast(elem: SyntaxElement) -> Option<Self> { AstNode::cast(elem.into_node()?) }

	fn span(&self) -> FileSpan {
		let range = self.0.text_range();
		FileSpan {
			start: range.start().into(),
			end: range.end().into(),
			relative: (),
		}
	}
}
