pub use cstree::{
	green::{GreenNode, GreenToken},
	text::{TextRange, TextSize},
	util::NodeOrToken,
};

pub use crate::generated::{ast, kind::SyntaxKind, token, AstElement, AstNode, AstToken, OptionNameExt};
pub mod builder;
mod generated;

pub type SyntaxNode = cstree::syntax::SyntaxNode<SyntaxKind>;
pub type SyntaxToken = cstree::syntax::SyntaxToken<SyntaxKind>;
pub type SyntaxElement = cstree::syntax::SyntaxElement<SyntaxKind>;
pub type SyntaxElementRef<'a> = cstree::syntax::SyntaxElementRef<'a, SyntaxKind>;
pub type SyntaxNodeChildren<'a> = cstree::syntax::SyntaxNodeChildren<'a, SyntaxKind>;

pub type ResolvedNode = cstree::syntax::ResolvedNode<SyntaxKind>;
pub type ResolvedToken = cstree::syntax::ResolvedToken<SyntaxKind>;
pub type ResolvedElement = cstree::syntax::ResolvedElement<SyntaxKind>;
