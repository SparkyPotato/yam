pub use cstree::{GreenNode, GreenToken, NodeOrToken, TextRange, TextSize};

use crate::kind::Lang;

pub mod builder;
pub mod intern;
pub mod kind;

pub type SyntaxNode = cstree::SyntaxNode<Lang>;
pub type SyntaxToken = cstree::SyntaxToken<Lang>;
pub type SyntaxElement = cstree::SyntaxElement<Lang>;
pub type SyntaxElementRef<'a> = cstree::SyntaxElementRef<'a, Lang>;

pub type ResolvedNode = cstree::ResolvedNode<Lang>;
pub type ResolvedToken = cstree::ResolvedToken<Lang>;
pub type ResolvedElement = cstree::ResolvedElement<Lang>;
