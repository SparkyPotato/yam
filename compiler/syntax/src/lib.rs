pub use cstree::{GreenNode, GreenToken, NodeOrToken, TextRange, TextSize};

pub use crate::kind::SyntaxKind;

pub mod builder;
mod kind;

pub type SyntaxNode = cstree::SyntaxNode<Lang>;
pub type SyntaxToken = cstree::SyntaxToken<Lang>;
pub type SyntaxElement = cstree::SyntaxElement<Lang>;
pub type SyntaxElementRef<'a> = cstree::SyntaxElementRef<'a, Lang>;

pub type ResolvedNode = cstree::ResolvedNode<Lang>;
pub type ResolvedToken = cstree::ResolvedToken<Lang>;
pub type ResolvedElement = cstree::ResolvedElement<Lang>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Lang;

impl cstree::Language for Lang {
	type Kind = SyntaxKind;

	fn kind_from_raw(raw: cstree::SyntaxKind) -> Self::Kind { raw.into() }

	fn kind_to_raw(kind: Self::Kind) -> cstree::SyntaxKind { kind.into() }
}

impl From<SyntaxKind> for cstree::SyntaxKind {
	fn from(v: SyntaxKind) -> Self { Self(v as _) }
}

impl From<cstree::SyntaxKind> for SyntaxKind {
	fn from(v: cstree::SyntaxKind) -> Self {
		assert!(v.0 < SyntaxKind::__Last as _);
		unsafe { std::mem::transmute(v.0) }
	}
}
