use diagnostics::FileSpan;
use text::Text;

use crate::{
	ast::{Name, PathSegment},
	SyntaxElement,
	SyntaxElementRef,
	SyntaxKind,
	SyntaxNode,
	SyntaxToken,
};

pub mod ast;
pub mod kind;
pub mod token;

pub trait AstNode: Sized {}

pub trait AstToken: Sized {
	fn text(&self) -> Text;
}

pub trait AstElement: Sized {
	fn can_cast(kind: SyntaxKind) -> bool;

	fn cast(elem: SyntaxElement) -> Option<Self>;

	fn span(&self) -> FileSpan;

	fn inner(self) -> SyntaxElement;
}

fn children<'a, T: 'a + AstElement>(node: &'a SyntaxNode) -> impl Iterator<Item = T> + 'a {
	node.children_with_tokens()
		.map(|x| match x {
			SyntaxElementRef::Node(node) => SyntaxElement::Node(node.clone()),
			SyntaxElementRef::Token(token) => SyntaxElement::Token(token.clone()),
		})
		.filter_map(T::cast)
}

pub struct TokenTree(SyntaxNode);
impl AstNode for TokenTree {}
impl AstElement for TokenTree {
	fn can_cast(kind: SyntaxKind) -> bool { kind == SyntaxKind::TokenTree }

	fn cast(elem: SyntaxElement) -> Option<Self> {
		let node = elem.into_node()?;
		Self::can_cast(node.kind()).then(|| Self(node))
	}

	fn span(&self) -> FileSpan {
		let range = self.0.text_range();
		FileSpan {
			start: range.start().into(),
			end: range.end().into(),
			relative: (),
		}
	}

	fn inner(self) -> SyntaxElement { self.0.into() }
}

pub trait OptionNameExt {
	fn text(&self) -> Option<Text>;
}

impl OptionNameExt for Option<Name> {
	fn text(&self) -> Option<Text> { self.as_ref().and_then(|x| x.text()) }
}

impl OptionNameExt for Option<PathSegment> {
	fn text(&self) -> Option<Text> {
		self.as_ref()
			.and_then(|x| match x {
				PathSegment::Name(name) => Some(name),
				PathSegment::Dot(_) => None,
			})
			.and_then(|x| x.ident())
			.map(|x| x.text())
	}
}

impl Name {
	pub fn text(&self) -> Option<Text> { self.ident().map(|x| x.text()) }
}

impl PathSegment {
	pub fn name(&self) -> Option<Name> {
		match self {
			PathSegment::Name(name) => Some(name.clone()),
			PathSegment::Dot(_) => None,
		}
	}
}
