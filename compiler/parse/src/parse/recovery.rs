use std::{convert::Infallible, fmt::Display, num::NonZeroUsize, ops::FromResidual};

use lex::{token::TokenKind, T};

use crate::parse::Parser;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum ParseRule {
	Item,
	Param,
	Field,
	Struct,
	Fn,
	Enum,
	EnumVariant,
	TypeAlias,
	Import,
	Static,
	ImportTree,
	Attribute,
	Type,
	Block,
	Expr,
	MatchArm,
	Repeat,
	List,
}

pub trait Rule: Copy {
	fn rule(&self) -> ParseRule;

	fn parse(self, p: &mut Parser) -> Recovery;
}

#[must_use]
#[derive(Copy, Clone)]
pub struct Recovery(Option<NonZeroUsize>);

impl Recovery {
	pub fn ok() -> Self { Self(None) }

	pub fn to(to: usize) -> Self { Self(Some(NonZeroUsize::new(to + 1).unwrap())) }

	pub fn get(self) -> usize {
		self.0
			.map(|n| n.get() - 1)
			.expect("Recovery::get called on Recovery::ok")
	}

	pub fn check(self, p: &mut Parser) -> Result<(), Recovery> {
		if matches!(self.0, Some(n) if p.rule_stack.len() != n.get()) {
			let last = p.rule_stack.last().unwrap();
			p.api.finish_node_at(last.node_depth);
			Err(self)
		} else {
			Ok(())
		}
	}
}

impl FromResidual<Result<Infallible, Recovery>> for Recovery {
	fn from_residual(residual: Result<Infallible, Recovery>) -> Self { residual.unwrap_err() }
}

impl ParseRule {
	pub fn start(&self) -> &'static [TokenKind] {
		match self {
			Self::Item => &[T![pub], T![struct], T![enum], T![fn], T![static], T![type], T![import]],
			Self::Param | Self::Field => &[T![ident]],
			Self::Struct => &[T![struct]],
			Self::Fn => &[T![fn]],
			Self::Enum => &[T![enum]],
			Self::EnumVariant => &[T![ident]],
			Self::TypeAlias => &[T![type]],
			Self::Import => &[T![import]],
			Self::Static => &[T![static]],
			Self::ImportTree => &[T![ident], T!['{']],
			Self::Attribute => &[T![@]],
			Self::Type => &[T!['['], T![extern], T![fn], T![_], T![ident], T![.], T![*]],
			Self::Block => &[T!['{']],
			Self::Expr | Self::MatchArm => &[
				T!['('],
				T!['{'],
				T![bool],
				T![char],
				T![float],
				T![int],
				T![string],
				T![ident],
				T![.],
				T![break],
				T![return],
				T![continue],
				T![loop],
				T![while],
				T![for],
				T![if],
				T![let],
				T![match],
				T![-],
				T![!],
				T![&],
				T![*],
			],
			Self::Repeat => unreachable!("Repeat is not a parse rule"),
			Self::List => unreachable!("List is not a parse rule"),
		}
	}

	pub fn end(&self) -> &'static [(TokenKind, bool)] {
		match self {
			Self::Item => &[(T![;], true), (T!['}'], true)],
			Self::Param => &[(T![,], false), (T![')'], false)],
			Self::Field => &[(T![,], false), (T!['}'], false)],
			Self::Struct | Self::Fn | Self::Enum => &[(T!['}'], true)],
			Self::EnumVariant => &[(T![,], false), (T!['}'], false)],
			Self::TypeAlias | Self::Import | Self::Static => &[(T![;], true)],
			Self::ImportTree => &[(T!['}'], true), (T![;], false)],
			Self::Attribute => &[
				(T![pub], false),
				(T![struct], false),
				(T![enum], false),
				(T![static], false),
				(T![type], false),
				(T![import], false),
			],
			Self::Type => &[
				(T![,], false),
				(T![;], false),
				(T![')'], false),
				(T![=], false),
				(T!['}'], false),
			],
			Self::Block => &[(T!['}'], true)],
			Self::Expr => &[(T![,], false), (T![;], false), (T![')'], false), (T!['}'], false)],
			Self::MatchArm => &[(T![=>], false), (T!['}'], false), (T![,], false)],
			Self::List => &[],
			Self::Repeat => unreachable!("Repeat is not a parse rule"),
		}
	}
}

impl Display for ParseRule {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(
			f,
			"{}",
			match self {
				Self::Item => "item",
				Self::Param => "function parameter",
				Self::Field => "struct field",
				Self::Struct => "struct",
				Self::Fn => "function",
				Self::Enum => "enum",
				Self::EnumVariant => "enum variant",
				Self::TypeAlias => "type alias",
				Self::Import => "import",
				Self::Static => "static",
				Self::ImportTree => "import tree",
				Self::Attribute => "attribute",
				Self::Type => "type",
				Self::Block => "block",
				Self::Expr => "expression",
				Self::MatchArm => "match arm",
				Self::Repeat => unreachable!("Repeat is not a parse rule"),
				Self::List => unreachable!("List is not a parse rule"),
			}
		)
	}
}
