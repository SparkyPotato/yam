use std::{collections::hash_map::Entry, fmt::Write};

use diagnostics::{FileDiagnostic, Span};
use lex::{token::TokenKind, T};
use rustc_hash::FxHashMap;
use syntax::builder::{TreeBuilder, TreeBuilderContext};

use crate::{
	api::Api,
	parse::{
		recovery::{ParseRule, Recovery, Rule},
		rules::Item,
	},
};

mod recovery;
mod rules;

#[derive(Copy, Clone, Debug)]
struct RuleData {
	rule: ParseRule,
	/// If we're still trying to parse the leading token of the rule.
	beginning: bool,
	/// CST node depth at the beginning of the rule.
	node_depth: usize,
}

impl PartialEq for RuleData {
	fn eq(&self, other: &Self) -> bool { self.rule == other.rule && self.node_depth == other.node_depth }
}

pub struct Parser<'c, 's> {
	api: Api<'c, 's>,
	diags: Vec<FileDiagnostic>,
	rule_stack: Vec<RuleData>,
}

impl<'c, 's> Parser<'c, 's> {
	pub fn new(source: &'s str, ctx: &'c mut TreeBuilderContext) -> Self {
		Self {
			api: Api::new(source, ctx),
			diags: Vec::new(),
			rule_stack: Vec::new(),
		}
	}

	pub fn parse(mut self) -> (TreeBuilder<'c>, Vec<FileDiagnostic>) {
		self.parse_inner();
		(self.api.finish(), self.diags)
	}

	fn parse_inner(&mut self) {
		let _ = self.repeat(|p| {
			while !p.is_empty() {
				let _ = p.run(Item);
			}

			Recovery::ok()
		});
	}
}

impl Parser<'_, '_> {
	fn run<T: Rule>(&mut self, rule: T) -> Recovery {
		let data = RuleData {
			rule: rule.rule(),
			beginning: true,
			node_depth: self.api.node_depth(),
		};
		self.rule_stack.push(data);
		let recovery = rule.parse(self);
		assert_eq!(data, self.rule_stack.pop().unwrap());
		recovery
	}

	fn repeat(&mut self, f: impl FnOnce(&mut Self) -> Recovery) -> Recovery {
		let data = RuleData {
			rule: ParseRule::Repeat,
			beginning: false,
			node_depth: self.api.node_depth(),
		};
		self.rule_stack.push(data);

		let ret = f(self);
		assert_eq!(data, self.rule_stack.pop().unwrap());
		ret
	}

	fn is_empty(&self) -> bool { matches!(self.api.peek().kind, T![eof]) }

	/// Expect a token of a certain kind, and recover if it's not found.
	fn expect(&mut self, kind: TokenKind) -> Recovery {
		let token = self.api.peek();

		if token.kind != kind {
			self.diags.push(
				token
					.span
					.error({
						let last = self.rule_stack.last_mut().expect("rule stack is empty");
						if last.beginning {
							last.beginning = false;
							format!("expected {}", last.rule)
						} else {
							format!("expected {}", SyntaxKind::from(kind))
						}
					})
					.label(token.span.label(format!("found {}", SyntaxKind::from(token.kind)))),
			);
			self.recover()
		} else {
			self.api.bump();
			Recovery::ok()
		}
	}

	fn comma_sep_list(&mut self, end: TokenKind, rule: impl Rule) -> Recovery {
		self.rule_stack.push(RuleData {
			rule: ParseRule::List,
			beginning: false,
			node_depth: self.api.node_depth(),
		});
		let ret = self.comma_sep_list_inner(end, rule);
		self.rule_stack.pop();
		ret
	}

	fn comma_sep_list_inner(&mut self, end: TokenKind, rule: impl Rule) -> Recovery {
		let mut end_comma = true;
		loop {
			let next = self.api.peek();
			if next.kind == end {
				break;
			}

			if !end_comma {
				self.diags.push(
					next.span
						.error(format!("expected `,` or {}", SyntaxKind::from(end)))
						.label(next.span.label(format!("found {}", SyntaxKind::from(next.kind)))),
				);

				p!(self.recover());
			}

			p!(self.run(rule));

			let next = self.api.peek();
			if next.kind == T![,] {
				self.api.bump();
				end_comma = true;
			} else {
				end_comma = false;
			}
		}
		Recovery::ok()
	}

	fn recover(&mut self) -> Recovery {
		self.api.start_node(SyntaxKind::Error);

		let map = self.generate_recovery_map();

		let ret = loop {
			let curr = self.api.peek();
			if let Some(x) = map.get(&curr.kind) {
				if x.consume {
					self.api.bump();
				}
				break x.recovery;
			}

			match curr.kind {
				T!['('] => {
					while !matches!(self.api.peek().kind, T![')'] | T![eof]) {
						self.api.bump();
					}
				},
				T!['{'] => {
					while !matches!(self.api.peek().kind, T!['}'] | T![eof]) {
						self.api.bump();
					}
				},
				T!['['] => {
					while !matches!(self.api.peek().kind, T![']'] | T![eof]) {
						self.api.bump();
					}
				},
				T![eof] => break Recovery::to(0),
				_ => self.api.bump(),
			}
		};

		self.api.finish_node();

		ret
	}

	fn generate_recovery_map(&mut self) -> FxHashMap<TokenKind, RecoveryTarget> {
		let mut map: FxHashMap<TokenKind, RecoveryTarget> = FxHashMap::default();
		let mut insert = |kind: TokenKind, target: RecoveryTarget| match map.entry(kind) {
			Entry::Occupied(mut o) => {
				if o.get().recovery.get() < target.recovery.get() {
					o.insert(target);
				}
			},
			Entry::Vacant(v) => {
				v.insert(target);
			},
		};

		let mut iter = self.rule_stack.windows(2).enumerate().rev();
		while let Some((parent_i, [parent, child, ..])) = iter.next() {
			if matches!(parent.rule, ParseRule::Repeat | ParseRule::List) {
				if matches!(parent.rule, ParseRule::List) {
					insert(
						T![,],
						RecoveryTarget {
							recovery: Recovery::to(parent_i),
							consume: false,
						},
					);
				}
				for &kind in child.rule.start() {
					insert(
						kind,
						RecoveryTarget {
							recovery: Recovery::to(parent_i),
							consume: false,
						},
					);
				}

				iter.next();
			}

			let nearest_repeat_or_list = self.rule_stack[..=parent_i]
				.iter()
				.enumerate()
				.rev()
				.find_map(|(i, x)| matches!(x.rule, ParseRule::Repeat | ParseRule::List).then_some(i));

			for &(kind, consume) in child.rule.end() {
				if !consume {
					insert(
						kind,
						RecoveryTarget {
							recovery: Recovery::to(nearest_repeat_or_list.expect("no repeat or list")),
							consume,
						},
					);
				} else {
					insert(
						kind,
						RecoveryTarget {
							recovery: Recovery::to(parent_i),
							consume,
						},
					);
				}
			}
		}

		map
	}
}

struct RecoveryTarget {
	recovery: Recovery,
	consume: bool,
}

fn fmt_kinds(kinds: &[TokenKind]) -> String {
	let mut s = String::new();
	for (i, kind) in kinds.iter().copied().enumerate() {
		if i != 0 {
			s.push_str(", ");
		}
		write!(s, "{}", SyntaxKind::from(kind)).unwrap();
	}
	s
}

macro_rules! p {
	($self:ident . $f:ident $args:tt) => {
		$self . $f $args . check($self)?
	};

	($f:ident ($self:ident $(, $($args:tt),+)? $(,)?)) => {
		$f($self, $($($args)*)?).check($self)?
	};
}
pub(crate) use p;

macro_rules! select {
	($self:ident, $(T!$kind:tt => $value:expr,)*) => {{
		let tok = $self.api.peek();
		let expected = [$(T!$kind,)*];
		match tok.kind {
			$(T!$kind => $value,)*
			_ => {
				$self.diags.push(
					tok.span
						.error({
							let last = $self.rule_stack.last_mut().expect("rule stack is empty");
							if last.beginning {
								last.beginning = false;
								format!("expected {}", last.rule)
							} else {
								format!("expected one of: {}", crate::parse::fmt_kinds(&expected))
							}
						})
						.label(tok.span.label(format!("found {}", SyntaxKind::from(tok.kind)))),
				);
				p!($self.recover());
			},
		}
	}};
}
pub(crate) use select;
use syntax::SyntaxKind;
