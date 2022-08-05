use diag::Diagnostics;
use intern::Id;
use lex::{token::Delim, T};
use syntax::{
	builder::{TreeBuilder, TreeBuilderContext},
	kind::SyntaxKind,
	FileId,
	Files,
};

use crate::{api::Api, helpers::one_of};

mod api;
mod helpers;
#[cfg(test)]
mod tests;

pub trait FileLoader {}

pub fn parse_file(
	ctx: &mut TreeBuilderContext, files: &mut Files, diags: &mut Diagnostics, parent: Option<FileId>,
	file_name: Id<str>, source: &str, recursive: bool,
) {
	let parser = Parser {
		api: Api::new(file_name, source, ctx),
		diags,
	};

	let builder = parser.parse();

	files.add(builder, parent);
}

struct Parser<'i, 'c, 's> {
	api: Api<'i, 'c, 's>,
	diags: &'c mut Diagnostics,
}

impl<'i, 'c> Parser<'i, 'c, '_> {
	pub fn parse(mut self) -> TreeBuilder<'c, 'i> {
		let b = self.api.start_node(SyntaxKind::File);

		while !self.is_empty() {
			self.item();
		}

		self.api.finish_node(b);

		self.api.finish()
	}
}

impl Parser<'_, '_, '_> {
	pub fn item(&mut self) {
		let b = self.api.start_node(SyntaxKind::Item);

		self.attributes();

		let vis = self.api.peek();
		if matches!(vis.kind, T![pub]) {
			let b = self.api.start_node(SyntaxKind::Visibility);
			self.api.bump();
			self.api.finish_node(b);
		}

		one_of! {
			self {
				"struct": T![struct] => self.struct_(),
				"fn": T![fn] => {},
			}
		}

		self.api.finish_node(b);
	}

	pub fn struct_(&mut self) {
		let b = self.api.start_node(SyntaxKind::Struct);

		self.expect(T![struct]);
		self.expect(T![ident]);
		self.generics();

		self.api.finish_node(b);
	}

	pub fn generics(&mut self) {
		if matches!(self.api.peek().kind, T![<]) {
			let b = self.api.start_node(SyntaxKind::Generics);

			self.api.bump();
			self.comma_sep_list(T![>], |this| {
				let b = this.api.start_node(SyntaxKind::Generic);

				this.expect(T![ident]);
				if matches!(this.api.peek().kind, T![:]) {
					this.api.bump();
					this.generic_bound();
				}

				this.api.finish_node(b);
			});

			self.api.finish_node(b);
		}
	}

	pub fn generic_bound(&mut self) {
		let b = self.api.start_node(SyntaxKind::GenericBound);

		// TODO: parse generic bound

		self.api.finish_node(b);
	}

	pub fn attributes(&mut self) {
		let b = self.api.start_node(SyntaxKind::Attributes);

		while matches!(self.api.peek().kind, T![@]) {
			let b = self.api.start_node(SyntaxKind::Attribute);

			self.api.bump();
			self.expect(T![ident]);
			self.token_tree(Delim::Paren);

			self.api.finish_node(b);
		}

		self.api.finish_node(b);
	}

	pub fn token_tree(&mut self, delim: Delim) {
		let b = self.api.start_node(SyntaxKind::TokenTree);
		let span = self.expect(T![ldelim: delim]);

		let mut delim_stack = vec![(delim, span)];
		loop {
			if delim_stack.is_empty() {
				break;
			}

			let curr = self.api.peek();
			self.api.bump();
			match curr.kind {
				T![ldelim: delim] => delim_stack.push((delim, curr.span)),
				T![rdelim: delim] => {
					let (seen, span) = delim_stack.pop().unwrap();
					if seen != delim {
						self.diags.push(
							span.error("mismatched delimiter")
								.label(span.mark())
								.label(curr.span.mark()),
						);

						// Try to get to the delimiter we expect
						while let Some((seen, _)) = delim_stack.pop() {
							if delim == seen {
								break;
							}
						}
					}
				},
				T![eof] => {
					self.diags
						.push(curr.span.error("unexpected <eof>").label(curr.span.mark()));
					break;
				},
				_ => {},
			}
		}

		self.api.finish_node(b);
	}
}
