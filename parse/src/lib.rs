use diag::Diagnostics;
use intern::Id;
use lex::{
	token::{Delim, TokenKind},
	T,
};
use syntax::{
	builder::{TreeBuilder, TreeBuilderContext},
	kind::SyntaxKind,
	FileId,
	Files,
};

use crate::{api::Api, helpers::select};

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
		silent: false,
	};

	let builder = parser.parse();

	files.add(builder, parent);
}

struct Parser<'i, 'c, 's> {
	api: Api<'i, 'c, 's>,
	diags: &'c mut Diagnostics,
	silent: bool,
}

impl<'i, 'c> Parser<'i, 'c, '_> {
	fn parse(mut self) -> TreeBuilder<'c, 'i> {
		let b = self.api.start_node(SyntaxKind::File);

		while !self.is_empty() {
			self.item();
		}

		self.api.finish_node(b);

		self.api.finish()
	}
}

impl Parser<'_, '_, '_> {
	fn item(&mut self) {
		let b = self.api.start_node(SyntaxKind::Item);

		self.attributes();

		let vis = self.api.peek();
		if matches!(vis.kind, T![pub]) {
			let b = self.api.start_node(SyntaxKind::Visibility);
			self.api.bump();
			self.api.finish_node(b);
		}

		select! {
			self {
				T![struct] => self.struct_(),
				T![fn] => self.fn_(),
			}
		}

		self.api.finish_node(b);

		self.recover_at_kw();
	}

	fn struct_(&mut self) {
		let b = self.api.start_node(SyntaxKind::Struct);

		let toks = [T![ident], T![<], T![where], T!['{']];

		self.expect(T![struct], &toks);
		self.expect(T![ident], &toks[1..]);
		self.generics_decl();
		self.where_();

		self.expect(T!['{'], &[]);
		self.comma_sep_list(T!['}'], |this| this.struct_field());
		self.expect(T!['}'], &[]);

		self.api.finish_node(b);
	}

	fn fn_(&mut self) {
		let b = self.api.start_node(SyntaxKind::Fn);

		let toks = [T![ident], T![<], T!['('], T![where], T!['{']];

		self.expect(T![fn], &toks);
		self.expect(T![ident], &toks[1..]);
		self.generics_decl();

		self.expect(T!['('], &toks[3..]);
		self.comma_sep_list(T![')'], |this| {
			let b = this.api.start_node(SyntaxKind::Arg);

			this.pat(&toks[3..]);
			this.expect(T![:], &toks[4..]);
			this.ty();

			this.api.finish_node(b);
		});
		self.expect(T![')'], &toks[4..]);

		self.where_();

		self.expect(T!['{'], &[]);
		self.expect(T!['}'], &[]);

		self.api.finish_node(b);
	}

	fn generics_decl(&mut self) {
		let b = self.api.start_node(SyntaxKind::Generics);

		if matches!(self.api.peek().kind, T![<]) {
			self.api.bump();
			self.comma_sep_list(T![>], |this| {
				let b = this.api.start_node(SyntaxKind::Generic);

				this.expect(T![ident], &[T![:]]);
				if matches!(this.api.peek().kind, T![:]) {
					this.api.bump();
					this.generic_bound();
				}

				this.api.finish_node(b);
			});
			self.expect(T![>], &[]);
		}

		self.api.finish_node(b);
	}

	fn generics(&mut self) {
		let b = self.api.start_node(SyntaxKind::Generics);

		if matches!(self.api.peek().kind, T![<]) {
			self.api.bump();
			self.comma_sep_list(T![>], |this| {
				this.ty();
			});
			self.expect(T![>], &[]);
		}

		self.api.finish_node(b);
	}

	fn generic_bound(&mut self) {
		let b = self.api.start_node(SyntaxKind::GenericBound);

		self.ty();

		self.api.finish_node(b);
	}

	fn attributes(&mut self) {
		let b = self.api.start_node(SyntaxKind::Attributes);

		while matches!(self.api.peek().kind, T![@]) {
			let b = self.api.start_node(SyntaxKind::Attribute);

			self.api.bump();
			self.expect(T![ident], &[T!['(']]);
			self.token_tree(Delim::Paren);

			self.api.finish_node(b);
		}

		self.api.finish_node(b);
	}

	fn struct_field(&mut self) {
		let b = self.api.start_node(SyntaxKind::StructField);

		self.expect(T![ident], &[T![:]]);
		self.expect(T![:], &[T![ident], T!['(']]);
		self.ty();

		self.api.finish_node(b);
	}

	fn where_(&mut self) {
		let b = self.api.start_node(SyntaxKind::Where);

		if matches!(self.api.peek().kind, T![where]) {
			self.api.bump();
			self.comma_sep_list(T!['{'], |this| {
				let b = this.api.start_node(SyntaxKind::WhereClause);

				this.ty();
				this.expect(T![:], &[T![ident]]);
				this.generic_bound();

				this.api.finish_node(b);
			});
		}

		self.api.finish_node(b);
	}

	fn ty(&mut self) {
		let t = self.api.start_node(SyntaxKind::Type);

		select! {
			self {
				T![ident] => self.ty_path(),
				T![.] => self.ty_path(),
				T!['('] => {
					let b = self.api.start_node(SyntaxKind::Tuple);
					self.api.bump();
					self.comma_sep_list(T![')'], |this| this.ty());
					self.expect(T![')'], &[]);
					self.api.finish_node(b);
				},
			}
		}

		self.api.finish_node(t);
	}

	fn ty_path(&mut self) {
		let b = self.api.start_node(SyntaxKind::Path);

		if matches!(self.api.peek().kind, T![.]) {
			self.api.bump();
		}

		loop {
			self.expect(T![ident], &[T![<], T![ident], T![.]]);
			self.generics();
			if matches!(self.api.peek().kind, T![.]) {
				self.api.bump();
			} else {
				break;
			}
		}

		self.api.finish_node(b);
	}

	fn path(&mut self) {
		let b = self.api.start_node(SyntaxKind::Path);

		select! {
			self {
				T![ident] => {},
				T![.] => self.api.bump(),
			}
		}

		loop {
			self.expect(T![ident], &[T![ident], T![.]]);
			if matches!(self.api.peek().kind, T![.]) {
				self.api.bump();
			} else {
				break;
			}
		}

		self.api.finish_node(b);
	}

	fn pat(&mut self, next: &[TokenKind]) {
		let b = self.api.start_node(SyntaxKind::Pat);

		select! {
			self {
				T![ident] => self.api.bump(),
				T!['('] => {
					let b = self.api.start_node(SyntaxKind::Tuple);
					self.api.bump();
					self.comma_sep_list(T![')'], |this| this.pat(next));
					self.expect(T![')'], next);
					self.api.finish_node(b);
				},
			}
		}

		self.api.finish_node(b);
	}

	fn token_tree(&mut self, delim: Delim) {
		let b = self.api.start_node(SyntaxKind::TokenTree);
		let span = self.expect(T![ldelim: delim], &[T![rdelim: delim]]);

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
