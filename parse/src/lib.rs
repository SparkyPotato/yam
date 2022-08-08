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
) -> FileId {
	let parser = Parser {
		api: Api::new(file_name, source, ctx),
		diags,
		silent: false,
	};

	let builder = parser.parse();

	files.add(file_name, builder, parent)
}

struct Parser<'i, 'c, 's> {
	api: Api<'i, 'c, 's>,
	diags: &'c mut Diagnostics,
	silent: bool,
}

impl<'i, 'c> Parser<'i, 'c, '_> {
	fn parse(mut self) -> TreeBuilder<'c, 'i> {
		while !self.is_empty() {
			self.item();
			self.silent = false;
		}

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
				T![enum] => self.enum_(),
				T![fn] => self.fn_(),
				T![extern] => {
					self.api.bump();
					if matches!(self.api.peek().kind, T![lit(str)]) {
						self.api.bump();
					}
					self.fn_();
				},
				T![trait] => self.trait_(),
				T![type] => self.type_alias(),
			}
		}

		self.api.finish_node(b);
	}

	fn struct_(&mut self) {
		let b = self.api.start_node(SyntaxKind::Struct);

		let toks = [T![ident], T![<], T![where], T!['{'], T!['('], T![;]];

		self.expect(T![struct], &toks);
		self.expect(T![ident], &toks[1..]);
		self.generics_decl();
		self.where_();

		let v = self.api.start_node(SyntaxKind::EnumVariant);
		let ret = self.enum_variant();
		self.api.finish_node(v);

		if ret {
			self.expect(T![;], &[]);
		}

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

		if matches!(self.api.peek().kind, T!['{']) {
			self.block();
		} else {
			self.expect(T![;], &[]);
		}

		self.api.finish_node(b);
	}

	fn enum_(&mut self) {
		let b = self.api.start_node(SyntaxKind::Enum);

		let toks = [T![ident], T![<], T!['{']];

		self.expect(T![enum], &toks);
		self.expect(T![ident], &toks[1..]);
		self.generics_decl();
		self.where_();
		self.expect(T!['{'], &[]);
		self.comma_sep_list(T!['}'], |this| {
			let b = this.api.start_node(SyntaxKind::EnumVariant);

			this.expect(T![ident], &[T![ident], T!['{'], T!['(']]);
			this.enum_variant();

			this.api.finish_node(b);
		});
		self.expect(T!['}'], &[]);

		self.api.finish_node(b);
	}

	fn trait_(&mut self) {
		let b = self.api.start_node(SyntaxKind::Trait);

		let toks = [T![ident], T![<], T![where], T!['{']];

		self.expect(T![trait], &toks);
		self.expect(T![ident], &toks[1..]);
		self.generics_decl();
		self.where_();

		let v = self.api.start_node(SyntaxKind::Mod);

		self.expect(T!['{'], &[]);

		while !matches!(self.api.peek().kind, T!['}'] | T![eof]) {
			self.item();
		}

		self.expect(T!['}'], &[]);

		self.api.finish_node(v);

		self.api.finish_node(b);
	}

	fn type_alias(&mut self) {
		let b = self.api.start_node(SyntaxKind::TypeAlias);

		let toks = [T![ident], T![<], T![where], T!['{']];

		self.expect(T![type], &toks);
		self.expect(T![ident], &toks[1..]);
		self.generics_decl();
		self.where_();
		self.expect(T![=], &toks[3..]);
		self.ty();

		self.api.finish_node(b);
	}

	fn enum_variant(&mut self) -> bool {
		let ret = match self.api.peek().kind {
			T!['{'] => {
				let b = self.api.start_node(SyntaxKind::Fields);

				self.api.bump();
				self.comma_sep_list(T!['}'], |this| this.field());
				self.expect(T!['}'], &[]);

				self.api.finish_node(b);
				false
			},
			T!['('] => {
				self.tuple();
				true
			},
			_ => true,
		};

		ret
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

	fn field(&mut self) {
		let b = self.api.start_node(SyntaxKind::Field);

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
				T![_] => self.api.bump(),
				T![ident] => self.ty_path(),
				T![.] => self.ty_path(),
				T![type] => {
					self.api.bump();
					self.expect(T!['('], &[]);
					self.expr(true);
					self.expect(T![')'], &[]);
				},
				T!['('] => self.tuple(),
			}
		}

		self.api.finish_node(t);
	}

	fn tuple(&mut self) {
		let b = self.api.start_node(SyntaxKind::Tuple);
		self.expect(T!['('], &[]);
		self.comma_sep_list(T![')'], |this| this.ty());
		self.expect(T![')'], &[]);
		self.api.finish_node(b);
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

	fn pat(&mut self, next: &[TokenKind]) {
		let b = self.api.start_node(SyntaxKind::Pat);

		select! {
			self {
				T![_] => self.api.bump(),
				T![lit(int)] => self.api.bump(),
				T![lit(char)] => self.api.bump(),
				T![lit(str)] => self.api.bump(),
				T![lit(bool)] => self.api.bump(),
				T![lit(float)] => self.api.bump(),
				T![ident] => self.variant_pat(next),
				T![.] => {
					let b = self.api.peek_n(1);
					if matches!(b.kind, T![.]) {
						self.api.bump();
						self.api.bump();
					} else {
						self.variant_pat(next)
					}
				},
				T![mut] => {
					self.api.bump();
					self.expect(T![ident], &[]);
				},
				T!['('] => self.tuple_pat(next),
			}
		}

		self.api.finish_node(b);
	}

	fn variant_pat(&mut self, next: &[TokenKind]) {
		let b = self.api.start_node(SyntaxKind::EnumVariant);

		self.ty_path();
		match self.api.peek().kind {
			T!['('] => self.tuple_pat(next),
			T!['{'] => self.field_pat(next),
			_ => {},
		}

		self.api.finish_node(b);
	}

	fn tuple_pat(&mut self, next: &[TokenKind]) {
		let b = self.api.start_node(SyntaxKind::Tuple);

		self.api.bump();
		self.comma_sep_list(T![')'], |this| this.pat(next));
		self.expect(T![')'], next);

		self.api.finish_node(b);
	}

	fn field_pat(&mut self, next: &[TokenKind]) {
		let b = self.api.start_node(SyntaxKind::Fields);

		self.api.bump();
		self.comma_sep_list(T!['}'], |this| {
			let b = this.api.start_node(SyntaxKind::Field);

			match this.api.peek().kind {
				T![ident] => {
					this.api.bump();
					if matches!(this.api.peek().kind, T![:]) {
						this.api.bump();
						this.pat(next)
					}
				},
				_ => this.pat(next),
			}

			this.api.finish_node(b);
		});
		self.expect(T!['}'], next);

		self.api.finish_node(b);
	}

	fn block(&mut self) {
		let b = self.api.start_node(SyntaxKind::Block);

		self.expect(T!['{'], &[T![pub], T![struct], T![enum], T![fn]]);

		loop {
			match self.api.peek().kind {
				T!['}'] => break,
				T![pub] | T![struct] | T![enum] | T![fn] => self.item(),
				_ => {
					self.expr(true);
					if matches!(self.api.peek().kind, T![;]) {
						self.api.bump();
					}
				},
			}
		}

		self.expect(T!['}'], &[]);

		self.api.finish_node(b);
	}

	fn expr(&mut self, struct_allowed: bool) { self.expr_inner(0, struct_allowed); }

	fn expr_inner(&mut self, min_bp: u8, struct_allowed: bool) {
		let b = self.api.start_node(SyntaxKind::Expr);

		let infix = self.api.checkpoint();
		let prefix = self.api.checkpoint();
		let postfix = self.api.checkpoint();

		if let Some((_, bp, f)) = self.prefix() {
			let b = self.api.start_node_at(prefix, SyntaxKind::Prefix);
			f(self, bp, struct_allowed);
			self.api.finish_node(b);
		} else {
			let b = self.api.start_node(SyntaxKind::Expr);
			self.atom();
			self.api.finish_node(b);
		};

		loop {
			if let Some((bp, (), f, kind)) = self.postfix(struct_allowed) {
				if bp < min_bp {
					break;
				}

				let b = self.api.start_node_at(postfix, kind);
				f(self, bp, struct_allowed);
				self.api.finish_node(b);

				continue;
			}

			if let Some((l_bp, r_bp, f)) = self.infix() {
				if l_bp < min_bp {
					break;
				}

				let b = self.api.start_node_at(infix, SyntaxKind::Infix);
				f(self, r_bp, struct_allowed);
				self.api.finish_node(b);

				continue;
			}

			break;
		}

		self.api.finish_node(b);
	}

	fn atom(&mut self) {
		let tok = self.api.peek();
		match tok.kind {
			T!['('] => self.tuple_or_paren_expr(),
			T!['{'] => self.block(),
			T![lit(_)] => self.api.bump(),
			T![ident] => self.api.bump(),
			T![break] => {
				let b = self.api.start_node(SyntaxKind::Break);

				self.api.bump();
				if !matches!(self.api.peek().kind, T![,] | T![;] | T!['}'] | T![')'] | T![']']) {
					self.expr(true);
				}

				self.api.finish_node(b);
			},
			T![continue] => {
				let b = self.api.start_node(SyntaxKind::Continue);
				self.api.bump();
				self.api.finish_node(b);
			},
			T![return] => {
				let b = self.api.start_node(SyntaxKind::Return);

				self.api.bump();
				if !matches!(self.api.peek().kind, T![,] | T![;] | T!['}'] | T![')'] | T![']']) {
					self.expr(true);
				}

				self.api.finish_node(b);
			},
			T![loop] => {
				let b = self.api.start_node(SyntaxKind::Loop);
				self.api.bump();
				self.block();

				if matches!(self.api.peek().kind, T![while]) {
					self.api.bump();
					self.expr(false);
				}

				self.api.finish_node(b);
			},
			T![while] => {
				let b = self.api.start_node(SyntaxKind::While);

				self.api.bump();
				self.expr(false);
				self.block();

				self.api.finish_node(b);
			},
			T![for] => {
				let b = self.api.start_node(SyntaxKind::For);

				self.api.bump();
				self.pat(&[T![in]]);
				self.expect(T![in], &[]);
				self.expr(false);
				self.block();

				self.api.finish_node(b);
			},
			T![if] => {
				let b = self.api.start_node(SyntaxKind::If);

				self.api.bump();
				self.expr(false);
				self.block();
				if matches!(self.api.peek().kind, T![else]) {
					let b = self.api.start_node(SyntaxKind::Else);
					self.api.bump();
					self.expr(false);
					self.api.finish_node(b);
				}

				self.api.finish_node(b);
			},
			T![let] => {
				self.api.bump();
				self.pat(&[T![:], T![=], T![;]]);
				if matches!(self.api.peek().kind, T![:]) {
					self.api.bump();
					self.ty();
				}
				if matches!(self.api.peek().kind, T![=]) {
					self.api.bump();
					self.expr(true);
				}
			},
			T![match] => {
				let b = self.api.start_node(SyntaxKind::Match);
				self.api.bump();
				self.expr(false);
				self.match_arms();
				self.api.finish_node(b);
			},
			_ => {
				if !self.silent {
					let diag = tok.span.error("expected expr");

					self.diags.push(if self.api.is_span_eof(tok.span) {
						diag
					} else {
						diag.label(tok.span.label(format!("found `{}`", tok.kind)))
					});
				}

				self.try_recover(&[]);
			},
		}
	}

	// Should eat the operator.
	fn prefix(&mut self) -> Option<((), u8, fn(&mut Self, u8, bool))> {
		let a = self.api.peek();

		match a.kind {
			T![-] | T![!] | T![*] => {
				let b = self.api.start_node(SyntaxKind::Op);
				self.api.bump();
				self.api.finish_node(b);

				Some(((), 23, Self::expr_inner))
			},
			T![&] => {
				let b = self.api.start_node(SyntaxKind::Op);
				self.api.bump();
				if matches!(self.api.peek().kind, T![mut]) {
					self.api.bump();
				}
				self.api.finish_node(b);

				Some(((), 23, Self::expr_inner))
			},
			_ => None,
		}
	}

	// Should not eat the operator: `f` should.
	fn infix(&mut self) -> Option<(u8, u8, fn(&mut Self, u8, bool))> {
		let a = self.api.peek();

		match a.kind {
			T![=] => {
				let b = self.api.peek_n(1);
				match b.kind {
					T![=] => Some((13, 14, Self::binary_2)),
					_ => Some((2, 1, Self::binary)),
				}
			},
			T![!] => {
				let b = self.api.peek_n(1);
				match b.kind {
					T![=] => Some((13, 14, Self::binary_2)),
					_ => None,
				}
			},
			T![<] => {
				let b = self.api.peek_n(1);
				let (l, r) = match b.kind {
					T![<] => (17, 18),
					T![=] => (15, 16),
					_ => return Some((15, 16, Self::binary)),
				};
				Some((l, r, Self::binary_2))
			},
			T![>] => {
				let b = self.api.peek_n(1);
				let (l, r) = match b.kind {
					T![>] => (17, 18),
					T![=] => (15, 16),
					_ => return Some((15, 16, Self::binary)),
				};
				Some((l, r, Self::binary_2))
			},
			T![&] => {
				let b = self.api.peek_n(1);
				match b.kind {
					T![&] => Some((11, 12, Self::binary_2)),
					T![=] => Some((2, 1, Self::binary_2)),
					_ => Some((5, 6, Self::binary)),
				}
			},
			T![^] => {
				let b = self.api.peek_n(1);
				match b.kind {
					T![=] => Some((2, 1, Self::binary_2)),
					_ => Some((9, 10, Self::binary)),
				}
			},
			T![|] => {
				let b = self.api.peek_n(1);
				match b.kind {
					T![|] => Some((7, 8, Self::binary_2)),
					T![=] => Some((2, 1, Self::binary_2)),
					_ => Some((3, 4, Self::binary)),
				}
			},
			T![+] | T![-] => {
				let b = self.api.peek_n(1);
				match b.kind {
					T![=] => Some((2, 1, Self::binary_2)),
					_ => Some((19, 20, Self::binary)),
				}
			},
			T![*] | T![/] | T![%] => {
				let b = self.api.peek_n(1);
				match b.kind {
					T![=] => Some((2, 1, Self::binary_2)),
					_ => Some((21, 22, Self::binary)),
				}
			},
			_ => None,
		}
	}

	// Should not eat the operator: `f` should.
	fn postfix(&mut self, struct_allowed: bool) -> Option<(u8, (), fn(&mut Self, u8, bool), SyntaxKind)> {
		let a = self.api.peek();

		match a.kind {
			T!['('] => Some((24, (), Self::call, SyntaxKind::Call)),
			T!['['] => Some((24, (), Self::index, SyntaxKind::Index)),
			T![.] => Some((24, (), Self::access, SyntaxKind::Access)),
			T!['{'] if struct_allowed => Some((24, (), Self::struct_lit, SyntaxKind::StructLit)),
			T![:] => Some((24, (), Self::ascript, SyntaxKind::Ascript)),
			_ => None,
		}
	}

	fn binary(&mut self, bp: u8, struct_allowed: bool) {
		let b = self.api.start_node(SyntaxKind::Op);
		self.api.bump();
		self.api.finish_node(b);

		self.expr_inner(bp, struct_allowed);
	}

	fn binary_2(&mut self, bp: u8, struct_allowed: bool) {
		let b = self.api.start_node(SyntaxKind::Op);
		self.api.bump();
		self.api.bump();
		self.api.finish_node(b);

		self.expr_inner(bp, struct_allowed);
	}

	fn call(&mut self, _: u8, _: bool) {
		let b = self.api.start_node(SyntaxKind::Args);

		self.api.bump();
		self.comma_sep_list(T![')'], |this| {
			this.expr(true);
		});
		self.expect(T![')'], &[T![;], T!['}']]);

		self.api.finish_node(b);
	}

	fn index(&mut self, _: u8, _: bool) {
		self.api.bump();
		self.expr(true);
		self.expect(T![']'], &[T![;], T!['}']]);
	}

	fn access(&mut self, _: u8, _: bool) {
		self.api.bump();
		select! {
			self {
				T![ident] => self.api.bump(),
				T![<] => self.generics(),
			}
		}
	}

	fn ascript(&mut self, _: u8, _: bool) {
		self.api.bump();
		self.ty();
	}

	fn struct_lit(&mut self, _: u8, _: bool) {
		self.api.bump();
		self.comma_sep_list(T!['}'], |this| {
			let b = this.api.start_node(SyntaxKind::Field);
			this.expect(T![ident], &[T!['}'], T![,], T![:]]);
			if matches!(this.api.peek().kind, T![:]) {
				this.api.bump();
				this.expr(true);
			}
			this.api.finish_node(b);
		});
		self.expect(T!['}'], &[]);
	}

	fn tuple_or_paren_expr(&mut self) {
		if matches!(self.api.peek_n(1).kind, T![')']) {
			let b = self.api.start_node(SyntaxKind::Tuple);
			self.api.bump();
			self.api.bump();
			self.api.finish_node(b);
		} else {
			let c = self.api.checkpoint();

			self.api.bump();
			self.expr(true);

			select! {
				self {
					T![')'] => {
						let b = self.api.start_node_at(c, SyntaxKind::Paren);
						self.api.bump();
						self.api.finish_node(b);
					},
					T![,] => {
						let b = self.api.start_node_at(c, SyntaxKind::Tuple);
						self.api.bump();

						self.comma_sep_list(T![')'], |this| this.expr(true));
						self.expect(T![')'], &[]);

						self.api.finish_node(b);
					},
				}
			}
		}
	}

	fn match_arms(&mut self) {
		let b = self.api.start_node(SyntaxKind::MatchArms);
		self.expect(T!['{'], &[T![;], T!['}']]);
		self.comma_sep_list(T!['}'], |this| {
			let b = this.api.start_node(SyntaxKind::MatchArm);

			let toks = [T![=], T![>], T![,], T!['}'], T![;]];
			this.pat(&toks);
			this.expect(T![=], &toks[1..]);
			this.expect(T![>], &toks[2..]);
			this.expr(true);

			this.api.finish_node(b);
		});
		self.expect(T!['}'], &[T![;], T!['}']]);
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
