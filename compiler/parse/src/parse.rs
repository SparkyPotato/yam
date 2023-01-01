use lex::{
	token::{Delim, TokenKind},
	T,
};
use syntax::{builder::TreeBuilder, kind::SyntaxKind};

use crate::{api::Api, helpers::select};

pub struct Parser<'c, 's> {
	pub api: Api<'c, 's>,
	pub diags: &'c mut Raw,
	pub silent: bool,
}

impl<'c> Parser<'c, '_> {
	pub fn parse(mut self) -> TreeBuilder<'c> {
		self.parse_inner();
		self.api.finish()
	}

	pub(crate) fn parse_inner(&mut self) {
		while !self.is_empty() {
			self.item();
			self.silent = false;
		}
	}
}

impl Parser<'_, '_> {
	pub(crate) fn item(&mut self) {
		let b = self.api.start_node(SyntaxKind::Item);

		self.attributes();

		if matches!(self.api.peek().kind, T![pub]) {
			let v = self.api.start_node(SyntaxKind::Visibility);
			self.api.bump();
			self.api.finish_node(v);
		}

		select! {
			self {
				T![struct] => self.struct_(),
				T![enum] => self.enum_(),
				T![fn] => {
					let b = self.api.start_node(SyntaxKind::Fn);

					self.fn_();

					self.api.finish_node(b);
				},
				T![extern] => {
					let b = self.api.start_node(SyntaxKind::Fn);

					self.api.bump();
					if matches!(self.api.peek().kind, T![lit(str)]) {
						self.api.bump();
					}
					self.fn_();

					self.api.finish_node(b);
				},
				T![trait] => self.trait_(),
				T![type] => self.type_alias(),
				T![impl] => self.impl_(),
				T![mod] => self.mod_(),
			}
		}

		self.api.finish_node(b);
	}

	pub(crate) fn struct_(&mut self) {
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

	pub(crate) fn fn_(&mut self) {
		let toks = [T![ident], T![<], T!['('], T![where], T!['{']];

		self.expect(T![fn], &toks);
		self.expect(T![ident], &toks[1..]);
		self.generics_decl();

		let a = self.api.start_node(SyntaxKind::Args);
		self.expect(T!['('], &toks[3..]);
		self.comma_sep_list(T![')'], |this| {
			let b = this.api.start_node(SyntaxKind::Arg);

			this.pat(&toks[3..]);
			this.expect(T![:], &toks[4..]);
			this.ty();

			this.api.finish_node(b);
		});
		self.expect(T![')'], &toks[4..]);
		self.api.finish_node(a);

		self.where_();

		if matches!(self.api.peek().kind, T![-]) {
			self.api.bump();
			self.expect(T![>], &[]);
			self.ty();
		}

		if matches!(self.api.peek().kind, T!['{']) {
			self.block();
		} else {
			self.expect(T![;], &[]);
		}
	}

	pub(crate) fn enum_(&mut self) {
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

	pub(crate) fn trait_(&mut self) {
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

	pub(crate) fn impl_(&mut self) {
		let b = self.api.start_node(SyntaxKind::Impl);

		let toks = [T![ident], T![<], T![where], T!['{']];

		self.expect(T![impl], &toks);
		self.generics_decl();
		self.ty();

		if matches!(self.api.peek().kind, T![for]) {
			self.api.bump();
			self.ty();
		}

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

	pub(crate) fn mod_(&mut self) {
		let b = self.api.start_node(SyntaxKind::Mod);

		let toks = [T![ident], T!['{']];

		self.expect(T![mod], &toks);
		self.expect(T![ident], &toks[1..]);
		self.expect(T![;], &[]);

		self.api.finish_node(b);
	}

	pub(crate) fn type_alias(&mut self) {
		let b = self.api.start_node(SyntaxKind::TypeAlias);

		let toks = [T![ident], T![<], T![where], T!['{']];

		self.expect(T![type], &toks);
		self.expect(T![ident], &toks[1..]);
		self.generics_decl();
		self.where_();

		if matches!(self.api.peek().kind, T![=]) {
			self.api.bump();
			self.ty();
		}
		self.expect(T![;], &[]);

		self.api.finish_node(b);
	}

	pub(crate) fn enum_variant(&mut self) -> bool {
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

	pub(crate) fn generics_decl(&mut self) {
		let b = self.api.start_node(SyntaxKind::Generics);

		if matches!(self.api.peek().kind, T![<]) {
			self.api.bump();
			self.comma_sep_list(T![>], |this| {
				let b = this.api.start_node(SyntaxKind::Generic);

				this.expect(T![ident], &[T![:], T![>], T![,]]);
				if matches!(this.api.peek().kind, T![:]) {
					this.api.bump();
					this.generic_bound();
				}

				if matches!(this.api.peek().kind, T![=]) {
					this.api.bump();
					this.ty();
				}

				this.api.finish_node(b);
			});
			self.expect(T![>], &[]);
		}

		self.api.finish_node(b);
	}

	pub(crate) fn generics(&mut self) {
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

	pub(crate) fn generic_bound(&mut self) {
		let b = self.api.start_node(SyntaxKind::Bound);

		if matches!(self.api.peek().kind, T![,] | T!['{'] | T![>]) {
			return;
		}

		self.ty();

		self.api.finish_node(b);
	}

	pub(crate) fn attributes(&mut self) {
		while matches!(self.api.peek().kind, T![@]) {
			let b = self.api.start_node(SyntaxKind::Attribute);

			self.api.bump();
			self.expect(T![ident], &[T!['(']]);
			if matches!(self.api.peek().kind, T!['(']) {
				self.token_tree(Delim::Paren);
			}

			self.api.finish_node(b);
		}
	}

	pub(crate) fn field(&mut self) {
		let b = self.api.start_node(SyntaxKind::Field);

		self.expect(T![ident], &[T![:]]);
		self.expect(T![:], &[T![ident], T!['(']]);
		self.ty();

		self.api.finish_node(b);
	}

	pub(crate) fn where_(&mut self) {
		if matches!(self.api.peek().kind, T![where]) {
			let b = self.api.start_node(SyntaxKind::Where);

			self.api.bump();
			self.comma_sep_list(T!['{'], |this| {
				let b = this.api.start_node(SyntaxKind::WhereClause);

				this.ty();
				this.expect(T![:], &[T![ident]]);
				this.generic_bound();

				this.api.finish_node(b);
			});

			self.api.finish_node(b);
		}
	}

	pub(crate) fn ty(&mut self) {
		let c = self.api.checkpoint();

		self.ty_inner();
		if matches!(self.api.peek().kind, T![+]) {
			let t = self.api.start_node_at(c, SyntaxKind::Type);
			let b = self.api.start_node_at(c, SyntaxKind::SumType);
			self.api.bump();

			loop {
				self.ty_inner();
				if matches!(self.api.peek().kind, T![+]) {
					self.api.bump();
				} else {
					break;
				}
			}

			self.api.finish_node(b);
			self.api.finish_node(t);
		}
	}

	pub(crate) fn ty_inner(&mut self) {
		let t = self.api.start_node(SyntaxKind::Type);

		select! {
			self {
				T![_] => self.api.bump(),
				T![ident] => self.ty_path(),
				T![.] => self.ty_path(),
				T![type] => {
					let b = self.api.start_node(SyntaxKind::TypeOf);

					self.api.bump();
					self.expect(T!['('], &[]);
					self.expr(true);
					self.expect(T![')'], &[]);

					self.api.finish_node(b);
				},
				T![*] => {
					let b = self.api.start_node(SyntaxKind::Ptr);

					self.api.bump();
					select! {
						self {
							T![const] => self.api.bump(),
							T![mut] => self.api.bump(),
						}
					}

					self.ty();

					self.api.finish_node(b);
				},
				T!['('] => self.tuple(),
			}
		}

		self.api.finish_node(t);
	}

	pub(crate) fn tuple(&mut self) {
		let b = self.api.start_node(SyntaxKind::Tuple);
		self.expect(T!['('], &[]);
		self.comma_sep_list(T![')'], |this| this.ty());
		self.expect(T![')'], &[]);
		self.api.finish_node(b);
	}

	pub(crate) fn ty_path(&mut self) {
		let b = self.api.start_node(SyntaxKind::Path);

		if matches!(self.api.peek().kind, T![.]) {
			self.api.bump();
		}

		if matches!(self.api.peek().kind, T![ident]) {
			loop {
				self.expect(T![ident], &[T![<], T![ident], T![.]]);
				self.generics();
				if matches!(self.api.peek().kind, T![.]) {
					self.api.bump();
				} else {
					break;
				}
			}
		}

		self.api.finish_node(b);
	}

	pub(crate) fn pat(&mut self, next: &[TokenKind]) {
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
					self.variant_pat(next);
				},
				T!['('] => self.tuple_pat(next),
			}
		}

		self.api.finish_node(b);
	}

	pub(crate) fn variant_pat(&mut self, next: &[TokenKind]) {
		let b = self.api.start_node(SyntaxKind::EnumVariant);

		self.ty_path();
		match self.api.peek().kind {
			T!['('] => self.tuple_pat(next),
			_ => {},
		}

		self.api.finish_node(b);
	}

	pub(crate) fn tuple_pat(&mut self, next: &[TokenKind]) {
		let b = self.api.start_node(SyntaxKind::Tuple);

		self.api.bump();
		self.comma_sep_list(T![')'], |this| this.pat(next));
		self.expect(T![')'], next);

		self.api.finish_node(b);
	}

	pub(crate) fn block(&mut self) {
		let b = self.api.start_node(SyntaxKind::Block);

		self.expect(T!['{'], &[T![pub], T![struct], T![enum], T![fn]]);

		loop {
			match self.api.peek().kind {
				T!['}'] => break,
				T![pub] | T![struct] | T![enum] | T![fn] => self.item(),
				_ => {
					let c = self.api.checkpoint();
					self.expr(true);
					if matches!(self.api.peek().kind, T![;]) {
						let b = self.api.start_node_at(c, SyntaxKind::SemiExpr);
						self.api.bump();
						self.api.finish_node(b);
					}
				},
			}
		}

		self.expect(T!['}'], &[]);

		self.api.finish_node(b);
	}

	pub(crate) fn expr(&mut self, struct_allowed: bool) { self.expr_inner(0, struct_allowed); }

	pub(crate) fn expr_inner(&mut self, min_bp: u8, struct_allowed: bool) {
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

	pub(crate) fn atom(&mut self) {
		let tok = self.api.peek();
		match tok.kind {
			T!['('] => self.tuple_or_paren_expr(),
			T!['{'] => self.block(),
			T![lit] => self.api.bump(),
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

				self.try_recover(T![eof], &[]);
			},
		}
	}

	// Should eat the operator.
	pub(crate) fn prefix(&mut self) -> Option<((), u8, fn(&mut Self, u8, bool))> {
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
	pub(crate) fn infix(&mut self) -> Option<(u8, u8, fn(&mut Self, u8, bool))> {
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
	pub(crate) fn postfix(&mut self, struct_allowed: bool) -> Option<(u8, (), fn(&mut Self, u8, bool), SyntaxKind)> {
		let a = self.api.peek();

		match a.kind {
			T![as] => Some((24, (), Self::ascript, SyntaxKind::Cast)),
			T!['('] => Some((24, (), Self::call, SyntaxKind::Call)),
			T!['['] => Some((24, (), Self::index, SyntaxKind::Index)),
			T![.] => Some((24, (), Self::access, SyntaxKind::Access)),
			T!['{'] if struct_allowed => Some((24, (), Self::struct_lit, SyntaxKind::StructLit)),
			T![:] => Some((24, (), Self::ascript, SyntaxKind::Ascript)),
			_ => None,
		}
	}

	pub(crate) fn binary(&mut self, bp: u8, struct_allowed: bool) {
		let b = self.api.start_node(SyntaxKind::Op);
		self.api.bump();
		self.api.finish_node(b);

		self.expr_inner(bp, struct_allowed);
	}

	pub(crate) fn binary_2(&mut self, bp: u8, struct_allowed: bool) {
		let b = self.api.start_node(SyntaxKind::Op);
		self.api.bump();
		self.api.bump();
		self.api.finish_node(b);

		self.expr_inner(bp, struct_allowed);
	}

	pub(crate) fn call(&mut self, _: u8, _: bool) {
		let b = self.api.start_node(SyntaxKind::Args);

		self.api.bump();
		self.comma_sep_list(T![')'], |this| {
			this.expr(true);
		});
		self.expect(T![')'], &[T![;], T!['}']]);

		self.api.finish_node(b);
	}

	pub(crate) fn index(&mut self, _: u8, _: bool) {
		self.api.bump();
		self.expr(true);
		self.expect(T![']'], &[T![;], T!['}']]);
	}

	pub(crate) fn access(&mut self, _: u8, _: bool) {
		self.api.bump();
		select! {
			self {
				T![ident] => self.api.bump(),
				T![<] => self.generics(),
			}
		}
	}

	pub(crate) fn ascript(&mut self, _: u8, _: bool) {
		self.api.bump();
		self.ty();
	}

	pub(crate) fn struct_lit(&mut self, _: u8, _: bool) {
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

	pub(crate) fn tuple_or_paren_expr(&mut self) {
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

	pub(crate) fn match_arms(&mut self) {
		let b = self.api.start_node(SyntaxKind::MatchArms);
		self.expect(T!['{'], &[T![;], T!['}']]);
		self.comma_sep_list(T!['}'], |this| {
			let b = this.api.start_node(SyntaxKind::MatchArm);

			let toks = [T![=], T![>], T![,], T!['}'], T![;]];
			this.pat(&toks);

			if matches!(this.api.peek().kind, T![if]) {
				let b = this.api.start_node(SyntaxKind::MatchGuard);
				this.api.bump();
				this.expr(true);

				this.api.finish_node(b);
			}

			this.expect(T![=], &toks[1..]);
			this.expect(T![>], &toks[2..]);
			this.expr(true);

			this.api.finish_node(b);
		});
		self.expect(T!['}'], &[T![;], T!['}']]);
		self.api.finish_node(b);
	}

	pub(crate) fn token_tree(&mut self, delim: Delim) {
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
