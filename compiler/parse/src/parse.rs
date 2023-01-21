use diagnostics::DiagSink;
use lex::{token::TokenKind, T};
use syntax::{builder::TreeBuilder, SyntaxKind};

use crate::{api::Api, helpers::select};

pub struct Parser<'c, 's> {
	pub api: Api<'c, 's>,
	pub diags: DiagSink<()>,
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
					self.abi();
					self.fn_();
					self.api.finish_node(b);
				},
				T![type] => self.type_alias(),
			}
		}
		self.api.finish_node(b);
	}

	pub(crate) fn abi(&mut self) {
		let b = self.api.start_node(SyntaxKind::Abi);
		self.api.bump();
		if matches!(self.api.peek().kind, T![string]) {
			self.api.bump();
		}
		self.api.finish_node(b);
	}

	pub(crate) fn struct_(&mut self) {
		let b = self.api.start_node(SyntaxKind::Struct);

		let toks = [T![ident], T!['{'], T!['}'], T![;]];

		self.expect(T![struct], &toks);
		self.name(&toks[1..]);
		self.expect(T!['{'], &toks[2..]);
		self.comma_sep_list(T!['}'], |this| {
			self.param();
		});

		self.api.finish_node(b);
	}

	pub(crate) fn param(&mut self) {
		let b = self.api.start_node(SyntaxKind::Param);

		let toks = [T![,], T![')'], T!['}'], T![;]];
		self.name(&toks);
		self.expect(T![:], &toks);
		self.ty();

		self.api.finish_node(b);
	}

	pub(crate) fn fn_(&mut self) {
		let toks = [T![ident], T!['('], T![')'], T![->], T!['{'], T!['}']];

		self.expect(T![fn], &toks);
		self.name(&toks[1..]);

		let a = self.api.start_node(SyntaxKind::ParamList);
		self.expect(T!['('], &toks[2..]);
		self.comma_sep_list(T![')'], |this| self.param());
		self.expect(T![')'], &toks[3..]);
		self.api.finish_node(a);

		if matches!(self.api.peek().kind, T![->]) {
			let b = self.api.start_node(SyntaxKind::RetTy);
			self.api.bump();
			self.ty();
			self.api.finish_node(b);
		}

		if matches!(self.api.peek().kind, T!['{']) {
			self.block();
		} else {
			self.expect(T![;], &[]);
		}
	}

	pub(crate) fn enum_(&mut self) {
		let b = self.api.start_node(SyntaxKind::Enum);

		let toks = [T![ident], T!['{'], T!['}']];

		self.expect(T![enum], &toks);
		self.expect(T![ident], &toks[1..]);
		self.expect(T!['{'], &toks[2..]);

		let v = self.api.start_node(SyntaxKind::VariantList);
		self.comma_sep_list(T!['}'], |this| {
			self.name(&toks[2..]);
		});
		self.api.finish_node(v);

		self.expect(T!['}'], &[]);

		self.api.finish_node(b);
	}

	pub(crate) fn type_alias(&mut self) {
		let b = self.api.start_node(SyntaxKind::TypeAlias);

		let toks = [T![ident]];

		self.expect(T![type], &toks);
		self.expect(T![ident], &[]);

		if matches!(self.api.peek().kind, T![=]) {
			self.api.bump();
			self.ty();
		}
		self.expect(T![;], &[]);

		self.api.finish_node(b);
	}

	pub(crate) fn attributes(&mut self) {
		while matches!(self.api.peek().kind, T![@]) {
			let b = self.api.start_node(SyntaxKind::Attribute);

			self.api.bump();
			self.name(&[T!['(']]);
			if matches!(self.api.peek().kind, T!['(']) {
				self.token_tree();
			}

			self.api.finish_node(b);
		}
	}

	pub(crate) fn ty(&mut self) {
		select! {
			self {
				T!['['] => {
					let b = self.api.start_node(SyntaxKind::ArrayType);

					self.api.bump();
					self.ty();
					self.expect(T![;], &[T![']']]);
					self.expr();
					self.expect(T![']'], &[]);

					self.api.finish_node(b);
				},
				T![extern] => {
					let b = self.api.start_node(SyntaxKind::FnType);
					self.abi();
					self.fn_type();
					self.api.finish_node(b);
				},
				T![fn] => {
					let b = self.api.start_node(SyntaxKind::FnType);
					self.fn_type();
					self.api.finish_node(b);
				},
				T![_] => self.api.bump(),
				T![ident] => self.path(),
				T![.] => self.path(),
				T![*] => {
					let b = self.api.start_node(SyntaxKind::PtrType);

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
			}
		}
	}

	pub(crate) fn fn_type(&mut self) {
		let toks = [T![')'], T![->], T!['{'], T!['}']];

		self.api.bump();

		let b = self.api.start_node(SyntaxKind::TyParamList);
		self.expect(T!['('], &toks);
		self.comma_sep_list(T![')'], |this| self.ty());
		self.expect(T![')'], &toks[1..]);
		self.api.finish_node(b);

		if matches!(self.api.peek().kind, T![->]) {
			let b = self.api.start_node(SyntaxKind::RetTy);
			self.api.bump();
			self.ty();
			self.api.finish_node(b);
		}
	}

	pub(crate) fn block(&mut self) {
		let b = self.api.start_node(SyntaxKind::Block);

		self.expect(T!['{'], &[T![pub], T![struct], T![enum], T![fn], T!['}']]);

		loop {
			match self.api.peek().kind {
				T![;] => {
					self.api.bump();
				},
				T!['}'] => break,
				T![pub] | T![struct] | T![enum] | T![fn] => self.item(),
				_ => {
					let c = self.api.checkpoint();
					self.expr();
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

	pub(crate) fn expr(&mut self) { self.expr_inner(0); }

	pub(crate) fn expr_inner(&mut self, min_bp: u8) {
		let infix = self.api.checkpoint();
		let prefix = self.api.checkpoint();
		let postfix = self.api.checkpoint();

		if let Some((_, bp, f)) = self.prefix() {
			let b = self.api.start_node_at(prefix, SyntaxKind::PrefixExpr);
			f(self, bp);
			self.api.finish_node(b);
		} else {
			self.atom();
		};

		loop {
			if let Some((bp, (), f, kind)) = self.postfix() {
				if bp < min_bp {
					break;
				}

				let b = self.api.start_node_at(postfix, kind);
				f(self, bp);
				self.api.finish_node(b);

				continue;
			}

			if let Some((l_bp, r_bp, f)) = self.infix() {
				if l_bp < min_bp {
					break;
				}

				let b = self.api.start_node_at(infix, SyntaxKind::InfixExpr);
				f(self, r_bp);
				self.api.finish_node(b);

				continue;
			}

			break;
		}
	}

	pub(crate) fn atom(&mut self) {
		let tok = self.api.peek();
		match tok.kind {
			T!['('] => {
				let b = self.api.start_node(SyntaxKind::ParenExpr);
				self.api.bump();
				self.expr();
				self.expect(T![')'], &[]);
				self.api.finish_node(b);
			},
			T!['{'] => self.block(),
			T![bool] | T![char] | T![float] | T![int] | T![string] => {
				self.api.bump();
			},
			T![ident] => self.api.bump(),
			T![break] => {
				let b = self.api.start_node(SyntaxKind::BreakExpr);

				self.api.bump();
				if !matches!(self.api.peek().kind, T![,] | T![;] | T!['}'] | T![')'] | T![']']) {
					self.expr();
				}

				self.api.finish_node(b);
			},
			T![continue] => self.api.bump(),
			T![return] => {
				let b = self.api.start_node(SyntaxKind::ReturnExpr);

				self.api.bump();
				if !matches!(self.api.peek().kind, T![,] | T![;] | T!['}'] | T![')'] | T![']']) {
					self.expr();
				}

				self.api.finish_node(b);
			},
			T![loop] => {
				let b = self.api.start_node(SyntaxKind::LoopExpr);
				self.api.bump();
				self.block();

				if matches!(self.api.peek().kind, T![while]) {
					self.api.bump();
					self.expr();
				}

				self.api.finish_node(b);
			},
			T![while] => {
				let b = self.api.start_node(SyntaxKind::WhileExpr);

				self.api.bump();
				self.expr();
				self.block();

				self.api.finish_node(b);
			},
			T![for] => {
				let b = self.api.start_node(SyntaxKind::ForExpr);

				self.api.bump();
				self.name(&[T![in]]);
				self.expect(T![in], &[]);
				self.expr();
				self.block();

				self.api.finish_node(b);
			},
			T![if] => {
				let b = self.api.start_node(SyntaxKind::IfExpr);

				self.api.bump();
				self.expr();
				self.block();
				if matches!(self.api.peek().kind, T![else]) {
					let b = self.api.start_node(SyntaxKind::ElseExpr);
					self.api.bump();
					self.expr();
					self.api.finish_node(b);
				}

				self.api.finish_node(b);
			},
			T![let] => {
				self.api.bump();
				self.name(&[T![:], T![=], T![;]]);
				if matches!(self.api.peek().kind, T![:]) {
					self.api.bump();
					self.ty();
				}
				if matches!(self.api.peek().kind, T![=]) {
					self.api.bump();
					self.expr();
				}
			},
			T![match] => {
				let b = self.api.start_node(SyntaxKind::MatchExpr);
				self.api.bump();
				self.expr();

				self.expect(T!['{'], &[T![;], T!['}']]);
				self.match_arms();
				self.expect(T!['}'], &[T![;], T!['}']]);

				self.api.finish_node(b);
			},
			_ => {
				if !self.silent {
					let diag = tok.span.error("expected expr");

					self.diags.push(if self.api.is_span_eof(tok.span) {
						diag
					} else {
						diag.label(tok.span.label(format!("found `{}`", SyntaxKind::from(tok.kind))))
					});
				}

				self.try_recover(T![eof], &[]);
			},
		}
	}

	// Should eat the operator.
	pub(crate) fn prefix(&mut self) -> Option<((), u8, fn(&mut Self, u8))> {
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
	pub(crate) fn infix(&mut self) -> Option<(u8, u8, fn(&mut Self, u8))> {
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
	pub(crate) fn postfix(&mut self) -> Option<(u8, (), fn(&mut Self, u8), SyntaxKind)> {
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

	pub(crate) fn binary(&mut self, bp: u8) {
		let b = self.api.start_node(SyntaxKind::Op);
		self.api.bump();
		self.api.finish_node(b);

		self.expr_inner(bp, struct_allowed);
	}

	pub(crate) fn binary_2(&mut self, bp: u8) {
		let b = self.api.start_node(SyntaxKind::Op);
		self.api.bump();
		self.api.bump();
		self.api.finish_node(b);

		self.expr_inner(bp, struct_allowed);
	}

	pub(crate) fn call(&mut self, _: u8) {
		let b = self.api.start_node(SyntaxKind::Args);

		self.api.bump();
		self.comma_sep_list(T![')'], |this| {
			this.expr(true);
		});
		self.expect(T![')'], &[T![;], T!['}']]);

		self.api.finish_node(b);
	}

	pub(crate) fn index(&mut self, _: u8) {
		self.api.bump();
		self.expr(true);
		self.expect(T![']'], &[T![;], T!['}']]);
	}

	pub(crate) fn access(&mut self, _: u8) {
		self.api.bump();
		select! {
			self {
				T![ident] => self.api.bump(),
				T![<] => self.generics(),
			}
		}
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
		self.comma_sep_list(T!['}'], |this| {
			let b = this.api.start_node(SyntaxKind::MatchArm);

			let toks = [T![=>], T![=], T![>], T![,], T!['}'], T![;]];
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
	}

	pub(crate) fn path(&mut self) {
		let b = self.api.start_node(SyntaxKind::Path);

		if matches!(self.api.peek().kind, T![.]) {
			self.api.bump();
		}

		loop {
			self.name(&[T![.], T![ident]]);
			if matches!(self.api.peek().kind, T![.]) {
				self.api.bump();
			} else {
				break;
			}
		}

		self.api.finish_node(b);
	}

	pub(crate) fn name(&mut self, next: &[TokenKind]) {
		let b = self.api.start_node(SyntaxKind::Name);
		self.expect(T![ident], next);
		self.api.finish_node(b);
	}

	pub(crate) fn token_tree(&mut self) {
		let b = self.api.start_node(SyntaxKind::TokenTree);
		self.expect(T!['('], &[T![')']]);

		loop {
			let curr = self.api.peek();
			self.api.bump();
			match curr.kind {
				T![')'] => break,
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
