use diagnostics::{FileDiagnostic, Span};
use lex::{token::TokenKind, T};
use syntax::{
	builder::{TreeBuilder, TreeBuilderContext},
	SyntaxKind,
};

use crate::{api::Api, helpers::select};

pub struct Parser<'c, 's> {
	pub api: Api<'c, 's>,
	pub diags: Vec<FileDiagnostic>,
}

impl<'c, 's> Parser<'c, 's> {
	pub fn new(source: &'s str, ctx: &'c mut TreeBuilderContext) -> Self {
		Self {
			api: Api::new(source, ctx),
			diags: Vec::new(),
		}
	}

	pub fn parse(mut self) -> (TreeBuilder<'c>, Vec<FileDiagnostic>) {
		self.parse_inner();
		(self.api.finish(), self.diags)
	}

	fn parse_inner(&mut self) {
		while !self.is_empty() {
			self.item();
		}
	}
}

impl Parser<'_, '_> {
	fn item(&mut self) {
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
				T![import] => self.import(),
				T![static] => self.static_(),
			}
		}

		self.api.finish_node(b);
	}

	fn abi(&mut self) {
		let b = self.api.start_node(SyntaxKind::Abi);
		self.api.bump();
		if matches!(self.api.peek().kind, T![string]) {
			self.api.bump();
		}
		self.api.finish_node(b);
	}

	fn struct_(&mut self) {
		let b = self.api.start_node(SyntaxKind::Struct);

		let toks = [T![ident], T!['{'], T!['}'], T![;]];

		self.expect(T![struct], &toks);
		self.name(&toks[1..]);
		self.expect(T!['{'], &toks[2..]);
		self.comma_sep_list(T!['}'], |this| this.param());
		self.expect(T!['}'], &toks[3..]);

		self.api.finish_node(b);
	}

	fn param(&mut self) {
		let b = self.api.start_node(SyntaxKind::Param);

		let toks = [T![,], T![')'], T!['}'], T![;]];
		self.name(&toks);
		self.expect(T![:], &toks);
		self.ty();

		self.api.finish_node(b);
	}

	fn fn_(&mut self) {
		let toks = [T![ident], T!['('], T![')'], T![->], T!['{'], T!['}']];

		self.expect(T![fn], &toks);
		self.name(&toks[1..]);

		let a = self.api.start_node(SyntaxKind::ParamList);
		self.expect(T!['('], &toks[2..]);
		self.comma_sep_list(T![')'], |this| this.param());
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

	fn enum_(&mut self) {
		let b = self.api.start_node(SyntaxKind::Enum);

		const TOKS: [TokenKind; 3] = [T![ident], T!['{'], T!['}']];

		self.expect(T![enum], &TOKS);
		self.expect(T![ident], &TOKS[1..]);

		let v = self.api.start_node(SyntaxKind::VariantList);
		self.expect(T!['{'], &TOKS[2..]);
		self.comma_sep_list(T!['}'], |this| this.name(&TOKS[2..]));
		self.expect(T!['}'], &[]);
		self.api.finish_node(v);

		self.api.finish_node(b);
	}

	fn type_alias(&mut self) {
		let b = self.api.start_node(SyntaxKind::TypeAlias);

		let toks = [T![ident], T![=], T![;]];

		self.expect(T![type], &toks);
		self.name(&toks[1..]);
		self.expect(T![=], &toks[2..]);
		self.ty();
		self.expect(T![;], &[]);

		self.api.finish_node(b);
	}

	fn import(&mut self) {
		let b = self.api.start_node(SyntaxKind::Import);

		let toks = [T![ident], T![;], T![.], T!['{'], T!['}']];

		self.expect(T![import], &toks);
		self.import_tree();
		self.expect(T![;], &[]);

		self.api.finish_node(b);
	}

	fn static_(&mut self) {
		let b = self.api.start_node(SyntaxKind::Static);

		let toks = [T![ident], T![:], T![=], T![;]];

		self.expect(T![static], &toks);
		self.name(&toks[1..]);
		self.expect(T![:], &toks[2..]);
		self.ty();
		self.expect(T![=], &toks[2..]);
		self.expr();
		self.expect(T![;], &[]);

		self.api.finish_node(b);
	}

	fn import_tree(&mut self) {
		let path = |this: &mut Self| {
			let c = this.api.checkpoint();
			this.path();
			if matches!(this.api.peek().kind, T![.]) {
				let b = this.api.start_node_at(c, SyntaxKind::ListImport);
				this.api.bump();
				this.import_tree_list();
				this.api.finish_node(b);
			} else {
				let b = this.api.start_node_at(c, SyntaxKind::RenameImport);
				if matches!(this.api.peek().kind, T![as]) {
					let b = this.api.start_node(SyntaxKind::Rename);
					this.api.bump();
					this.name(&[T![;], T!['}'], T![,]]);
					this.api.finish_node(b);
				}
				this.api.finish_node(b);
			}
		};

		select! {
			self {
				T!['{'] => {
					let b = self.api.start_node(SyntaxKind::ListImport);
					self.import_tree_list();
					self.api.finish_node(b);
				},
				T![.] => path(self),
				T![ident] => path(self),
			}
		}
	}

	fn import_tree_list(&mut self) {
		let b = self.api.start_node(SyntaxKind::ImportTreeList);
		self.expect(T!['{'], &[T!['}'], T![,], T![.], T![ident]]);
		self.comma_sep_list(T!['}'], |this| this.import_tree());
		self.expect(T!['}'], &[T![;], T![,], T![ident]]);
		self.api.finish_node(b);
	}

	fn attributes(&mut self) {
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

	fn ty(&mut self) {
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
				T![_] => {
					let b = self.api.start_node(SyntaxKind::InferType);
					self.api.bump();
					self.api.finish_node(b);
				},
				T![ident] => {
					let b = self.api.start_node(SyntaxKind::PathType);
					self.path();
					self.api.finish_node(b);
				},
				T![.] => {
					let b = self.api.start_node(SyntaxKind::PathType);
					self.path();
					self.api.finish_node(b);
				},
				T![*] => {
					let b = self.api.start_node(SyntaxKind::PtrType);

					self.api.bump();
					if self.api.peek().kind == T![mut] {
						self.api.bump();
					}

					self.ty();

					self.api.finish_node(b);
				},
			}
		}
	}

	fn fn_type(&mut self) {
		let toks = [T![')'], T![->], T!['{'], T!['}']];

		self.api.bump();

		let b = self.api.start_node(SyntaxKind::TyParamList);
		self.expect(T!['('], &toks);
		self.comma_sep_list(T![')'], |this| this.ty());
		self.expect(T![')'], &toks[1..]);
		self.api.finish_node(b);

		if matches!(self.api.peek().kind, T![->]) {
			let b = self.api.start_node(SyntaxKind::RetTy);
			self.api.bump();
			self.ty();
			self.api.finish_node(b);
		}
	}

	fn block(&mut self) {
		let b = self.api.start_node(SyntaxKind::Block);

		self.expect(T!['{'], &[T![pub], T![struct], T![enum], T![fn], T!['}']]);

		loop {
			let curr = self.api.peek();
			match curr.kind {
				T![eof] => {
					self.diags
						.push(curr.span.error("unexpected <eof>").label(curr.span.mark()));
					break;
				},
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

	fn expr(&mut self) { self.expr_inner(0); }

	fn expr_inner(&mut self, min_bp: u8) {
		let check = self.api.checkpoint();

		if let Some(((), bp, kind)) = self.prefix() {
			let b = self.api.start_node_at(check, kind);
			self.expr_inner(bp);
			self.api.finish_node(b);
		} else {
			self.atom();
		};

		loop {
			if let Some((bp, (), f, kind)) = self.postfix() {
				if bp < min_bp {
					break;
				}

				let b = self.api.start_node_at(check, kind);
				f(self);
				self.api.finish_node(b);

				continue;
			}

			if let Some((l_bp, _r_bp)) = self.infix() {
				if l_bp < min_bp {
					break;
				}

				let b = self.api.start_node_at(check, SyntaxKind::InfixExpr);
				self.api.bump();
				self.api.finish_node(b);

				continue;
			}

			break;
		}
	}

	fn atom(&mut self) {
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
			T![ident] => {
				let b = self.api.start_node(SyntaxKind::PathExpr);
				self.path();
				self.api.finish_node(b);
			},
			T![.] => {
				let b = self.api.start_node(SyntaxKind::PathExpr);
				self.path();
				self.api.finish_node(b);
			},
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
					self.api.bump();
					self.expr(); // TODO: this should be a block or an if expr
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
				let diag = tok.span.error("expected expr");

				self.diags.push(if self.api.is_span_eof(tok.span) {
					diag
				} else {
					diag.label(tok.span.label(format!("found `{}`", SyntaxKind::from(tok.kind))))
				});

				self.try_recover(T![eof], &[]);
			},
		}
	}

	// Should eat the operator.
	fn prefix(&mut self) -> Option<((), u8, SyntaxKind)> {
		let a = self.api.peek();

		match a.kind {
			T![-] | T![!] | T![*] => {
				self.api.bump();
				Some(((), 23, SyntaxKind::PrefixExpr))
			},
			T![&] => {
				self.api.bump();
				if matches!(self.api.peek().kind, T![mut]) {
					self.api.bump();
				}
				Some(((), 23, SyntaxKind::RefExpr))
			},
			_ => None,
		}
	}

	// Should eat the operator.
	fn infix(&mut self) -> Option<(u8, u8)> {
		let ret = match self.api.peek().kind {
			T![=] | T![+=] | T![-=] | T![*=] | T![/=] | T![%=] | T![<<=] | T![>>=] | T![&=] | T![|=] | T![^=] => (2, 1),
			T![|] => (3, 4),
			T![&] => (5, 6),
			T![||] => (7, 8),
			T![^] => (9, 10),
			T![&&] => (11, 12),
			T![==] | T![!=] => (13, 14),
			T![<] | T![<=] | T![>] | T![>=] => (15, 16),
			T![<<] | T![>>] => (17, 18),
			T![+] | T![-] => (19, 20),
			T![*] | T![/] | T![%] => (21, 22),
			_ => return None,
		};
		self.api.bump();
		Some(ret)
	}

	// Should not eat the operator: `f` should
	fn postfix(&mut self) -> Option<(u8, (), fn(&mut Self), SyntaxKind)> {
		let a = self.api.peek();

		match a.kind {
			T![as] => Some((24, (), Self::cast, SyntaxKind::CastExpr)),
			T!['('] => Some((24, (), Self::call, SyntaxKind::CallExpr)),
			T!['['] => Some((24, (), Self::index, SyntaxKind::IndexExpr)),
			T![.] => Some((24, (), Self::field, SyntaxKind::FieldExpr)),
			_ => None,
		}
	}

	fn cast(&mut self) {
		self.api.bump();
		self.ty();
	}

	fn call(&mut self) {
		let b = self.api.start_node(SyntaxKind::ArgList);

		self.api.bump();
		self.comma_sep_list(T![')'], |this| this.expr());
		self.expect(T![')'], &[T![;], T!['}']]);

		self.api.finish_node(b);
	}

	fn index(&mut self) {
		self.api.bump();
		self.expr();
		self.expect(T![']'], &[T![;], T!['}']]);
	}

	fn field(&mut self) {
		self.api.bump();
		self.name(&[T![;], T!['}']]);
	}

	fn match_arms(&mut self) {
		self.comma_sep_list(T!['}'], |this| {
			let b = this.api.start_node(SyntaxKind::MatchArm);

			this.expr();
			this.expect(T![=>], &[T![=], T![,], T!['}'], T![;]]);
			this.expr();

			this.api.finish_node(b);
		});
	}

	fn path(&mut self) {
		let c = self.api.checkpoint();

		if matches!(self.api.peek().kind, T![.]) {
			let b = self.api.start_node_at(c, SyntaxKind::Path);
			self.api.bump();
			self.api.finish_node(b);
		}

		loop {
			if matches!(self.api.peek().kind, T![ident]) {
				let b = self.api.start_node_at(c, SyntaxKind::Path);
				self.api.bump();
				self.api.finish_node(b);

				if matches!(self.api.peek().kind, T![.]) && matches!(self.api.peek_n(1).kind, T![ident]) {
					let b = self.api.start_node_at(c, SyntaxKind::Path);
					self.api.bump();
					self.api.finish_node(b);
					continue;
				}
			}
			break;
		}
	}

	fn name(&mut self, next: &[TokenKind]) {
		let b = self.api.start_node(SyntaxKind::Name);
		self.expect(T![ident], next);
		self.api.finish_node(b);
	}

	fn token_tree(&mut self) {
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
