use diagnostics::Span;
use lex::T;
use syntax::SyntaxKind;

use crate::parse::{
	p,
	recovery::{ParseRule, Recovery, Rule},
	rules::expr::expr_inner,
	select,
	Parser,
};

#[derive(Copy, Clone)]
pub struct Item;

impl Rule for Item {
	fn rule(&self) -> ParseRule { ParseRule::Item }

	fn parse(self, p: &mut Parser) -> Recovery {
		p.api.start_node(SyntaxKind::Item);

		p!(attributes(p));

		if matches!(p.api.peek().kind, T![pub]) {
			p.api.start_node(SyntaxKind::Visibility);
			p.api.bump();
			p.api.finish_node();
		}

		select! {
			p,
			T![struct] => p!(p.run(Struct)),
			T![enum] => p!(p.run(Enum)),
			T![fn] => {
				p.api.start_node(SyntaxKind::Fn);
				p!(p.run(Fn));
				p.api.finish_node();
			},
			T![extern] => {
				p.api.start_node(SyntaxKind::Fn);
				abi(p);
				p!(p.run(Fn));
				p.api.finish_node();
			},
			T![type] => p!(p.run(TypeAlias)),
			T![import] => p!(p.run(Import)),
			T![static] => p!(p.run(Static)),
		};

		p.api.finish_node();
		Recovery::ok()
	}
}

#[derive(Copy, Clone)]
pub struct Struct;

impl Rule for Struct {
	fn rule(&self) -> ParseRule { ParseRule::Struct }

	fn parse(self, p: &mut Parser) -> Recovery {
		p.api.start_node(SyntaxKind::Struct);

		p!(p.expect(T![struct]));
		p!(name(p));
		p!(p.expect(T!['{']));
		p!(p.comma_sep_list(T!['}'], Param(ParseRule::Field)));
		p!(p.expect(T!['}']));

		p.api.finish_node();
		Recovery::ok()
	}
}

#[derive(Copy, Clone)]
pub struct Param(ParseRule);

impl Rule for Param {
	fn rule(&self) -> ParseRule { self.0 }

	fn parse(self, p: &mut Parser) -> Recovery {
		p.api.start_node(SyntaxKind::Param);

		p!(name(p));
		p!(p.expect(T![:]));
		p!(p.run(Type));

		p.api.finish_node();
		Recovery::ok()
	}
}

#[derive(Copy, Clone)]
pub struct Fn;

impl Rule for Fn {
	fn rule(&self) -> ParseRule { ParseRule::Fn }

	fn parse(self, p: &mut Parser) -> Recovery {
		p!(p.expect(T![fn]));
		p!(name(p));

		p.api.start_node(SyntaxKind::ParamList);
		p!(p.expect(T!['(']));
		p!(p.comma_sep_list(T![')'], Param(ParseRule::Param)));
		p!(p.expect(T![')']));
		p.api.finish_node();

		if matches!(p.api.peek().kind, T![->]) {
			p.api.start_node(SyntaxKind::RetTy);
			p.api.bump();
			p!(p.run(Type));
			p.api.finish_node();
		}

		if matches!(p.api.peek().kind, T!['{']) {
			p!(p.run(Block));
		} else {
			p!(p.expect(T![;]));
		}
		Recovery::ok()
	}
}

#[derive(Copy, Clone)]
pub struct Enum;

impl Rule for Enum {
	fn rule(&self) -> ParseRule { ParseRule::Enum }

	fn parse(self, p: &mut Parser) -> Recovery {
		p.api.start_node(SyntaxKind::Enum);

		p!(p.expect(T![enum]));
		p!(p.expect(T![ident]));

		p.api.start_node(SyntaxKind::VariantList);
		p!(p.expect(T!['{']));
		p!(p.comma_sep_list(T!['}'], EnumVariant));
		p!(p.expect(T!['}']));
		p.api.finish_node();

		p.api.finish_node();
		Recovery::ok()
	}
}

#[derive(Copy, Clone)]
pub struct EnumVariant;

impl Rule for EnumVariant {
	fn rule(&self) -> ParseRule { ParseRule::EnumVariant }

	fn parse(self, p: &mut Parser) -> Recovery { name(p) }
}

#[derive(Copy, Clone)]
pub struct TypeAlias;

impl Rule for TypeAlias {
	fn rule(&self) -> ParseRule { ParseRule::TypeAlias }

	fn parse(self, p: &mut Parser) -> Recovery {
		p.api.start_node(SyntaxKind::TypeAlias);

		p!(p.expect(T![type]));
		p!(name(p));
		p!(p.expect(T![=]));
		p!(p.run(Type));
		p!(p.expect(T![;]));

		p.api.finish_node();
		Recovery::ok()
	}
}

#[derive(Copy, Clone)]
pub struct Import;

impl Rule for Import {
	fn rule(&self) -> ParseRule { ParseRule::Import }

	fn parse(self, p: &mut Parser) -> Recovery {
		p.api.start_node(SyntaxKind::Import);

		p!(p.expect(T![import]));
		p!(p.run(ImportTree));
		p!(p.expect(T![;]));

		p.api.finish_node();
		Recovery::ok()
	}
}

#[derive(Copy, Clone)]
pub struct Static;

impl Rule for Static {
	fn rule(&self) -> ParseRule { ParseRule::Static }

	fn parse(self, p: &mut Parser) -> Recovery {
		p.api.start_node(SyntaxKind::Static);

		p!(p.expect(T![static]));
		p!(name(p));
		p!(p.expect(T![:]));
		p!(p.run(Type));
		p!(p.expect(T![=]));
		p!(p.run(Expr));
		p!(p.expect(T![;]));

		p.api.finish_node();
		Recovery::ok()
	}
}

#[derive(Copy, Clone)]
pub struct ImportTree;

impl Rule for ImportTree {
	fn rule(&self) -> ParseRule { ParseRule::ImportTree }

	fn parse(self, p: &mut Parser) -> Recovery {
		let path = |p: &mut Parser| -> Recovery {
			let c = p.api.checkpoint();
			p!(path(p));
			if matches!(p.api.peek().kind, T![.]) {
				p.api.start_node_at(c, SyntaxKind::ListImport);
				p.api.bump();
				p!(import_tree_list(p));
				p.api.finish_node();
			} else {
				p.api.start_node_at(c, SyntaxKind::RenameImport);
				if matches!(p.api.peek().kind, T![as]) {
					p.api.start_node(SyntaxKind::Rename);
					p.api.bump();
					p!(name(p));
					p.api.finish_node();
				}
				p.api.finish_node();
			}

			Recovery::ok()
		};

		select! {
			p,
			T!['{'] => {
				p.api.start_node(SyntaxKind::ListImport);
				p!(import_tree_list(p));
				p.api.finish_node();
			},
			T![.] => p!(path(p)),
			T![ident] => p!(path(p)),
		}
		Recovery::ok()
	}
}

#[derive(Copy, Clone)]
pub struct Attribute;

impl Rule for Attribute {
	fn rule(&self) -> ParseRule { ParseRule::Attribute }

	fn parse(self, p: &mut Parser) -> Recovery {
		p.api.start_node(SyntaxKind::Attribute);

		p.api.bump();
		p!(name(p));
		if matches!(p.api.peek().kind, T!['(']) {
			p!(token_tree(p));
		}

		p.api.finish_node();
		Recovery::ok()
	}
}

#[derive(Copy, Clone)]
pub struct Type;

impl Rule for Type {
	fn rule(&self) -> ParseRule { ParseRule::Type }

	fn parse(self, p: &mut Parser) -> Recovery {
		select! {
			p,
			T!['['] => {
				p.api.start_node(SyntaxKind::ArrayType);

				p.api.bump();
				p!(p.run(Type));
				p!(p.expect(T![;]));
				p!(p.run(Expr));
				p!(p.expect(T![']']));

				p.api.finish_node();
			},
			T![extern] => {
				p.api.start_node(SyntaxKind::FnType);
				abi(p);
				p!(fn_type(p));
				p.api.finish_node();
			},
			T![fn] => {
				p.api.start_node(SyntaxKind::FnType);
				p!(fn_type(p));
				p.api.finish_node();
			},
			T![_] => {
				p.api.start_node(SyntaxKind::InferType);
				p.api.bump();
				p.api.finish_node();
			},
			T![ident] => {
				p.api.start_node(SyntaxKind::PathType);
				p!(path(p));
				p.api.finish_node();
			},
			T![.] => {
				p.api.start_node(SyntaxKind::PathType);
				p!(path(p));
				p.api.finish_node();
			},
			T![*] => {
				p.api.start_node(SyntaxKind::PtrType);

				p.api.bump();
				if p.api.peek().kind == T![mut] {
					p.api.bump();
				}
				p!(p.run(Type));

				p.api.finish_node();
			},
		}
		Recovery::ok()
	}
}

#[derive(Copy, Clone)]
pub struct Block;

impl Rule for Block {
	fn rule(&self) -> ParseRule { ParseRule::Block }

	fn parse(self, p: &mut Parser) -> Recovery {
		p.api.start_node(SyntaxKind::Block);
		p!(p.expect(T!['{']));
		p.begin_repeat();

		loop {
			let curr = p.api.peek();
			match curr.kind {
				T![eof] => {
					p.end_repeat();
					p!(p.expect(T!['}']));
					p.api.finish_node();
					return Recovery::ok();
				},
				T![;] => {
					p.api.bump();
				},
				T!['}'] => break,
				T![pub] | T![struct] | T![enum] | T![fn] => p!(p.run(Item)),
				_ => {
					let c = p.api.checkpoint();
					p!(p.run(Expr));
					if matches!(p.api.peek().kind, T![;]) {
						p.api.start_node_at(c, SyntaxKind::SemiExpr);
						p.api.bump();
						p.api.finish_node();
					}
				},
			}
		}

		p.end_repeat();
		p!(p.expect(T!['}']));
		p.api.finish_node();
		Recovery::ok()
	}
}

#[derive(Copy, Clone)]
pub struct Expr;

impl Rule for Expr {
	fn rule(&self) -> ParseRule { ParseRule::Expr }

	fn parse(self, p: &mut Parser) -> Recovery { expr_inner(p, 0) }
}

#[derive(Copy, Clone)]
pub struct MatchArm;

impl Rule for MatchArm {
	fn rule(&self) -> ParseRule { ParseRule::MatchArm }

	fn parse(self, p: &mut Parser) -> Recovery {
		p.api.start_node(SyntaxKind::MatchArm);

		p!(p.run(Expr));
		p!(p.expect(T![=>]));
		p!(p.run(Expr));

		p.api.finish_node();
		Recovery::ok()
	}
}

fn abi(p: &mut Parser) {
	p.api.start_node(SyntaxKind::Abi);
	p.api.bump();
	if matches!(p.api.peek().kind, T![string]) {
		p.api.bump();
	}
	p.api.finish_node();
}

fn attributes(p: &mut Parser) -> Recovery {
	p.begin_repeat();
	while matches!(p.api.peek().kind, T![@]) {
		p!(p.run(Attribute));
	}
	p.end_repeat();
	Recovery::ok()
}

fn fn_type(p: &mut Parser) -> Recovery {
	p.api.bump();

	p.api.start_node(SyntaxKind::TyParamList);
	p!(p.expect(T!['(']));
	p!(p.comma_sep_list(T![')'], Type));
	p!(p.expect(T![')']));
	p.api.finish_node();

	if matches!(p.api.peek().kind, T![->]) {
		p.api.start_node(SyntaxKind::RetTy);
		p.api.bump();
		p!(p.run(Type));
		p.api.finish_node();
	}

	Recovery::ok()
}

fn import_tree_list(p: &mut Parser) -> Recovery {
	p.api.start_node(SyntaxKind::ImportTreeList);
	p!(p.expect(T!['{']));
	p!(p.comma_sep_list(T!['}'], ImportTree));
	p!(p.expect(T!['}']));
	p.api.finish_node();

	Recovery::ok()
}

fn name(p: &mut Parser) -> Recovery {
	p.api.start_node(SyntaxKind::Name);
	p!(p.expect(T![ident]));
	p.api.finish_node();
	Recovery::ok()
}

fn path(p: &mut Parser) -> Recovery {
	let c = p.api.checkpoint();

	if matches!(p.api.peek().kind, T![.]) {
		p.api.start_node_at(c, SyntaxKind::Path);
		p.api.bump();
		p.api.finish_node();
	}

	loop {
		if matches!(p.api.peek().kind, T![ident]) {
			p.api.start_node_at(c, SyntaxKind::Path);
			p!(name(p));
			p.api.finish_node();

			if matches!(p.api.peek().kind, T![.]) && matches!(p.api.peek_n(1).kind, T![ident]) {
				p.api.start_node_at(c, SyntaxKind::Path);
				p.api.bump();
				p.api.finish_node();
				continue;
			}
		}
		break Recovery::ok();
	}
}

fn token_tree(p: &mut Parser) -> Recovery {
	p.api.start_node(SyntaxKind::TokenTree);
	p!(p.expect(T!['(']));

	loop {
		let curr = p.api.peek();
		p.api.bump();
		match curr.kind {
			T![')'] => break,
			T![eof] => {
				p.diags
					.push(curr.span.error("unexpected <eof>").label(curr.span.mark()));
				break;
			},
			_ => {},
		}
	}

	p.api.finish_node();
	Recovery::ok()
}

mod expr {
	use diagnostics::Span;
	use lex::T;
	use syntax::SyntaxKind;

	use crate::parse::{
		p,
		recovery::Recovery,
		rules::{name, path, Block, Expr, MatchArm, Type},
		select,
		Parser,
	};

	pub fn expr_inner(p: &mut Parser, min_bp: u8) -> Recovery {
		let check = p.api.checkpoint();

		if let Some(((), bp, kind)) = prefix(p) {
			p.api.start_node_at(check, kind);
			p!(expr_inner(p, bp));
			p.api.finish_node();
		} else {
			p!(atom(p));
		};

		loop {
			if let Some((bp, (), f, kind)) = postfix(p) {
				if bp < min_bp {
					break;
				}

				p.api.start_node_at(check, kind);
				p!(f(p));
				p.api.finish_node();

				continue;
			}

			if let Some((l_bp, _r_bp)) = infix(p) {
				if l_bp < min_bp {
					break;
				}

				p.api.start_node_at(check, SyntaxKind::InfixExpr);
				p!(expr_inner(p, l_bp));
				p.api.finish_node();

				continue;
			}

			break;
		}

		Recovery::ok()
	}

	fn atom(p: &mut Parser) -> Recovery {
		select! {
			p,
			T!['('] => {
				p.api.start_node(SyntaxKind::ParenExpr);
				p.api.bump();
				p!(p.run(Expr));
				p!(p.expect(T![')']));
				p.api.finish_node();
			},
			T!['{'] => p!(p.run(Block)),
			T![bool] => {
				p.api.bump();
			},
			T![char]=> {
				p.api.bump();
			},
			T![float]=> {
				p.api.bump();
			},
			T![int]=> {
				p.api.bump();
			},
			T![string] => {
				p.api.bump();
			},
			T![ident] => {
				p.api.start_node(SyntaxKind::PathExpr);
				p!(path(p));
				p.api.finish_node();
			},
			T![.] => {
				p.api.start_node(SyntaxKind::PathExpr);
				p!(path(p));
				p.api.finish_node();
			},
			T![break] => {
				p.api.start_node(SyntaxKind::BreakExpr);

				p.api.bump();
				if !matches!(p.api.peek().kind, T![,] | T![;] | T!['}'] | T![')'] | T![']']) {
					p!(p.run(Expr));
				}

				p.api.finish_node();
			},
			T![continue] => p.api.bump(),
			T![return] => {
				p.api.start_node(SyntaxKind::ReturnExpr);

				p.api.bump();
				if !matches!(p.api.peek().kind, T![,] | T![;] | T!['}'] | T![')'] | T![']']) {
					p!(p.run(Expr));
				}

				p.api.finish_node();
			},
			T![loop] => {
				p.api.start_node(SyntaxKind::LoopExpr);
				p.api.bump();
				p!(p.run(Block));

				if matches!(p.api.peek().kind, T![while]) {
					p.api.bump();
					p!(p.run(Expr));
				}

				p.api.finish_node();
			},
			T![while] => {
				p.api.start_node(SyntaxKind::WhileExpr);

				p.api.bump();
				p!(p.run(Expr));
				p!(p.run(Block));

				p.api.finish_node();
			},
			T![for] => {
				p.api.start_node(SyntaxKind::ForExpr);

				p.api.bump();
				p!(name(p));
				p!(p.expect(T![in]));
				p!(p.run(Expr));
				p!(p.run(Block));

				p.api.finish_node();
			},
			T![if] => {
				p.api.start_node(SyntaxKind::IfExpr);

				p.api.bump();
				p!(p.run(Expr));
				p!(p.run(Block));
				if matches!(p.api.peek().kind, T![else]) {
					p.api.bump();
					p!(p.run(Expr)); // TODO: this should be a block or an if expr
				}

				p.api.finish_node();
			},
			T![let] => {
				p.api.bump();
				p!(name(p));
				if matches!(p.api.peek().kind, T![:]) {
					p.api.bump();
					p!(p.run(Type));
				}
				if matches!(p.api.peek().kind, T![=]) {
					p.api.bump();
					p!(p.run(Expr));
				}
			},
			T![match] => {
				p.api.start_node(SyntaxKind::MatchExpr);
				p.api.bump();
				p!(p.run(Expr));

				p!(p.expect(T!['{']));
				p!(p.comma_sep_list(T!['}'], MatchArm));
				p!(p.expect(T!['}']));

				p.api.finish_node();
			},
		}

		Recovery::ok()
	}

	// Should eat the operator.
	fn prefix(p: &mut Parser) -> Option<((), u8, SyntaxKind)> {
		let a = p.api.peek();

		match a.kind {
			T![-] | T![!] | T![*] => {
				p.api.bump();
				Some(((), 23, SyntaxKind::PrefixExpr))
			},
			T![&] => {
				p.api.bump();
				if matches!(p.api.peek().kind, T![mut]) {
					p.api.bump();
				}
				Some(((), 23, SyntaxKind::RefExpr))
			},
			_ => None,
		}
	}

	// Should eat the operator.
	fn infix(p: &mut Parser) -> Option<(u8, u8)> {
		let ret = match p.api.peek().kind {
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
		p.api.bump();
		Some(ret)
	}

	// Should not eat the operator: `f` should
	fn postfix(p: &mut Parser) -> Option<(u8, (), fn(&mut Parser) -> Recovery, SyntaxKind)> {
		let a = p.api.peek();

		match a.kind {
			T![as] => Some((24, (), cast, SyntaxKind::CastExpr)),
			T!['('] => Some((24, (), call, SyntaxKind::CallExpr)),
			T!['['] => Some((24, (), index, SyntaxKind::IndexExpr)),
			T![.] => Some((24, (), field, SyntaxKind::FieldExpr)),
			_ => None,
		}
	}

	fn cast(p: &mut Parser) -> Recovery {
		p.api.bump();
		p.run(Type)
	}

	fn call(p: &mut Parser) -> Recovery {
		p.api.start_node(SyntaxKind::ArgList);

		p.api.bump();
		p!(p.comma_sep_list(T![')'], Expr));
		p!(p.expect(T![')']));

		p.api.finish_node();

		Recovery::ok()
	}

	fn index(p: &mut Parser) -> Recovery {
		p.api.bump();
		p!(p.run(Expr));
		p!(p.expect(T![']']));
		Recovery::ok()
	}

	fn field(p: &mut Parser) -> Recovery {
		p.api.bump();
		p!(name(p));
		Recovery::ok()
	}
}
