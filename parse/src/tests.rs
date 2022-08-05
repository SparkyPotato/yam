use std::panic::{catch_unwind, resume_unwind, AssertUnwindSafe};

use diag::Diagnostics;
use expect_test::{expect, Expect};
use intern::Id;
use lex::{token::Delim, T};
use pretty_assertions::assert_eq;
use syntax::{
	builder::{ResolvedNode, TreeBuilderContext},
	kind::SyntaxKind,
};

use crate::{Api, Parser};

#[test]
fn peeking() {
	let source = "0 ident";

	let ast = expect![[r#"
    File@0..7
      IntLit@0..1 "0"
      Whitespace@1..2 " "
      Ident@2..7 "ident""#]];

	let diags = expect![[r#""#]];

	harness(
		source,
		|p| {
			assert_eq!(p.api.peek_n(0).kind, T![lit(int)]);
			assert_eq!(p.api.peek_n(1).kind, T![ident]);

			p.api.bump();
			assert_eq!(p.api.peek_n(0).kind, T![ident]);
			assert_eq!(p.api.peek_n(1).kind, T![eof]);

			p.api.bump();
			assert_eq!(p.api.peek_n(0).kind, T![eof]);
			assert_eq!(p.api.peek_n(1).kind, T![eof]);
		},
		ast,
		diags,
	);
}

#[test]
fn expect() {
	let source = "something";

	let ast = expect![[r#"
    File@0..9
      Ident@0..9 "something""#]];

	let diags = expect![[r#""#]];

	harness(
		source,
		|p| {
			p.expect(T![ident]);
		},
		ast,
		diags,
	);

	let source = "hello()) fn";

	let ast = expect![[r#"
    File@0..11
      Err@0..8
        Ident@0..5 "hello"
        LParen@5..6 "("
        RParen@6..7 ")"
        RParen@7..8 ")"
      Whitespace@8..9 " "
      FnKw@9..11 "fn""#]];

	let diags = expect![[r#"
    Error: expected `fn`
       ,-[<unknown>:1:1]
       |
     1 | hello()) fn
       * ^^|^^  
       *   `---- found `<ident>`
    ---'
"#]];

	harness(
		source,
		|p| {
			p.expect(T![fn]);
			p.expect(T![fn]);
		},
		ast,
		diags,
	);

	let source = "";

	let ast = expect![[r#"
    File@0..0
      Err@0..0"#]];

	let diags = expect![[r#"
    Error: expected `fn`
"#]];

	harness(
		source,
		|p| {
			p.expect(T![fn]);
		},
		ast,
		diags,
	);
}

#[test]
fn comma_sep_list() {
	let source = "hello, world, bye)";

	let ast = expect![[r#"
    File@0..18
      Item@0..5
        Ident@0..5 "hello"
      Comma@5..6 ","
      Whitespace@6..7 " "
      Item@7..12
        Ident@7..12 "world"
      Comma@12..13 ","
      Whitespace@13..14 " "
      Item@14..17
        Ident@14..17 "bye"
      RParen@17..18 ")""#]];

	let diags = expect![[r#""#]];

	harness(
		source,
		|p| {
			p.comma_sep_list(T![')'], |p| {
				let b = p.api.start_node(SyntaxKind::Item);
				p.expect(T![ident]);
				p.api.finish_node(b);
			});
		},
		ast,
		diags,
	);

	let source = "hello, world, bye,)";

	let ast = expect![[r#"
    File@0..19
      Item@0..5
        Ident@0..5 "hello"
      Comma@5..6 ","
      Whitespace@6..7 " "
      Item@7..12
        Ident@7..12 "world"
      Comma@12..13 ","
      Whitespace@13..14 " "
      Item@14..17
        Ident@14..17 "bye"
      Comma@17..18 ","
      RParen@18..19 ")""#]];

	let diags = expect![[r#""#]];

	harness(
		source,
		|p| {
			p.comma_sep_list(T![')'], |p| {
				let b = p.api.start_node(SyntaxKind::Item);
				p.expect(T![ident]);
				p.api.finish_node(b);
			});
		},
		ast,
		diags,
	);

	let source = "hello, world bye,)";

	let ast = expect![[r#"
    File@0..18
      Item@0..5
        Ident@0..5 "hello"
      Comma@5..6 ","
      Whitespace@6..7 " "
      Item@7..12
        Ident@7..12 "world"
      Whitespace@12..13 " "
      Err@13..17
        Ident@13..16 "bye"
        Comma@16..17 ","
      RParen@17..18 ")""#]];

	let diags = expect![[r#"
    Error: expected `,` or `)`
       ,-[<unknown>:1:14]
       |
     1 | hello, world bye,)
       *              ^|^  
       *               `--- found `<ident>`
    ---'
"#]];

	harness(
		source,
		|p| {
			p.comma_sep_list(T![')'], |p| {
				let b = p.api.start_node(SyntaxKind::Item);
				p.expect(T![ident]);
				p.api.finish_node(b);
			});
		},
		ast,
		diags,
	);
}

#[test]
fn token_tree() {
	let source = "({ something; a() + b })[]";

	let ast = expect![[r#"
    File@0..26
      TokenTree@0..24
        LParen@0..1 "("
        LBrace@1..2 "{"
        Whitespace@2..3 " "
        Ident@3..12 "something"
        Semi@12..13 ";"
        Whitespace@13..14 " "
        Ident@14..15 "a"
        LParen@15..16 "("
        RParen@16..17 ")"
        Whitespace@17..18 " "
        Plus@18..19 "+"
        Whitespace@19..20 " "
        Ident@20..21 "b"
        Whitespace@21..22 " "
        RBrace@22..23 "}"
        RParen@23..24 ")"
      TokenTree@24..26
        LBracket@24..25 "["
        RBracket@25..26 "]""#]];

	let diags = expect![[r#""#]];

	harness(
		source,
		|p| {
			p.token_tree(Delim::Paren);
			p.token_tree(Delim::Bracket);
		},
		ast,
		diags,
	);
}

#[test]
fn attributes() {
	let source = "@lang(u8)@lang(u16)\n@lang(u32)struct";

	let ast = expect![[r#"
    File@0..36
      Attributes@0..30
        Attribute@0..9
          At@0..1 "@"
          Ident@1..5 "lang"
          TokenTree@5..9
            LParen@5..6 "("
            Ident@6..8 "u8"
            RParen@8..9 ")"
        Attribute@9..19
          At@9..10 "@"
          Ident@10..14 "lang"
          TokenTree@14..19
            LParen@14..15 "("
            Ident@15..18 "u16"
            RParen@18..19 ")"
        Whitespace@19..20 "\n"
        Attribute@20..30
          At@20..21 "@"
          Ident@21..25 "lang"
          TokenTree@25..30
            LParen@25..26 "("
            Ident@26..29 "u32"
            RParen@29..30 ")"
      StructKw@30..36 "struct""#]];

	let diags = expect![[r#""#]];

	harness(
		source,
		|p| {
			p.attributes();
			p.expect(T![struct]);
		},
		ast,
		diags,
	);
}

fn file() -> Id<str> { Id::new(1) }

fn harness(source: &str, f: impl FnOnce(&mut Parser), ast: Expect, diagnostics: Expect) {
	let mut diags = Diagnostics::new();
	let mut ctx = TreeBuilderContext::new();

	let mut parser = Parser {
		api: Api::new(file(), source, &mut ctx),
		diags: &mut diags,
	};

	let b = parser.api.start_node(SyntaxKind::File);

	let res = catch_unwind(AssertUnwindSafe(|| {
		f(&mut parser);
	}));

	parser.api.finish_node(b);

	if let Err(e) = res {
		resume_unwind(e);
	}

	let node = parser.api.finish().finish();
	let res = ctx.finalize();

	let resolved = ResolvedNode::new_root_with_resolver(node, res);

	let text = resolved.text();
	assert_eq!(text, source, "CST is not lossless");

	let debug = fmt(&resolved);
	ast.assert_eq(&debug);

	let diags = diags.emit_test(source);
	diagnostics.assert_eq(&diags);
}

fn fmt(node: &ResolvedNode) -> String {
	let mut s = node.debug(node.resolver().as_ref(), true);
	s.pop();
	s
}
