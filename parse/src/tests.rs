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
			p.expect(T![ident], &[]);
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
			p.expect(T![fn], &[]);
			p.expect(T![fn], &[]);
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
			p.expect(T![fn], &[]);
		},
		ast,
		diags,
	);
}

#[test]
fn comma_sep_list() {
	let source = "hello, world, bye";

	let ast = expect![[r#"
    File@0..17
      Item@0..5
        Ident@0..5 "hello"
      Comma@5..6 ","
      Whitespace@6..7 " "
      Item@7..12
        Ident@7..12 "world"
      Comma@12..13 ","
      Whitespace@13..14 " "
      Item@14..17
        Ident@14..17 "bye""#]];

	let diags = expect![[r#""#]];

	harness(
		source,
		|p| {
			p.comma_sep_list(T![eof], |p| {
				let b = p.api.start_node(SyntaxKind::Item);
				p.expect(T![ident], &[]);
				p.api.finish_node(b);
			});
		},
		ast,
		diags,
	);

	let source = "hello, world, bye,";

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
      Comma@17..18 ",""#]];

	let diags = expect![[r#""#]];

	harness(
		source,
		|p| {
			p.comma_sep_list(T![eof], |p| {
				let b = p.api.start_node(SyntaxKind::Item);
				p.expect(T![ident], &[]);
				p.api.finish_node(b);
			});
		},
		ast,
		diags,
	);

	let source = "hello, world bye,";

	let ast = expect![[r#"
    File@0..17
      Item@0..5
        Ident@0..5 "hello"
      Comma@5..6 ","
      Whitespace@6..7 " "
      Item@7..12
        Ident@7..12 "world"
      Whitespace@12..13 " "
      Err@13..17
        Ident@13..16 "bye"
        Comma@16..17 ",""#]];

	let diags = expect![[r#"
    Error: expected `,` or `<eof>`
       ,-[<unknown>:1:14]
       |
     1 | hello, world bye,
       *              ^|^
       *               `--- found `<ident>`
    ---'
"#]];

	harness(
		source,
		|p| {
			p.comma_sep_list(T![eof], |p| {
				let b = p.api.start_node(SyntaxKind::Item);
				p.expect(T![ident], &[]);
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
			p.expect(T![struct], &[]);
		},
		ast,
		diags,
	);
}

#[test]
fn generics() {
	let source = "<T, U:,>";

	let ast = expect![[r#"
    File@0..8
      Generics@0..8
        Lt@0..1 "<"
        Generic@1..2
          Ident@1..2 "T"
        Comma@2..3 ","
        Whitespace@3..4 " "
        Generic@4..7
          Ident@4..5 "U"
          Colon@5..6 ":"
          GenericBound@6..7
            Type@6..7
              Err@6..7
                Comma@6..7 ","
        Gt@7..8 ">""#]];

	let diags = expect![[r#"
    Error: expected one of: [ident], [.], ['('],
       ,-[<unknown>:1:7]
       |
     1 | <T, U:,>
       *       |
       *       `-- found `,`
    ---'
"#]];

	harness(source, |p| p.generics_decl(), ast, diags);
}

#[test]
fn path() {
	let source = "module.something;.module.something";

	let ast = expect![[r#"
    File@0..34
      Path@0..16
        Ident@0..6 "module"
        Dot@6..7 "."
        Ident@7..16 "something"
      Semi@16..17 ";"
      Path@17..34
        Dot@17..18 "."
        Ident@18..24 "module"
        Dot@24..25 "."
        Ident@25..34 "something""#]];

	let diags = expect![""];

	harness(
		source,
		|p| {
			p.path();
			p.expect(T![;], &[]);
			p.path();
		},
		ast,
		diags,
	);
}

#[test]
fn ty() {
	let source = "std.vec.Vec<T>(ty, ty<Y>)";

	let ast = expect![[r#"
    File@0..25
      Type@0..14
        Path@0..14
          Ident@0..3 "std"
          Generics@3..3
          Dot@3..4 "."
          Ident@4..7 "vec"
          Generics@7..7
          Dot@7..8 "."
          Ident@8..11 "Vec"
          Generics@11..14
            Lt@11..12 "<"
            Type@12..13
              Path@12..13
                Ident@12..13 "T"
                Generics@13..13
            Gt@13..14 ">"
      Type@14..25
        Tuple@14..25
          LParen@14..15 "("
          Type@15..17
            Path@15..17
              Ident@15..17 "ty"
              Generics@17..17
          Comma@17..18 ","
          Whitespace@18..19 " "
          Type@19..24
            Path@19..24
              Ident@19..21 "ty"
              Generics@21..24
                Lt@21..22 "<"
                Type@22..23
                  Path@22..23
                    Ident@22..23 "Y"
                    Generics@23..23
                Gt@23..24 ">"
          RParen@24..25 ")""#]];

	let diags = expect![""];

	harness(
		source,
		|p| {
			p.ty();
			p.ty();
		},
		ast,
		diags,
	);
}

#[test]
fn struct_() {
	let source = "struct Sus<T> where T: T { inner: T, }";

	let ast = expect![[r#"
    File@0..38
      Struct@0..38
        StructKw@0..6 "struct"
        Whitespace@6..7 " "
        Ident@7..10 "Sus"
        Generics@10..13
          Lt@10..11 "<"
          Generic@11..12
            Ident@11..12 "T"
          Gt@12..13 ">"
        Whitespace@13..14 " "
        Where@14..25
          WhereKw@14..19 "where"
          Whitespace@19..20 " "
          WhereClause@20..25
            Type@20..21
              Path@20..21
                Ident@20..21 "T"
                Generics@21..21
            Colon@21..22 ":"
            Whitespace@22..23 " "
            GenericBound@23..25
              Type@23..25
                Path@23..25
                  Ident@23..24 "T"
                  Whitespace@24..25 " "
                  Generics@25..25
        LBrace@25..26 "{"
        Whitespace@26..27 " "
        StructField@27..35
          Ident@27..32 "inner"
          Colon@32..33 ":"
          Whitespace@33..34 " "
          Type@34..35
            Path@34..35
              Ident@34..35 "T"
              Generics@35..35
        Comma@35..36 ","
        Whitespace@36..37 " "
        RBrace@37..38 "}""#]];

	let diags = expect![""];

	harness(source, |p| p.struct_(), ast, diags);
}

#[test]
fn fn_() {
	let source = "fn something<T: bussy>(p: T, (s, v): (T, T)) where T: sus { }";

	let ast = expect![[r#"
    File@0..61
      Fn@0..61
        FnKw@0..2 "fn"
        Whitespace@2..3 " "
        Ident@3..12 "something"
        Generics@12..22
          Lt@12..13 "<"
          Generic@13..21
            Ident@13..14 "T"
            Colon@14..15 ":"
            Whitespace@15..16 " "
            GenericBound@16..21
              Type@16..21
                Path@16..21
                  Ident@16..21 "bussy"
                  Generics@21..21
          Gt@21..22 ">"
        LParen@22..23 "("
        Arg@23..27
          Pat@23..24
            Ident@23..24 "p"
          Colon@24..25 ":"
          Whitespace@25..26 " "
          Type@26..27
            Path@26..27
              Ident@26..27 "T"
              Generics@27..27
        Comma@27..28 ","
        Whitespace@28..29 " "
        Arg@29..43
          Pat@29..35
            Tuple@29..35
              LParen@29..30 "("
              Pat@30..31
                Ident@30..31 "s"
              Comma@31..32 ","
              Whitespace@32..33 " "
              Pat@33..34
                Ident@33..34 "v"
              RParen@34..35 ")"
          Colon@35..36 ":"
          Whitespace@36..37 " "
          Type@37..43
            Tuple@37..43
              LParen@37..38 "("
              Type@38..39
                Path@38..39
                  Ident@38..39 "T"
                  Generics@39..39
              Comma@39..40 ","
              Whitespace@40..41 " "
              Type@41..42
                Path@41..42
                  Ident@41..42 "T"
                  Generics@42..42
              RParen@42..43 ")"
        RParen@43..44 ")"
        Whitespace@44..45 " "
        Where@45..58
          WhereKw@45..50 "where"
          Whitespace@50..51 " "
          WhereClause@51..58
            Type@51..52
              Path@51..52
                Ident@51..52 "T"
                Generics@52..52
            Colon@52..53 ":"
            Whitespace@53..54 " "
            GenericBound@54..58
              Type@54..58
                Path@54..58
                  Ident@54..57 "sus"
                  Whitespace@57..58 " "
                  Generics@58..58
        LBrace@58..59 "{"
        Whitespace@59..60 " "
        RBrace@60..61 "}""#]];

	let diags = expect![""];

	harness(source, |p| p.fn_(), ast, diags);
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
