use std::panic::{catch_unwind, resume_unwind, AssertUnwindSafe};

use diag::Diagnostics;
use expect_test::{expect, Expect};
use intern::Id;
use lex::{token::Delim, T};
use pretty_assertions::assert_eq;
use syntax::{builder::TreeBuilderContext, kind::SyntaxKind, ResolvedNode};

use crate::{Api, Parser};

#[test]
fn module() {
	let source = r#"
		struct S<T: Copy> where T: Clone {
			field: T,
			tuples: (T, T)
		}

		extern "C" fn main() {
			let s = S { field: 0, tuples: (0, 0) };
			core.io.print("{}", s.field);
		}
	"#;

	let ast = expect![[r#"
    File@0..179
      Item@0..72
        Struct@0..72
          Whitespace@0..3 "\n\t\t"
          StructKw@3..9 "struct"
          Whitespace@9..10 " "
          Ident@10..11 "S"
          Generics@11..20
            Lt@11..12 "<"
            Generic@12..19
              Ident@12..13 "T"
              Colon@13..14 ":"
              Type@14..19
                Path@14..19
                  Whitespace@14..15 " "
                  Ident@15..19 "Copy"
                  Generics@19..19
            Gt@19..20 ">"
          Where@20..35
            Whitespace@20..21 " "
            WhereKw@21..26 "where"
            WhereClause@26..35
              Type@26..28
                Path@26..28
                  Whitespace@26..27 " "
                  Ident@27..28 "T"
                  Generics@28..28
              Colon@28..29 ":"
              Type@29..35
                Path@29..35
                  Whitespace@29..30 " "
                  Ident@30..35 "Clone"
                  Generics@35..35
          EnumVariant@35..72
            Fields@35..72
              Whitespace@35..36 " "
              LBrace@36..37 "{"
              Field@37..49
                Whitespace@37..41 "\n\t\t\t"
                Ident@41..46 "field"
                Colon@46..47 ":"
                Type@47..49
                  Path@47..49
                    Whitespace@47..48 " "
                    Ident@48..49 "T"
                    Generics@49..49
              Comma@49..50 ","
              Field@50..68
                Whitespace@50..54 "\n\t\t\t"
                Ident@54..60 "tuples"
                Colon@60..61 ":"
                Type@61..68
                  Tuple@61..68
                    Whitespace@61..62 " "
                    LParen@62..63 "("
                    Type@63..64
                      Path@63..64
                        Ident@63..64 "T"
                        Generics@64..64
                    Comma@64..65 ","
                    Type@65..67
                      Path@65..67
                        Whitespace@65..66 " "
                        Ident@66..67 "T"
                        Generics@67..67
                    RParen@67..68 ")"
              Whitespace@68..71 "\n\t\t"
              RBrace@71..72 "}"
      Item@72..177
        Fn@72..177
          Whitespace@72..76 "\n\n\t\t"
          ExternKw@76..82 "extern"
          Whitespace@82..83 " "
          StringLit@83..86 "\"C\""
          Whitespace@86..87 " "
          FnKw@87..89 "fn"
          Whitespace@89..90 " "
          Ident@90..94 "main"
          Generics@94..94
          Args@94..96
            LParen@94..95 "("
            RParen@95..96 ")"
          Block@96..177
            Whitespace@96..97 " "
            LBrace@97..98 "{"
            SemiExpr@98..141
              Expr@98..140
                Expr@98..140
                  Whitespace@98..102 "\n\t\t\t"
                  LetKw@102..105 "let"
                  Pat@105..107
                    EnumVariant@105..107
                      Path@105..107
                        Whitespace@105..106 " "
                        Ident@106..107 "s"
                        Generics@107..107
                  Whitespace@107..108 " "
                  Eq@108..109 "="
                  Expr@109..140
                    StructLit@109..140
                      Expr@109..111
                        Whitespace@109..110 " "
                        Ident@110..111 "S"
                      Whitespace@111..112 " "
                      LBrace@112..113 "{"
                      Field@113..122
                        Whitespace@113..114 " "
                        Ident@114..119 "field"
                        Colon@119..120 ":"
                        Expr@120..122
                          Expr@120..122
                            Whitespace@120..121 " "
                            IntLit@121..122 "0"
                      Comma@122..123 ","
                      Field@123..138
                        Whitespace@123..124 " "
                        Ident@124..130 "tuples"
                        Colon@130..131 ":"
                        Expr@131..138
                          Expr@131..138
                            Tuple@131..138
                              Whitespace@131..132 " "
                              LParen@132..133 "("
                              Expr@133..134
                                Expr@133..134
                                  IntLit@133..134 "0"
                              Comma@134..135 ","
                              Expr@135..137
                                Expr@135..137
                                  Whitespace@135..136 " "
                                  IntLit@136..137 "0"
                              RParen@137..138 ")"
                      Whitespace@138..139 " "
                      RBrace@139..140 "}"
              Semi@140..141 ";"
            SemiExpr@141..173
              Expr@141..172
                Call@141..172
                  Access@141..157
                    Access@141..151
                      Expr@141..148
                        Whitespace@141..145 "\n\t\t\t"
                        Ident@145..148 "core"
                      Dot@148..149 "."
                      Ident@149..151 "io"
                    Dot@151..152 "."
                    Ident@152..157 "print"
                  Args@157..172
                    LParen@157..158 "("
                    Expr@158..162
                      Expr@158..162
                        StringLit@158..162 "\"{}\""
                    Comma@162..163 ","
                    Expr@163..171
                      Access@163..171
                        Expr@163..165
                          Whitespace@163..164 " "
                          Ident@164..165 "s"
                        Dot@165..166 "."
                        Ident@166..171 "field"
                    RParen@171..172 ")"
              Semi@172..173 ";"
            Whitespace@173..176 "\n\t\t"
            RBrace@176..177 "}"
      Whitespace@177..179 "\n\t""#]];

	let diags = expect![""];

	harness(source, |p| p.parse_inner(), ast, diags);
}

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
      Item@6..12
        Whitespace@6..7 " "
        Ident@7..12 "world"
      Comma@12..13 ","
      Item@13..17
        Whitespace@13..14 " "
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
      Item@6..12
        Whitespace@6..7 " "
        Ident@7..12 "world"
      Comma@12..13 ","
      Item@13..17
        Whitespace@13..14 " "
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
      Item@6..12
        Whitespace@6..7 " "
        Ident@7..12 "world"
      Err@12..17
        Whitespace@12..13 " "
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
      Attribute@19..30
        Whitespace@19..20 "\n"
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
	let source = "<T, U: T,>";

	let ast = expect![[r#"
    File@0..10
      Generics@0..10
        Lt@0..1 "<"
        Generic@1..2
          Ident@1..2 "T"
        Comma@2..3 ","
        Generic@3..8
          Whitespace@3..4 " "
          Ident@4..5 "U"
          Colon@5..6 ":"
          Type@6..8
            Path@6..8
              Whitespace@6..7 " "
              Ident@7..8 "T"
              Generics@8..8
        Comma@8..9 ","
        Gt@9..10 ">""#]];

	let diags = expect![""];

	harness(source, |p| p.generics_decl(), ast, diags);
}

#[test]
fn pat() {
	let source = r#"
		_;
		..;
		something.something;
		.scoped;
		0; 1.0; "hello";
		mut value;
		(tuple, tuple);
		Struct { field: (), something: else_, same, .. };
		Tuple(x, _);
	"#;

	let ast = expect![[r#"
    File@0..164
      Pat@0..4
        Whitespace@0..3 "\n\t\t"
        Underscore@3..4 "_"
      Semi@4..5 ";"
      Pat@5..10
        Whitespace@5..8 "\n\t\t"
        Dot@8..9 "."
        Dot@9..10 "."
      Semi@10..11 ";"
      Pat@11..33
        EnumVariant@11..33
          Path@11..33
            Whitespace@11..14 "\n\t\t"
            Ident@14..23 "something"
            Generics@23..23
            Dot@23..24 "."
            Ident@24..33 "something"
            Generics@33..33
      Semi@33..34 ";"
      Pat@34..44
        EnumVariant@34..44
          Path@34..44
            Whitespace@34..37 "\n\t\t"
            Dot@37..38 "."
            Ident@38..44 "scoped"
            Generics@44..44
      Semi@44..45 ";"
      Pat@45..49
        Whitespace@45..48 "\n\t\t"
        IntLit@48..49 "0"
      Semi@49..50 ";"
      Pat@50..54
        Whitespace@50..51 " "
        FloatLit@51..54 "1.0"
      Semi@54..55 ";"
      Pat@55..63
        Whitespace@55..56 " "
        StringLit@56..63 "\"hello\""
      Semi@63..64 ";"
      Pat@64..76
        Whitespace@64..67 "\n\t\t"
        MutKw@67..70 "mut"
        Whitespace@70..71 " "
        Ident@71..76 "value"
      Semi@76..77 ";"
      Pat@77..94
        Tuple@77..94
          Whitespace@77..80 "\n\t\t"
          LParen@80..81 "("
          Pat@81..86
            EnumVariant@81..86
              Path@81..86
                Ident@81..86 "tuple"
                Generics@86..86
          Comma@86..87 ","
          Pat@87..93
            EnumVariant@87..93
              Path@87..93
                Whitespace@87..88 " "
                Ident@88..93 "tuple"
                Generics@93..93
          RParen@93..94 ")"
      Semi@94..95 ";"
      Pat@95..146
        EnumVariant@95..146
          Path@95..104
            Whitespace@95..98 "\n\t\t"
            Ident@98..104 "Struct"
            Generics@104..104
          Fields@104..146
            Whitespace@104..105 " "
            LBrace@105..106 "{"
            Field@106..116
              Whitespace@106..107 " "
              Ident@107..112 "field"
              Colon@112..113 ":"
              Pat@113..116
                Tuple@113..116
                  Whitespace@113..114 " "
                  LParen@114..115 "("
                  RParen@115..116 ")"
            Comma@116..117 ","
            Field@117..134
              Whitespace@117..118 " "
              Ident@118..127 "something"
              Colon@127..128 ":"
              Pat@128..134
                EnumVariant@128..134
                  Path@128..134
                    Whitespace@128..129 " "
                    Ident@129..134 "else_"
                    Generics@134..134
            Comma@134..135 ","
            Field@135..140
              Whitespace@135..136 " "
              Ident@136..140 "same"
            Comma@140..141 ","
            Field@141..144
              Pat@141..144
                Whitespace@141..142 " "
                Dot@142..143 "."
                Dot@143..144 "."
            Whitespace@144..145 " "
            RBrace@145..146 "}"
      Semi@146..147 ";"
      Pat@147..161
        EnumVariant@147..161
          Path@147..155
            Whitespace@147..150 "\n\t\t"
            Ident@150..155 "Tuple"
            Generics@155..155
          Tuple@155..161
            LParen@155..156 "("
            Pat@156..157
              EnumVariant@156..157
                Path@156..157
                  Ident@156..157 "x"
                  Generics@157..157
            Comma@157..158 ","
            Pat@158..160
              Whitespace@158..159 " "
              Underscore@159..160 "_"
            RParen@160..161 ")"
      Semi@161..162 ";"
      Whitespace@162..164 "\n\t""#]];

	let diags = expect![""];

	harness(
		source,
		|p| {
			while !matches!(p.api.peek().kind, T![eof]) {
				p.pat(&[T![;]]);
				p.expect(T![;], &[T![;]]);
			}
		},
		ast,
		diags,
	);
}

#[test]
fn ty() {
	let source = r#"
	core.vec.Vec<T>;
	(ty, ty<_>, x + y);
	type(2);
	*const T;
	"#;

	let ast = expect![[r#"
    File@0..61
      Type@0..16
        Path@0..16
          Whitespace@0..2 "\n\t"
          Ident@2..5 "core"
          Generics@5..5
          Dot@5..6 "."
          Ident@6..9 "vec"
          Generics@9..9
          Dot@9..10 "."
          Ident@10..13 "Vec"
          Generics@13..16
            Lt@13..14 "<"
            Type@14..15
              Path@14..15
                Ident@14..15 "T"
                Generics@15..15
            Gt@15..16 ">"
      Semi@16..17 ";"
      Type@17..37
        Tuple@17..37
          Whitespace@17..19 "\n\t"
          LParen@19..20 "("
          Type@20..22
            Path@20..22
              Ident@20..22 "ty"
              Generics@22..22
          Comma@22..23 ","
          Type@23..29
            Path@23..29
              Whitespace@23..24 " "
              Ident@24..26 "ty"
              Generics@26..29
                Lt@26..27 "<"
                Type@27..28
                  Underscore@27..28 "_"
                Gt@28..29 ">"
          Comma@29..30 ","
          Type@30..36
            SumType@30..36
              Type@30..32
                Path@30..32
                  Whitespace@30..31 " "
                  Ident@31..32 "x"
                  Generics@32..32
              Whitespace@32..33 " "
              Plus@33..34 "+"
              Type@34..36
                Path@34..36
                  Whitespace@34..35 " "
                  Ident@35..36 "y"
                  Generics@36..36
          RParen@36..37 ")"
      Semi@37..38 ";"
      Type@38..47
        TypeOf@38..47
          Whitespace@38..40 "\n\t"
          TypeKw@40..44 "type"
          LParen@44..45 "("
          Expr@45..46
            Expr@45..46
              IntLit@45..46 "2"
          RParen@46..47 ")"
      Semi@47..48 ";"
      Type@48..58
        Ptr@48..58
          Whitespace@48..50 "\n\t"
          Star@50..51 "*"
          ConstKw@51..56 "const"
          Type@56..58
            Path@56..58
              Whitespace@56..57 " "
              Ident@57..58 "T"
              Generics@58..58
      Semi@58..59 ";"
      Whitespace@59..61 "\n\t""#]];

	let diags = expect![""];

	harness(
		source,
		|p| {
			while !matches!(p.api.peek().kind, T![eof]) {
				p.ty();
				p.expect(T![;], &[T![;]]);
			}
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
        Where@13..24
          Whitespace@13..14 " "
          WhereKw@14..19 "where"
          WhereClause@19..24
            Type@19..21
              Path@19..21
                Whitespace@19..20 " "
                Ident@20..21 "T"
                Generics@21..21
            Colon@21..22 ":"
            Type@22..24
              Path@22..24
                Whitespace@22..23 " "
                Ident@23..24 "T"
                Generics@24..24
        EnumVariant@24..38
          Fields@24..38
            Whitespace@24..25 " "
            LBrace@25..26 "{"
            Field@26..35
              Whitespace@26..27 " "
              Ident@27..32 "inner"
              Colon@32..33 ":"
              Type@33..35
                Path@33..35
                  Whitespace@33..34 " "
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
	let source = "fn something<T: bussy>(p: T, (s, v): (T, T)) where T: sus { () }";

	let ast = expect![[r#"
    File@0..64
      FnKw@0..2 "fn"
      Whitespace@2..3 " "
      Ident@3..12 "something"
      Generics@12..22
        Lt@12..13 "<"
        Generic@13..21
          Ident@13..14 "T"
          Colon@14..15 ":"
          Type@15..21
            Path@15..21
              Whitespace@15..16 " "
              Ident@16..21 "bussy"
              Generics@21..21
        Gt@21..22 ">"
      Args@22..44
        LParen@22..23 "("
        Arg@23..27
          Pat@23..24
            EnumVariant@23..24
              Path@23..24
                Ident@23..24 "p"
                Generics@24..24
          Colon@24..25 ":"
          Type@25..27
            Path@25..27
              Whitespace@25..26 " "
              Ident@26..27 "T"
              Generics@27..27
        Comma@27..28 ","
        Arg@28..43
          Pat@28..35
            Tuple@28..35
              Whitespace@28..29 " "
              LParen@29..30 "("
              Pat@30..31
                EnumVariant@30..31
                  Path@30..31
                    Ident@30..31 "s"
                    Generics@31..31
              Comma@31..32 ","
              Pat@32..34
                EnumVariant@32..34
                  Path@32..34
                    Whitespace@32..33 " "
                    Ident@33..34 "v"
                    Generics@34..34
              RParen@34..35 ")"
          Colon@35..36 ":"
          Type@36..43
            Tuple@36..43
              Whitespace@36..37 " "
              LParen@37..38 "("
              Type@38..39
                Path@38..39
                  Ident@38..39 "T"
                  Generics@39..39
              Comma@39..40 ","
              Type@40..42
                Path@40..42
                  Whitespace@40..41 " "
                  Ident@41..42 "T"
                  Generics@42..42
              RParen@42..43 ")"
        RParen@43..44 ")"
      Where@44..57
        Whitespace@44..45 " "
        WhereKw@45..50 "where"
        WhereClause@50..57
          Type@50..52
            Path@50..52
              Whitespace@50..51 " "
              Ident@51..52 "T"
              Generics@52..52
          Colon@52..53 ":"
          Type@53..57
            Path@53..57
              Whitespace@53..54 " "
              Ident@54..57 "sus"
              Generics@57..57
      Block@57..64
        Whitespace@57..58 " "
        LBrace@58..59 "{"
        Expr@59..62
          Expr@59..62
            Tuple@59..62
              Whitespace@59..60 " "
              LParen@60..61 "("
              RParen@61..62 ")"
        Whitespace@62..63 " "
        RBrace@63..64 "}""#]];

	let diags = expect![""];

	harness(source, |p| p.fn_(), ast, diags);
}

#[test]
fn atom() {
	let source = r#"
		();
		(x);
		(x, x);
		34; "hello"; 4.5; 'c'; true; false;
		break;
		continue;
		return 10;
		loop { hello; } while false;
		while true { hello; };
		for x in y { hello; };
		if true { hello; } else if false { hello; } else { hello; };
	"#;

	let ast = expect![[r#"
    File@0..241
      Tuple@0..5
        Whitespace@0..3 "\n\t\t"
        LParen@3..4 "("
        RParen@4..5 ")"
      Semi@5..6 ";"
      Paren@6..12
        Whitespace@6..9 "\n\t\t"
        LParen@9..10 "("
        Expr@10..11
          Expr@10..11
            Ident@10..11 "x"
        RParen@11..12 ")"
      Semi@12..13 ";"
      Tuple@13..22
        Whitespace@13..16 "\n\t\t"
        LParen@16..17 "("
        Expr@17..18
          Expr@17..18
            Ident@17..18 "x"
        Comma@18..19 ","
        Expr@19..21
          Expr@19..21
            Whitespace@19..20 " "
            Ident@20..21 "x"
        RParen@21..22 ")"
      Semi@22..23 ";"
      Whitespace@23..26 "\n\t\t"
      IntLit@26..28 "34"
      Semi@28..29 ";"
      Whitespace@29..30 " "
      StringLit@30..37 "\"hello\""
      Semi@37..38 ";"
      Whitespace@38..39 " "
      FloatLit@39..42 "4.5"
      Semi@42..43 ";"
      Whitespace@43..44 " "
      CharLit@44..47 "'c'"
      Semi@47..48 ";"
      Whitespace@48..49 " "
      BoolLit@49..53 "true"
      Semi@53..54 ";"
      Whitespace@54..55 " "
      BoolLit@55..60 "false"
      Semi@60..61 ";"
      Break@61..69
        Whitespace@61..64 "\n\t\t"
        BreakKw@64..69 "break"
      Semi@69..70 ";"
      Continue@70..81
        Whitespace@70..73 "\n\t\t"
        ContinueKw@73..81 "continue"
      Semi@81..82 ";"
      Return@82..94
        Whitespace@82..85 "\n\t\t"
        ReturnKw@85..91 "return"
        Expr@91..94
          Expr@91..94
            Whitespace@91..92 " "
            IntLit@92..94 "10"
      Semi@94..95 ";"
      Loop@95..125
        Whitespace@95..98 "\n\t\t"
        LoopKw@98..102 "loop"
        Block@102..113
          Whitespace@102..103 " "
          LBrace@103..104 "{"
          SemiExpr@104..111
            Expr@104..110
              Expr@104..110
                Whitespace@104..105 " "
                Ident@105..110 "hello"
            Semi@110..111 ";"
          Whitespace@111..112 " "
          RBrace@112..113 "}"
        Whitespace@113..114 " "
        WhileKw@114..119 "while"
        Expr@119..125
          Expr@119..125
            Whitespace@119..120 " "
            BoolLit@120..125 "false"
      Semi@125..126 ";"
      While@126..150
        Whitespace@126..129 "\n\t\t"
        WhileKw@129..134 "while"
        Expr@134..139
          Expr@134..139
            Whitespace@134..135 " "
            BoolLit@135..139 "true"
        Block@139..150
          Whitespace@139..140 " "
          LBrace@140..141 "{"
          SemiExpr@141..148
            Expr@141..147
              Expr@141..147
                Whitespace@141..142 " "
                Ident@142..147 "hello"
            Semi@147..148 ";"
          Whitespace@148..149 " "
          RBrace@149..150 "}"
      Semi@150..151 ";"
      For@151..175
        Whitespace@151..154 "\n\t\t"
        ForKw@154..157 "for"
        Pat@157..159
          EnumVariant@157..159
            Path@157..159
              Whitespace@157..158 " "
              Ident@158..159 "x"
              Generics@159..159
        Whitespace@159..160 " "
        InKw@160..162 "in"
        Expr@162..164
          Expr@162..164
            Whitespace@162..163 " "
            Ident@163..164 "y"
        Block@164..175
          Whitespace@164..165 " "
          LBrace@165..166 "{"
          SemiExpr@166..173
            Expr@166..172
              Expr@166..172
                Whitespace@166..167 " "
                Ident@167..172 "hello"
            Semi@172..173 ";"
          Whitespace@173..174 " "
          RBrace@174..175 "}"
      Semi@175..176 ";"
      If@176..238
        Whitespace@176..179 "\n\t\t"
        IfKw@179..181 "if"
        Expr@181..186
          Expr@181..186
            Whitespace@181..182 " "
            BoolLit@182..186 "true"
        Block@186..197
          Whitespace@186..187 " "
          LBrace@187..188 "{"
          SemiExpr@188..195
            Expr@188..194
              Expr@188..194
                Whitespace@188..189 " "
                Ident@189..194 "hello"
            Semi@194..195 ";"
          Whitespace@195..196 " "
          RBrace@196..197 "}"
        Else@197..238
          Whitespace@197..198 " "
          ElseKw@198..202 "else"
          Expr@202..238
            Expr@202..238
              If@202..238
                Whitespace@202..203 " "
                IfKw@203..205 "if"
                Expr@205..211
                  Expr@205..211
                    Whitespace@205..206 " "
                    BoolLit@206..211 "false"
                Block@211..222
                  Whitespace@211..212 " "
                  LBrace@212..213 "{"
                  SemiExpr@213..220
                    Expr@213..219
                      Expr@213..219
                        Whitespace@213..214 " "
                        Ident@214..219 "hello"
                    Semi@219..220 ";"
                  Whitespace@220..221 " "
                  RBrace@221..222 "}"
                Else@222..238
                  Whitespace@222..223 " "
                  ElseKw@223..227 "else"
                  Expr@227..238
                    Expr@227..238
                      Block@227..238
                        Whitespace@227..228 " "
                        LBrace@228..229 "{"
                        SemiExpr@229..236
                          Expr@229..235
                            Expr@229..235
                              Whitespace@229..230 " "
                              Ident@230..235 "hello"
                          Semi@235..236 ";"
                        Whitespace@236..237 " "
                        RBrace@237..238 "}"
      Semi@238..239 ";"
      Whitespace@239..241 "\n\t""#]];

	let diags = expect![""];

	harness(
		source,
		|p| {
			while !matches!(p.api.peek().kind, T![eof]) {
				p.atom();
				p.expect(T![;], &[T![;]]);
			}
		},
		ast,
		diags,
	);
}

#[test]
fn prefix() {
	let source = r#"
		-10;
		!true;
		*ptr;
		&ptr;
		&mut ptr;
		--!&something;
	"#;

	let ast = expect![[r#"
    File@0..63
      Expr@0..6
        Expr@0..6
          Whitespace@0..3 "\n\t\t"
          IntLit@3..6 "-10"
      Semi@6..7 ";"
      Expr@7..15
        Prefix@7..15
          Op@7..11
            Whitespace@7..10 "\n\t\t"
            Exclaim@10..11 "!"
          Expr@11..15
            Expr@11..15
              BoolLit@11..15 "true"
      Semi@15..16 ";"
      Expr@16..23
        Prefix@16..23
          Op@16..20
            Whitespace@16..19 "\n\t\t"
            Star@19..20 "*"
          Expr@20..23
            Expr@20..23
              Ident@20..23 "ptr"
      Semi@23..24 ";"
      Expr@24..31
        Prefix@24..31
          Op@24..28
            Whitespace@24..27 "\n\t\t"
            And@27..28 "&"
          Expr@28..31
            Expr@28..31
              Ident@28..31 "ptr"
      Semi@31..32 ";"
      Expr@32..43
        Prefix@32..43
          Op@32..39
            Whitespace@32..35 "\n\t\t"
            And@35..36 "&"
            MutKw@36..39 "mut"
          Expr@39..43
            Expr@39..43
              Whitespace@39..40 " "
              Ident@40..43 "ptr"
      Semi@43..44 ";"
      Expr@44..60
        Prefix@44..60
          Op@44..48
            Whitespace@44..47 "\n\t\t"
            Minus@47..48 "-"
          Expr@48..60
            Prefix@48..60
              Op@48..49
                Minus@48..49 "-"
              Expr@49..60
                Prefix@49..60
                  Op@49..50
                    Exclaim@49..50 "!"
                  Expr@50..60
                    Prefix@50..60
                      Op@50..51
                        And@50..51 "&"
                      Expr@51..60
                        Expr@51..60
                          Ident@51..60 "something"
      Semi@60..61 ";"
      Whitespace@61..63 "\n\t""#]];

	let diags = expect![""];

	harness(
		source,
		|p| {
			while !matches!(p.api.peek().kind, T![eof]) {
				p.expr(true);
				p.expect(T![;], &[T![;]]);
			}
		},
		ast,
		diags,
	);
}

#[test]
fn infix() {
	let source = r#"
		10 + 20;
		10 + --20;
		10 + 4 * 5;
		5 && 6 == 3;
		a = b = c + 3;
	"#;

	let ast = expect![[r#"
    File@0..72
      Expr@0..10
        Infix@0..10
          Expr@0..5
            Whitespace@0..3 "\n\t\t"
            IntLit@3..5 "10"
          Op@5..7
            Whitespace@5..6 " "
            Plus@6..7 "+"
          Expr@7..10
            Expr@7..10
              Whitespace@7..8 " "
              IntLit@8..10 "20"
      Semi@10..11 ";"
      Expr@11..23
        Infix@11..23
          Expr@11..16
            Whitespace@11..14 "\n\t\t"
            IntLit@14..16 "10"
          Op@16..18
            Whitespace@16..17 " "
            Plus@17..18 "+"
          Expr@18..23
            Prefix@18..23
              Op@18..20
                Whitespace@18..19 " "
                Minus@19..20 "-"
              Expr@20..23
                Expr@20..23
                  IntLit@20..23 "-20"
      Semi@23..24 ";"
      Expr@24..37
        Infix@24..37
          Expr@24..29
            Whitespace@24..27 "\n\t\t"
            IntLit@27..29 "10"
          Op@29..31
            Whitespace@29..30 " "
            Plus@30..31 "+"
          Expr@31..37
            Infix@31..37
              Expr@31..33
                Whitespace@31..32 " "
                IntLit@32..33 "4"
              Op@33..35
                Whitespace@33..34 " "
                Star@34..35 "*"
              Expr@35..37
                Expr@35..37
                  Whitespace@35..36 " "
                  IntLit@36..37 "5"
      Semi@37..38 ";"
      Expr@38..52
        Infix@38..52
          Expr@38..42
            Whitespace@38..41 "\n\t\t"
            IntLit@41..42 "5"
          Op@42..45
            Whitespace@42..43 " "
            And@43..44 "&"
            And@44..45 "&"
          Expr@45..52
            Infix@45..52
              Expr@45..47
                Whitespace@45..46 " "
                IntLit@46..47 "6"
              Op@47..50
                Whitespace@47..48 " "
                Eq@48..49 "="
                Eq@49..50 "="
              Expr@50..52
                Expr@50..52
                  Whitespace@50..51 " "
                  IntLit@51..52 "3"
      Semi@52..53 ";"
      Expr@53..69
        Infix@53..69
          Expr@53..57
            Whitespace@53..56 "\n\t\t"
            Ident@56..57 "a"
          Op@57..59
            Whitespace@57..58 " "
            Eq@58..59 "="
          Expr@59..69
            Infix@59..69
              Expr@59..61
                Whitespace@59..60 " "
                Ident@60..61 "b"
              Op@61..63
                Whitespace@61..62 " "
                Eq@62..63 "="
              Expr@63..69
                Infix@63..69
                  Expr@63..65
                    Whitespace@63..64 " "
                    Ident@64..65 "c"
                  Op@65..67
                    Whitespace@65..66 " "
                    Plus@66..67 "+"
                  Expr@67..69
                    Expr@67..69
                      Whitespace@67..68 " "
                      IntLit@68..69 "3"
      Semi@69..70 ";"
      Whitespace@70..72 "\n\t""#]];

	let diags = expect![""];

	harness(
		source,
		|p| {
			while !matches!(p.api.peek().kind, T![eof]) {
				p.expr(true);
				p.expect(T![;], &[T![;]]);
			}
		},
		ast,
		diags,
	);
}

#[test]
fn postfix() {
	let source = r#"
		x();
		x[10];
		y.a;
		hello()[0].x();
		x.<T>();
		Struct { a: true, b };
		10 : i32;
		10 : i32();
		(Struct : Trait).function();
	"#;

	let ast = expect![[r#"
    File@0..136
      Expr@0..6
        Call@0..6
          Expr@0..4
            Whitespace@0..3 "\n\t\t"
            Ident@3..4 "x"
          Args@4..6
            LParen@4..5 "("
            RParen@5..6 ")"
      Semi@6..7 ";"
      Expr@7..15
        Index@7..15
          Expr@7..11
            Whitespace@7..10 "\n\t\t"
            Ident@10..11 "x"
          LBracket@11..12 "["
          Expr@12..14
            Expr@12..14
              IntLit@12..14 "10"
          RBracket@14..15 "]"
      Semi@15..16 ";"
      Expr@16..22
        Access@16..22
          Expr@16..20
            Whitespace@16..19 "\n\t\t"
            Ident@19..20 "y"
          Dot@20..21 "."
          Ident@21..22 "a"
      Semi@22..23 ";"
      Expr@23..40
        Call@23..40
          Access@23..38
            Index@23..36
              Call@23..33
                Expr@23..31
                  Whitespace@23..26 "\n\t\t"
                  Ident@26..31 "hello"
                Args@31..33
                  LParen@31..32 "("
                  RParen@32..33 ")"
              LBracket@33..34 "["
              Expr@34..35
                Expr@34..35
                  IntLit@34..35 "0"
              RBracket@35..36 "]"
            Dot@36..37 "."
            Ident@37..38 "x"
          Args@38..40
            LParen@38..39 "("
            RParen@39..40 ")"
      Semi@40..41 ";"
      Expr@41..51
        Call@41..51
          Access@41..49
            Expr@41..45
              Whitespace@41..44 "\n\t\t"
              Ident@44..45 "x"
            Dot@45..46 "."
            Generics@46..49
              Lt@46..47 "<"
              Type@47..48
                Path@47..48
                  Ident@47..48 "T"
                  Generics@48..48
              Gt@48..49 ">"
          Args@49..51
            LParen@49..50 "("
            RParen@50..51 ")"
      Semi@51..52 ";"
      Expr@52..76
        StructLit@52..76
          Expr@52..61
            Whitespace@52..55 "\n\t\t"
            Ident@55..61 "Struct"
          Whitespace@61..62 " "
          LBrace@62..63 "{"
          Field@63..71
            Whitespace@63..64 " "
            Ident@64..65 "a"
            Colon@65..66 ":"
            Expr@66..71
              Expr@66..71
                Whitespace@66..67 " "
                BoolLit@67..71 "true"
          Comma@71..72 ","
          Field@72..74
            Whitespace@72..73 " "
            Ident@73..74 "b"
          Whitespace@74..75 " "
          RBrace@75..76 "}"
      Semi@76..77 ";"
      Expr@77..88
        Ascript@77..88
          Expr@77..82
            Whitespace@77..80 "\n\t\t"
            IntLit@80..82 "10"
          Whitespace@82..83 " "
          Colon@83..84 ":"
          Type@84..88
            Path@84..88
              Whitespace@84..85 " "
              Ident@85..88 "i32"
              Generics@88..88
      Semi@88..89 ";"
      Expr@89..102
        Call@89..102
          Ascript@89..100
            Expr@89..94
              Whitespace@89..92 "\n\t\t"
              IntLit@92..94 "10"
            Whitespace@94..95 " "
            Colon@95..96 ":"
            Type@96..100
              Path@96..100
                Whitespace@96..97 " "
                Ident@97..100 "i32"
                Generics@100..100
          Args@100..102
            LParen@100..101 "("
            RParen@101..102 ")"
      Semi@102..103 ";"
      Expr@103..133
        Call@103..133
          Access@103..131
            Expr@103..122
              Paren@103..122
                Whitespace@103..106 "\n\t\t"
                LParen@106..107 "("
                Expr@107..121
                  Ascript@107..121
                    Expr@107..113
                      Ident@107..113 "Struct"
                    Whitespace@113..114 " "
                    Colon@114..115 ":"
                    Type@115..121
                      Path@115..121
                        Whitespace@115..116 " "
                        Ident@116..121 "Trait"
                        Generics@121..121
                RParen@121..122 ")"
            Dot@122..123 "."
            Ident@123..131 "function"
          Args@131..133
            LParen@131..132 "("
            RParen@132..133 ")"
      Semi@133..134 ";"
      Whitespace@134..136 "\n\t""#]];

	let diags = expect![""];

	harness(
		source,
		|p| {
			while !matches!(p.api.peek().kind, T![eof]) {
				p.expr(true);
				p.expect(T![;], &[T![;]]);
			}
		},
		ast,
		diags,
	);
}

#[test]
fn match_() {
	let source = r#"
		match my_enum {
			.X => "X",
			.Y => "Y",
		}
	"#;

	let ast = expect![[r#"
    File@0..52
      Expr@0..50
        Expr@0..50
          Match@0..50
            Whitespace@0..3 "\n\t\t"
            MatchKw@3..8 "match"
            Expr@8..16
              Expr@8..16
                Whitespace@8..9 " "
                Ident@9..16 "my_enum"
            MatchArms@16..50
              Whitespace@16..17 " "
              LBrace@17..18 "{"
              MatchArm@18..31
                Pat@18..24
                  EnumVariant@18..24
                    Path@18..24
                      Whitespace@18..22 "\n\t\t\t"
                      Dot@22..23 "."
                      Ident@23..24 "X"
                      Generics@24..24
                Whitespace@24..25 " "
                Eq@25..26 "="
                Gt@26..27 ">"
                Expr@27..31
                  Expr@27..31
                    Whitespace@27..28 " "
                    StringLit@28..31 "\"X\""
              Comma@31..32 ","
              MatchArm@32..45
                Pat@32..38
                  EnumVariant@32..38
                    Path@32..38
                      Whitespace@32..36 "\n\t\t\t"
                      Dot@36..37 "."
                      Ident@37..38 "Y"
                      Generics@38..38
                Whitespace@38..39 " "
                Eq@39..40 "="
                Gt@40..41 ">"
                Expr@41..45
                  Expr@41..45
                    Whitespace@41..42 " "
                    StringLit@42..45 "\"Y\""
              Comma@45..46 ","
              Whitespace@46..49 "\n\t\t"
              RBrace@49..50 "}"
      Whitespace@50..52 "\n\t""#]];

	let diags = expect![""];

	harness(
		source,
		|p| {
			while !matches!(p.api.peek().kind, T![eof]) {
				p.expr(true);
			}
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
		silent: false,
	};

	let res = catch_unwind(AssertUnwindSafe(|| {
		f(&mut parser);
	}));

	let node = parser.api.finish().finish();

	if let Err(e) = res {
		resume_unwind(e);
	}

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
