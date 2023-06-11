use expect_test::expect;

use super::harness;

#[test]
fn expr() {
	let source = r#"
		fn main() {
			3 + ;
			4 + 5;
			-;
			f();
			b[;
			10;
		}
	"#;

	let ast = expect![[r#"
    File@0..67
      Whitespace@0..3 "\n\t\t"
      Item@3..67
        Fn@3..67
          FnKw@3..5 "fn"
          Whitespace@5..6 " "
          Name@6..10
            Ident@6..10 "main"
          ParamList@10..12
            LParen@10..11 "("
            RParen@11..12 ")"
          Whitespace@12..13 " "
          Block@13..65
            LBrace@13..14 "{"
            SemiExpr@14..23
              InfixExpr@14..22
                Whitespace@14..18 "\n\t\t\t"
                IntLit@18..19 "3"
                Whitespace@19..20 " "
                Plus@20..21 "+"
                Whitespace@21..22 " "
                Error@22..22
              Semi@22..23 ";"
            Whitespace@23..27 "\n\t\t\t"
            SemiExpr@27..33
              InfixExpr@27..32
                IntLit@27..28 "4"
                Whitespace@28..29 " "
                Plus@29..30 "+"
                Whitespace@30..31 " "
                IntLit@31..32 "5"
              Semi@32..33 ";"
            Whitespace@33..37 "\n\t\t\t"
            SemiExpr@37..39
              PrefixExpr@37..38
                Minus@37..38 "-"
                Error@38..38
              Semi@38..39 ";"
            Whitespace@39..43 "\n\t\t\t"
            SemiExpr@43..47
              CallExpr@43..46
                PathExpr@43..44
                  Path@43..44
                    Ident@43..44 "f"
                ArgList@44..46
                  LParen@44..45 "("
                  RParen@45..46 ")"
              Semi@46..47 ";"
            Whitespace@47..51 "\n\t\t\t"
            SemiExpr@51..54
              IndexExpr@51..53
                PathExpr@51..52
                  Path@51..52
                    Ident@51..52 "b"
                LBracket@52..53 "["
                Error@53..53
              Semi@53..54 ";"
            Whitespace@54..58 "\n\t\t\t"
            SemiExpr@58..61
              IntLit@58..60 "10"
              Semi@60..61 ";"
            Whitespace@61..64 "\n\t\t"
            RBrace@64..65 "}"
          Whitespace@65..67 "\n\t""#]];

	let diags = expect![[r#"
    Error: expected expression
       ,-[<unknown>:3:8]
       |
     3 |          3 + ;
       |              |  
       |              `-- found `;`
    ---'
    Error: expected expression
       ,-[<unknown>:5:5]
       |
     5 |          -;
       |           |  
       |           `-- found `;`
    ---'
    Error: expected expression
       ,-[<unknown>:7:6]
       |
     7 |          b[;
       |            |  
       |            `-- found `;`
    ---'
"#]];

	harness(source, ast, diags);
}

#[test]
fn item() {
	let source = r#"
		struct S {
			s: u8,,
			v: ,


		fn main() {
	"#;

	let ast = expect![[r#"
    File@0..50
      Whitespace@0..3 "\n\t\t"
      Item@3..37
        Struct@3..37
          StructKw@3..9 "struct"
          Whitespace@9..10 " "
          Name@10..11
            Ident@10..11 "S"
          Whitespace@11..12 " "
          LBrace@12..13 "{"
          Whitespace@13..17 "\n\t\t\t"
          Param@17..22
            Name@17..18
              Ident@17..18 "s"
            Colon@18..19 ":"
            Whitespace@19..20 " "
            PathType@20..22
              Path@20..22
                Ident@20..22 "u8"
          Comma@22..23 ","
          Param@23..23
            Name@23..23
              Error@23..23
          Comma@23..24 ","
          Whitespace@24..28 "\n\t\t\t"
          Param@28..31
            Name@28..29
              Ident@28..29 "v"
            Colon@29..30 ":"
            Whitespace@30..31 " "
            Error@31..31
          Comma@31..32 ","
          Whitespace@32..37 "\n\n\n\t\t"
          Param@37..37
            Name@37..37
              Error@37..37
      Item@37..50
        Fn@37..50
          FnKw@37..39 "fn"
          Whitespace@39..40 " "
          Name@40..44
            Ident@40..44 "main"
          ParamList@44..46
            LParen@44..45 "("
            RParen@45..46 ")"
          Whitespace@46..47 " "
          Block@47..50
            LBrace@47..48 "{"
            Whitespace@48..50 "\n\t"
            Error@50..50"#]];

	let diags = expect![[r#"
    Error: expected struct field
       ,-[<unknown>:3:10]
       |
     3 |          s: u8,,
       |                |  
       |                `-- found `,`
    ---'
    Error: expected type
       ,-[<unknown>:4:7]
       |
     4 |          v: ,
       |             |  
       |             `-- found `,`
    ---'
    Error: expected struct field
       ,-[<unknown>:7:3]
       |
     7 |        fn main() {
       |        ^|  
       |         `-- found `fn`
    ---'
    Error: expected block
       ,-[<unknown>:8:1]
       |
     8 | 
       | |  
       | `-- found <eof>
    ---'
"#]];

	harness(source, ast, diags);
}
