use expect_test::{expect, Expect};
use pretty_assertions::assert_eq;
use syntax::{builder::TreeBuilderContext, ResolvedNode};

use crate::Parser;

#[test]
fn attributes() {
	let source = r#"
		@lang(u16)
		@replace(x = y)
		@inline
		struct X {}
	"#;

	let ast = expect![[r#"
    File@0..57
      Item@0..55
        Attribute@0..13
          Whitespace@0..3 "\n\t\t"
          At@3..4 "@"
          Name@4..8
            Ident@4..8 "lang"
          TokenTree@8..13
            LParen@8..9 "("
            Ident@9..12 "u16"
            RParen@12..13 ")"
        Attribute@13..31
          Whitespace@13..16 "\n\t\t"
          At@16..17 "@"
          Name@17..24
            Ident@17..24 "replace"
          TokenTree@24..31
            LParen@24..25 "("
            Ident@25..26 "x"
            Whitespace@26..27 " "
            Eq@27..28 "="
            Whitespace@28..29 " "
            Ident@29..30 "y"
            RParen@30..31 ")"
        Attribute@31..41
          Whitespace@31..34 "\n\t\t"
          At@34..35 "@"
          Name@35..41
            Ident@35..41 "inline"
        Struct@41..55
          Whitespace@41..44 "\n\t\t"
          StructKw@44..50 "struct"
          Name@50..52
            Whitespace@50..51 " "
            Ident@51..52 "X"
          Whitespace@52..53 " "
          LBrace@53..54 "{"
          RBrace@54..55 "}"
      Whitespace@55..57 "\n\t""#]];

	let diags = expect![[r#""#]];

	harness(source, ast, diags);
}

#[test]
fn ty() {
	let source = r#"
		type Y = core.vec.Vec;
		type X = *T;
		type Z = [T; 10];
		type A = _;
	"#;

	let ast = {
		expect![[r#"
    File@0..76
      Item@0..25
        TypeAlias@0..25
          Whitespace@0..3 "\n\t\t"
          TypeKw@3..7 "type"
          Name@7..9
            Whitespace@7..8 " "
            Ident@8..9 "Y"
          Whitespace@9..10 " "
          Eq@10..11 "="
          PathType@11..24
            Path@11..24
              PathSegment@11..17
                Name@11..16
                  Whitespace@11..12 " "
                  Ident@12..16 "core"
                Dot@16..17 "."
              PathSegment@17..21
                Name@17..20
                  Ident@17..20 "vec"
                Dot@20..21 "."
              Name@21..24
                Ident@21..24 "Vec"
          Semi@24..25 ";"
      Item@25..40
        TypeAlias@25..40
          Whitespace@25..28 "\n\t\t"
          TypeKw@28..32 "type"
          Name@32..34
            Whitespace@32..33 " "
            Ident@33..34 "X"
          Whitespace@34..35 " "
          Eq@35..36 "="
          PtrType@36..39
            Whitespace@36..37 " "
            Star@37..38 "*"
            PathType@38..39
              Path@38..39
                Name@38..39
                  Ident@38..39 "T"
          Semi@39..40 ";"
      Item@40..60
        TypeAlias@40..60
          Whitespace@40..43 "\n\t\t"
          TypeKw@43..47 "type"
          Name@47..49
            Whitespace@47..48 " "
            Ident@48..49 "Z"
          Whitespace@49..50 " "
          Eq@50..51 "="
          ArrayType@51..59
            Whitespace@51..52 " "
            LBracket@52..53 "["
            PathType@53..54
              Path@53..54
                Name@53..54
                  Ident@53..54 "T"
            Semi@54..55 ";"
            Whitespace@55..56 " "
            IntLit@56..58 "10"
            RBracket@58..59 "]"
          Semi@59..60 ";"
      Item@60..74
        TypeAlias@60..74
          Whitespace@60..63 "\n\t\t"
          TypeKw@63..67 "type"
          Name@67..69
            Whitespace@67..68 " "
            Ident@68..69 "A"
          Whitespace@69..70 " "
          Eq@70..71 "="
          InferType@71..73
            Whitespace@71..72 " "
            Underscore@72..73 "_"
          Semi@73..74 ";"
      Whitespace@74..76 "\n\t""#]]
	};

	let diags = expect![""];

	harness(source, ast, diags);
}

#[test]
fn struct_() {
	let source = r#"
		struct Sus { inner: T, }
	"#;

	let ast = {
		expect![[r#"
    File@0..29
      Item@0..27
        Struct@0..27
          Whitespace@0..3 "\n\t\t"
          StructKw@3..9 "struct"
          Name@9..13
            Whitespace@9..10 " "
            Ident@10..13 "Sus"
          Whitespace@13..14 " "
          LBrace@14..15 "{"
          Param@15..24
            Name@15..21
              Whitespace@15..16 " "
              Ident@16..21 "inner"
            Colon@21..22 ":"
            PathType@22..24
              Path@22..24
                Name@22..24
                  Whitespace@22..23 " "
                  Ident@23..24 "T"
          Comma@24..25 ","
          Whitespace@25..26 " "
          RBrace@26..27 "}"
      Whitespace@27..29 "\n\t""#]]
	};

	let diags = expect![""];

	harness(source, ast, diags);
}

#[test]
fn fn_() {
	let source = r#"
		fn something(p: T) {}
		fn returns(x: i32, y: i32, z: i32) -> i32 { 10 }
	"#;

	let ast = {
		expect![[r#"
    File@0..77
      Item@0..24
        Fn@0..24
          Whitespace@0..3 "\n\t\t"
          FnKw@3..5 "fn"
          Name@5..15
            Whitespace@5..6 " "
            Ident@6..15 "something"
          ParamList@15..21
            LParen@15..16 "("
            Param@16..20
              Name@16..17
                Ident@16..17 "p"
              Colon@17..18 ":"
              PathType@18..20
                Path@18..20
                  Name@18..20
                    Whitespace@18..19 " "
                    Ident@19..20 "T"
            RParen@20..21 ")"
          Block@21..24
            Whitespace@21..22 " "
            LBrace@22..23 "{"
            RBrace@23..24 "}"
      Item@24..75
        Fn@24..75
          Whitespace@24..27 "\n\t\t"
          FnKw@27..29 "fn"
          Name@29..37
            Whitespace@29..30 " "
            Ident@30..37 "returns"
          ParamList@37..61
            LParen@37..38 "("
            Param@38..44
              Name@38..39
                Ident@38..39 "x"
              Colon@39..40 ":"
              PathType@40..44
                Path@40..44
                  Name@40..44
                    Whitespace@40..41 " "
                    Ident@41..44 "i32"
            Comma@44..45 ","
            Param@45..52
              Name@45..47
                Whitespace@45..46 " "
                Ident@46..47 "y"
              Colon@47..48 ":"
              PathType@48..52
                Path@48..52
                  Name@48..52
                    Whitespace@48..49 " "
                    Ident@49..52 "i32"
            Comma@52..53 ","
            Param@53..60
              Name@53..55
                Whitespace@53..54 " "
                Ident@54..55 "z"
              Colon@55..56 ":"
              PathType@56..60
                Path@56..60
                  Name@56..60
                    Whitespace@56..57 " "
                    Ident@57..60 "i32"
            RParen@60..61 ")"
          RetTy@61..68
            Whitespace@61..62 " "
            Arrow@62..64 "->"
            PathType@64..68
              Path@64..68
                Name@64..68
                  Whitespace@64..65 " "
                  Ident@65..68 "i32"
          Block@68..75
            Whitespace@68..69 " "
            LBrace@69..70 "{"
            Whitespace@70..71 " "
            IntLit@71..73 "10"
            Whitespace@73..74 " "
            RBrace@74..75 "}"
      Whitespace@75..77 "\n\t""#]]
	};

	let diags = expect![""];

	harness(source, ast, diags);
}

#[test]
fn atom() {
	let source = r#"
		fn main() {
			(x);
			34; "hello"; 4.5; 'c'; true; false;
			break;
			continue;
			return 10;
			loop {}
			loop { hello; } while false;
			while true { hello; }
			for x in y { hello; }
			if true { hello; } else if false { hello; } else { hello; }
		}
	"#;

	let ast = {
		expect![[r#"
    File@0..260
      Item@0..258
        Fn@0..258
          Whitespace@0..3 "\n\t\t"
          FnKw@3..5 "fn"
          Name@5..10
            Whitespace@5..6 " "
            Ident@6..10 "main"
          ParamList@10..12
            LParen@10..11 "("
            RParen@11..12 ")"
          Block@12..258
            Whitespace@12..13 " "
            LBrace@13..14 "{"
            SemiExpr@14..22
              ParenExpr@14..21
                Whitespace@14..18 "\n\t\t\t"
                LParen@18..19 "("
                PathExpr@19..20
                  Path@19..20
                    Name@19..20
                      Ident@19..20 "x"
                RParen@20..21 ")"
              Semi@21..22 ";"
            SemiExpr@22..29
              Whitespace@22..26 "\n\t\t\t"
              IntLit@26..28 "34"
              Semi@28..29 ";"
            SemiExpr@29..38
              Whitespace@29..30 " "
              StringLit@30..37 "\"hello\""
              Semi@37..38 ";"
            SemiExpr@38..43
              Whitespace@38..39 " "
              FloatLit@39..42 "4.5"
              Semi@42..43 ";"
            SemiExpr@43..48
              Whitespace@43..44 " "
              CharLit@44..47 "'c'"
              Semi@47..48 ";"
            SemiExpr@48..54
              Whitespace@48..49 " "
              BoolLit@49..53 "true"
              Semi@53..54 ";"
            SemiExpr@54..61
              Whitespace@54..55 " "
              BoolLit@55..60 "false"
              Semi@60..61 ";"
            SemiExpr@61..71
              BreakExpr@61..70
                Whitespace@61..65 "\n\t\t\t"
                BreakKw@65..70 "break"
              Semi@70..71 ";"
            SemiExpr@71..84
              Whitespace@71..75 "\n\t\t\t"
              ContinueKw@75..83 "continue"
              Semi@83..84 ";"
            SemiExpr@84..98
              ReturnExpr@84..97
                Whitespace@84..88 "\n\t\t\t"
                ReturnKw@88..94 "return"
                Whitespace@94..95 " "
                IntLit@95..97 "10"
              Semi@97..98 ";"
            LoopExpr@98..109
              Whitespace@98..102 "\n\t\t\t"
              LoopKw@102..106 "loop"
              Block@106..109
                Whitespace@106..107 " "
                LBrace@107..108 "{"
                RBrace@108..109 "}"
            SemiExpr@109..141
              LoopExpr@109..140
                Whitespace@109..113 "\n\t\t\t"
                LoopKw@113..117 "loop"
                Block@117..128
                  Whitespace@117..118 " "
                  LBrace@118..119 "{"
                  SemiExpr@119..126
                    PathExpr@119..125
                      Path@119..125
                        Name@119..125
                          Whitespace@119..120 " "
                          Ident@120..125 "hello"
                    Semi@125..126 ";"
                  Whitespace@126..127 " "
                  RBrace@127..128 "}"
                Whitespace@128..129 " "
                WhileKw@129..134 "while"
                Whitespace@134..135 " "
                BoolLit@135..140 "false"
              Semi@140..141 ";"
            WhileExpr@141..166
              Whitespace@141..145 "\n\t\t\t"
              WhileKw@145..150 "while"
              Whitespace@150..151 " "
              BoolLit@151..155 "true"
              Block@155..166
                Whitespace@155..156 " "
                LBrace@156..157 "{"
                SemiExpr@157..164
                  PathExpr@157..163
                    Path@157..163
                      Name@157..163
                        Whitespace@157..158 " "
                        Ident@158..163 "hello"
                  Semi@163..164 ";"
                Whitespace@164..165 " "
                RBrace@165..166 "}"
            ForExpr@166..191
              Whitespace@166..170 "\n\t\t\t"
              ForKw@170..173 "for"
              Name@173..175
                Whitespace@173..174 " "
                Ident@174..175 "x"
              Whitespace@175..176 " "
              InKw@176..178 "in"
              PathExpr@178..180
                Path@178..180
                  Name@178..180
                    Whitespace@178..179 " "
                    Ident@179..180 "y"
              Block@180..191
                Whitespace@180..181 " "
                LBrace@181..182 "{"
                SemiExpr@182..189
                  PathExpr@182..188
                    Path@182..188
                      Name@182..188
                        Whitespace@182..183 " "
                        Ident@183..188 "hello"
                  Semi@188..189 ";"
                Whitespace@189..190 " "
                RBrace@190..191 "}"
            IfExpr@191..254
              Whitespace@191..195 "\n\t\t\t"
              IfKw@195..197 "if"
              Whitespace@197..198 " "
              BoolLit@198..202 "true"
              Block@202..213
                Whitespace@202..203 " "
                LBrace@203..204 "{"
                SemiExpr@204..211
                  PathExpr@204..210
                    Path@204..210
                      Name@204..210
                        Whitespace@204..205 " "
                        Ident@205..210 "hello"
                  Semi@210..211 ";"
                Whitespace@211..212 " "
                RBrace@212..213 "}"
              Whitespace@213..214 " "
              ElseKw@214..218 "else"
              IfExpr@218..254
                Whitespace@218..219 " "
                IfKw@219..221 "if"
                Whitespace@221..222 " "
                BoolLit@222..227 "false"
                Block@227..238
                  Whitespace@227..228 " "
                  LBrace@228..229 "{"
                  SemiExpr@229..236
                    PathExpr@229..235
                      Path@229..235
                        Name@229..235
                          Whitespace@229..230 " "
                          Ident@230..235 "hello"
                    Semi@235..236 ";"
                  Whitespace@236..237 " "
                  RBrace@237..238 "}"
                Whitespace@238..239 " "
                ElseKw@239..243 "else"
                Block@243..254
                  Whitespace@243..244 " "
                  LBrace@244..245 "{"
                  SemiExpr@245..252
                    PathExpr@245..251
                      Path@245..251
                        Name@245..251
                          Whitespace@245..246 " "
                          Ident@246..251 "hello"
                    Semi@251..252 ";"
                  Whitespace@252..253 " "
                  RBrace@253..254 "}"
            Whitespace@254..257 "\n\t\t"
            RBrace@257..258 "}"
      Whitespace@258..260 "\n\t""#]]
	};

	let diags = expect![""];

	harness(source, ast, diags);
}

#[test]
fn prefix() {
	let source = r#"
		fn main() {
			-10;
			!true;
			*ptr;
			&ptr;
			&mut ptr;
			--!&something;
		}
	"#;

	let ast = {
		expect![[r#"
    File@0..87
      Item@0..85
        Fn@0..85
          Whitespace@0..3 "\n\t\t"
          FnKw@3..5 "fn"
          Name@5..10
            Whitespace@5..6 " "
            Ident@6..10 "main"
          ParamList@10..12
            LParen@10..11 "("
            RParen@11..12 ")"
          Block@12..85
            Whitespace@12..13 " "
            LBrace@13..14 "{"
            SemiExpr@14..22
              Whitespace@14..18 "\n\t\t\t"
              IntLit@18..21 "-10"
              Semi@21..22 ";"
            SemiExpr@22..32
              PrefixExpr@22..31
                Whitespace@22..26 "\n\t\t\t"
                Not@26..27 "!"
                BoolLit@27..31 "true"
              Semi@31..32 ";"
            SemiExpr@32..41
              PrefixExpr@32..40
                Whitespace@32..36 "\n\t\t\t"
                Star@36..37 "*"
                PathExpr@37..40
                  Path@37..40
                    Name@37..40
                      Ident@37..40 "ptr"
              Semi@40..41 ";"
            SemiExpr@41..50
              RefExpr@41..49
                Whitespace@41..45 "\n\t\t\t"
                Amp@45..46 "&"
                PathExpr@46..49
                  Path@46..49
                    Name@46..49
                      Ident@46..49 "ptr"
              Semi@49..50 ";"
            SemiExpr@50..63
              RefExpr@50..62
                Whitespace@50..54 "\n\t\t\t"
                Amp@54..55 "&"
                MutKw@55..58 "mut"
                PathExpr@58..62
                  Path@58..62
                    Name@58..62
                      Whitespace@58..59 " "
                      Ident@59..62 "ptr"
              Semi@62..63 ";"
            SemiExpr@63..81
              PrefixExpr@63..80
                Whitespace@63..67 "\n\t\t\t"
                Minus@67..68 "-"
                PrefixExpr@68..80
                  Minus@68..69 "-"
                  PrefixExpr@69..80
                    Not@69..70 "!"
                    RefExpr@70..80
                      Amp@70..71 "&"
                      PathExpr@71..80
                        Path@71..80
                          Name@71..80
                            Ident@71..80 "something"
              Semi@80..81 ";"
            Whitespace@81..84 "\n\t\t"
            RBrace@84..85 "}"
      Whitespace@85..87 "\n\t""#]]
	};

	let diags = expect![""];

	harness(source, ast, diags);
}

#[test]
fn infix() {
	let source = r#"
		fn main() {
			10 + 20;
			10 + --20;
			10 + 4 * 5;
			5 && 6 == 3;
			a = b = c + 3;
		}
	"#;

	let ast = {
		expect![[r#"
    File@0..95
      Item@0..93
        Fn@0..93
          Whitespace@0..3 "\n\t\t"
          FnKw@3..5 "fn"
          Name@5..10
            Whitespace@5..6 " "
            Ident@6..10 "main"
          ParamList@10..12
            LParen@10..11 "("
            RParen@11..12 ")"
          Block@12..93
            Whitespace@12..13 " "
            LBrace@13..14 "{"
            SemiExpr@14..26
              InfixExpr@14..25
                Whitespace@14..18 "\n\t\t\t"
                IntLit@18..20 "10"
                Whitespace@20..21 " "
                Plus@21..22 "+"
                Whitespace@22..23 " "
                IntLit@23..25 "20"
              Semi@25..26 ";"
            InfixExpr@26..36
              Whitespace@26..30 "\n\t\t\t"
              IntLit@30..32 "10"
              Whitespace@32..33 " "
              Plus@33..34 "+"
              Whitespace@34..35 " "
              Minus@35..36 "-"
            SemiExpr@36..40
              IntLit@36..39 "-20"
              Semi@39..40 ";"
            SemiExpr@40..55
              InfixExpr@40..54
                InfixExpr@40..50
                  Whitespace@40..44 "\n\t\t\t"
                  IntLit@44..46 "10"
                  Whitespace@46..47 " "
                  Plus@47..48 "+"
                  Whitespace@48..49 " "
                  IntLit@49..50 "4"
                Whitespace@50..51 " "
                Star@51..52 "*"
                Whitespace@52..53 " "
                IntLit@53..54 "5"
              Semi@54..55 ";"
            SemiExpr@55..71
              InfixExpr@55..70
                InfixExpr@55..65
                  Whitespace@55..59 "\n\t\t\t"
                  IntLit@59..60 "5"
                  Whitespace@60..61 " "
                  AmpAmp@61..63 "&&"
                  Whitespace@63..64 " "
                  IntLit@64..65 "6"
                Whitespace@65..66 " "
                EqEq@66..68 "=="
                Whitespace@68..69 " "
                IntLit@69..70 "3"
              Semi@70..71 ";"
            SemiExpr@71..89
              InfixExpr@71..88
                InfixExpr@71..84
                  InfixExpr@71..80
                    PathExpr@71..76
                      Path@71..76
                        Name@71..76
                          Whitespace@71..75 "\n\t\t\t"
                          Ident@75..76 "a"
                    Whitespace@76..77 " "
                    Eq@77..78 "="
                    Whitespace@78..79 " "
                    Ident@79..80 "b"
                  Whitespace@80..81 " "
                  Eq@81..82 "="
                  Whitespace@82..83 " "
                  Ident@83..84 "c"
                Whitespace@84..85 " "
                Plus@85..86 "+"
                Whitespace@86..87 " "
                IntLit@87..88 "3"
              Semi@88..89 ";"
            Whitespace@89..92 "\n\t\t"
            RBrace@92..93 "}"
      Whitespace@93..95 "\n\t""#]]
	};

	let diags = expect![""];

	harness(source, ast, diags);
}

#[test]
fn postfix() {
	let source = r#"
		fn main() {
			x();
			x[10];
			y.a;
			hello()[0].x();
			10 as f32;
		}
	"#;

	let ast = {
		expect![[r#"
    File@0..79
      Item@0..77
        Fn@0..77
          Whitespace@0..3 "\n\t\t"
          FnKw@3..5 "fn"
          Name@5..10
            Whitespace@5..6 " "
            Ident@6..10 "main"
          ParamList@10..12
            LParen@10..11 "("
            RParen@11..12 ")"
          Block@12..77
            Whitespace@12..13 " "
            LBrace@13..14 "{"
            SemiExpr@14..22
              CallExpr@14..21
                PathExpr@14..19
                  Path@14..19
                    Name@14..19
                      Whitespace@14..18 "\n\t\t\t"
                      Ident@18..19 "x"
                ArgList@19..21
                  LParen@19..20 "("
                  RParen@20..21 ")"
              Semi@21..22 ";"
            SemiExpr@22..32
              IndexExpr@22..31
                PathExpr@22..27
                  Path@22..27
                    Name@22..27
                      Whitespace@22..26 "\n\t\t\t"
                      Ident@26..27 "x"
                LBracket@27..28 "["
                IntLit@28..30 "10"
                RBracket@30..31 "]"
              Semi@31..32 ";"
            SemiExpr@32..40
              PathExpr@32..39
                Path@32..39
                  PathSegment@32..38
                    Name@32..37
                      Whitespace@32..36 "\n\t\t\t"
                      Ident@36..37 "y"
                    Dot@37..38 "."
                  Name@38..39
                    Ident@38..39 "a"
              Semi@39..40 ";"
            SemiExpr@40..59
              CallExpr@40..58
                FieldExpr@40..56
                  IndexExpr@40..54
                    CallExpr@40..51
                      PathExpr@40..49
                        Path@40..49
                          Name@40..49
                            Whitespace@40..44 "\n\t\t\t"
                            Ident@44..49 "hello"
                      ArgList@49..51
                        LParen@49..50 "("
                        RParen@50..51 ")"
                    LBracket@51..52 "["
                    IntLit@52..53 "0"
                    RBracket@53..54 "]"
                  Dot@54..55 "."
                  Name@55..56
                    Ident@55..56 "x"
                ArgList@56..58
                  LParen@56..57 "("
                  RParen@57..58 ")"
              Semi@58..59 ";"
            SemiExpr@59..73
              CastExpr@59..72
                Whitespace@59..63 "\n\t\t\t"
                IntLit@63..65 "10"
                Whitespace@65..66 " "
                AsKw@66..68 "as"
                PathType@68..72
                  Path@68..72
                    Name@68..72
                      Whitespace@68..69 " "
                      Ident@69..72 "f32"
              Semi@72..73 ";"
            Whitespace@73..76 "\n\t\t"
            RBrace@76..77 "}"
      Whitespace@77..79 "\n\t""#]]
	};

	let diags = expect![""];

	harness(source, ast, diags);
}

#[test]
fn match_() {
	let source = r#"
		fn main() {
			match my_enum {
				.X => "X",
				.Y => "Y",
			}
		}
	"#;

	let ast = {
		expect![[r#"
    File@0..74
      Item@0..72
        Fn@0..72
          Whitespace@0..3 "\n\t\t"
          FnKw@3..5 "fn"
          Name@5..10
            Whitespace@5..6 " "
            Ident@6..10 "main"
          ParamList@10..12
            LParen@10..11 "("
            RParen@11..12 ")"
          Block@12..72
            Whitespace@12..13 " "
            LBrace@13..14 "{"
            MatchExpr@14..68
              Whitespace@14..18 "\n\t\t\t"
              MatchKw@18..23 "match"
              PathExpr@23..31
                Path@23..31
                  Name@23..31
                    Whitespace@23..24 " "
                    Ident@24..31 "my_enum"
              Whitespace@31..32 " "
              LBrace@32..33 "{"
              MatchArm@33..47
                PathExpr@33..40
                  Path@33..40
                    Whitespace@33..38 "\n\t\t\t\t"
                    Dot@38..39 "."
                    Name@39..40
                      Ident@39..40 "X"
                Whitespace@40..41 " "
                FatArrow@41..43 "=>"
                Whitespace@43..44 " "
                StringLit@44..47 "\"X\""
              Comma@47..48 ","
              MatchArm@48..62
                PathExpr@48..55
                  Path@48..55
                    Whitespace@48..53 "\n\t\t\t\t"
                    Dot@53..54 "."
                    Name@54..55
                      Ident@54..55 "Y"
                Whitespace@55..56 " "
                FatArrow@56..58 "=>"
                Whitespace@58..59 " "
                StringLit@59..62 "\"Y\""
              Comma@62..63 ","
              Whitespace@63..67 "\n\t\t\t"
              RBrace@67..68 "}"
            Whitespace@68..71 "\n\t\t"
            RBrace@71..72 "}"
      Whitespace@72..74 "\n\t""#]]
	};

	let diags = expect![""];

	harness(source, ast, diags);
}

fn harness(source: &str, ast: Expect, diagnostics: Expect) {
	let mut ctx = TreeBuilderContext::new();
	let (builder, diags) = Parser::new(source, &mut ctx).parse();
	let node = builder.finish();

	let resolved = ResolvedNode::new_root_with_resolver(node, text::get_interner());

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
