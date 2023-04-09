use expect_test::expect;

use super::harness;

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
      Whitespace@0..3 "\n\t\t"
      Item@3..57
        Attribute@3..16
          At@3..4 "@"
          Name@4..8
            Ident@4..8 "lang"
          TokenTree@8..13
            LParen@8..9 "("
            Ident@9..12 "u16"
            RParen@12..13 ")"
          Whitespace@13..16 "\n\t\t"
        Attribute@16..34
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
          Whitespace@31..34 "\n\t\t"
        Attribute@34..44
          At@34..35 "@"
          Name@35..41
            Ident@35..41 "inline"
          Whitespace@41..44 "\n\t\t"
        Struct@44..55
          StructKw@44..50 "struct"
          Whitespace@50..51 " "
          Name@51..52
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
		type F = fn(T) -> T;
	"#;

	let ast = {
		expect![[r#"
    File@0..99
      Whitespace@0..3 "\n\t\t"
      Item@3..28
        TypeAlias@3..25
          TypeKw@3..7 "type"
          Whitespace@7..8 " "
          Name@8..9
            Ident@8..9 "Y"
          Whitespace@9..10 " "
          Eq@10..11 "="
          Whitespace@11..12 " "
          PathType@12..24
            Path@12..24
              Path@12..21
                Path@12..20
                  Path@12..17
                    Path@12..16
                      Ident@12..16 "core"
                    Dot@16..17 "."
                  Ident@17..20 "vec"
                Dot@20..21 "."
              Ident@21..24 "Vec"
          Semi@24..25 ";"
        Whitespace@25..28 "\n\t\t"
      Item@28..43
        TypeAlias@28..40
          TypeKw@28..32 "type"
          Whitespace@32..33 " "
          Name@33..34
            Ident@33..34 "X"
          Whitespace@34..35 " "
          Eq@35..36 "="
          Whitespace@36..37 " "
          PtrType@37..39
            Star@37..38 "*"
            PathType@38..39
              Path@38..39
                Ident@38..39 "T"
          Semi@39..40 ";"
        Whitespace@40..43 "\n\t\t"
      Item@43..63
        TypeAlias@43..60
          TypeKw@43..47 "type"
          Whitespace@47..48 " "
          Name@48..49
            Ident@48..49 "Z"
          Whitespace@49..50 " "
          Eq@50..51 "="
          Whitespace@51..52 " "
          ArrayType@52..59
            LBracket@52..53 "["
            PathType@53..54
              Path@53..54
                Ident@53..54 "T"
            Semi@54..55 ";"
            Whitespace@55..56 " "
            IntLit@56..58 "10"
            RBracket@58..59 "]"
          Semi@59..60 ";"
        Whitespace@60..63 "\n\t\t"
      Item@63..77
        TypeAlias@63..74
          TypeKw@63..67 "type"
          Whitespace@67..68 " "
          Name@68..69
            Ident@68..69 "A"
          Whitespace@69..70 " "
          Eq@70..71 "="
          Whitespace@71..72 " "
          InferType@72..73
            Underscore@72..73 "_"
          Semi@73..74 ";"
        Whitespace@74..77 "\n\t\t"
      Item@77..99
        TypeAlias@77..97
          TypeKw@77..81 "type"
          Whitespace@81..82 " "
          Name@82..83
            Ident@82..83 "F"
          Whitespace@83..84 " "
          Eq@84..85 "="
          Whitespace@85..86 " "
          FnType@86..96
            FnKw@86..88 "fn"
            TyParamList@88..91
              LParen@88..89 "("
              PathType@89..90
                Path@89..90
                  Ident@89..90 "T"
              RParen@90..91 ")"
            Whitespace@91..92 " "
            RetTy@92..96
              Arrow@92..94 "->"
              Whitespace@94..95 " "
              PathType@95..96
                Path@95..96
                  Ident@95..96 "T"
          Semi@96..97 ";"
        Whitespace@97..99 "\n\t""#]]
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
      Whitespace@0..3 "\n\t\t"
      Item@3..29
        Struct@3..27
          StructKw@3..9 "struct"
          Whitespace@9..10 " "
          Name@10..13
            Ident@10..13 "Sus"
          Whitespace@13..14 " "
          LBrace@14..15 "{"
          Whitespace@15..16 " "
          Param@16..24
            Name@16..21
              Ident@16..21 "inner"
            Colon@21..22 ":"
            Whitespace@22..23 " "
            PathType@23..24
              Path@23..24
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
      Whitespace@0..3 "\n\t\t"
      Item@3..27
        Fn@3..27
          FnKw@3..5 "fn"
          Whitespace@5..6 " "
          Name@6..15
            Ident@6..15 "something"
          ParamList@15..21
            LParen@15..16 "("
            Param@16..20
              Name@16..17
                Ident@16..17 "p"
              Colon@17..18 ":"
              Whitespace@18..19 " "
              PathType@19..20
                Path@19..20
                  Ident@19..20 "T"
            RParen@20..21 ")"
          Whitespace@21..22 " "
          Block@22..24
            LBrace@22..23 "{"
            RBrace@23..24 "}"
          Whitespace@24..27 "\n\t\t"
      Item@27..77
        Fn@27..77
          FnKw@27..29 "fn"
          Whitespace@29..30 " "
          Name@30..37
            Ident@30..37 "returns"
          ParamList@37..61
            LParen@37..38 "("
            Param@38..44
              Name@38..39
                Ident@38..39 "x"
              Colon@39..40 ":"
              Whitespace@40..41 " "
              PathType@41..44
                Path@41..44
                  Ident@41..44 "i32"
            Comma@44..45 ","
            Whitespace@45..46 " "
            Param@46..52
              Name@46..47
                Ident@46..47 "y"
              Colon@47..48 ":"
              Whitespace@48..49 " "
              PathType@49..52
                Path@49..52
                  Ident@49..52 "i32"
            Comma@52..53 ","
            Whitespace@53..54 " "
            Param@54..60
              Name@54..55
                Ident@54..55 "z"
              Colon@55..56 ":"
              Whitespace@56..57 " "
              PathType@57..60
                Path@57..60
                  Ident@57..60 "i32"
            RParen@60..61 ")"
          Whitespace@61..62 " "
          RetTy@62..69
            Arrow@62..64 "->"
            Whitespace@64..65 " "
            PathType@65..69
              Path@65..68
                Ident@65..68 "i32"
              Whitespace@68..69 " "
          Block@69..75
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
fn import() {
	let source = r#"
		import hello;
		import hello.hello;
		import .;
		import .hello;
		import hello.{
			hello,
			hello.{
				hello,
				hello.hello,
			}
		};
	"#;

	let ast = {
		expect![[r#"
    File@0..145
      Whitespace@0..3 "\n\t\t"
      Item@3..19
        Import@3..16
          ImportKw@3..9 "import"
          RenameImport@9..15
            Path@9..15
              Whitespace@9..10 " "
              Ident@10..15 "hello"
          Semi@15..16 ";"
        Whitespace@16..19 "\n\t\t"
      Item@19..41
        Import@19..38
          ImportKw@19..25 "import"
          RenameImport@25..37
            Path@25..37
              Path@25..32
                Path@25..31
                  Whitespace@25..26 " "
                  Ident@26..31 "hello"
                Dot@31..32 "."
              Ident@32..37 "hello"
          Semi@37..38 ";"
        Whitespace@38..41 "\n\t\t"
      Item@41..53
        Import@41..50
          ImportKw@41..47 "import"
          RenameImport@47..49
            Path@47..49
              Whitespace@47..48 " "
              Dot@48..49 "."
          Semi@49..50 ";"
        Whitespace@50..53 "\n\t\t"
      Item@53..70
        Import@53..67
          ImportKw@53..59 "import"
          RenameImport@59..66
            Path@59..66
              Path@59..61
                Whitespace@59..60 " "
                Dot@60..61 "."
              Ident@61..66 "hello"
          Semi@66..67 ";"
        Whitespace@67..70 "\n\t\t"
      Item@70..145
        Import@70..143
          ImportKw@70..76 "import"
          ListImport@76..142
            Path@76..82
              Whitespace@76..77 " "
              Ident@77..82 "hello"
            Dot@82..83 "."
            ImportTreeList@83..142
              LBrace@83..84 "{"
              RenameImport@84..93
                Path@84..93
                  Whitespace@84..88 "\n\t\t\t"
                  Ident@88..93 "hello"
              Comma@93..94 ","
              ListImport@94..141
                Path@94..103
                  Whitespace@94..98 "\n\t\t\t"
                  Ident@98..103 "hello"
                Dot@103..104 "."
                ImportTreeList@104..138
                  LBrace@104..105 "{"
                  RenameImport@105..115
                    Path@105..115
                      Whitespace@105..110 "\n\t\t\t\t"
                      Ident@110..115 "hello"
                  Comma@115..116 ","
                  RenameImport@116..132
                    Path@116..132
                      Path@116..127
                        Path@116..126
                          Whitespace@116..121 "\n\t\t\t\t"
                          Ident@121..126 "hello"
                        Dot@126..127 "."
                      Ident@127..132 "hello"
                  Comma@132..133 ","
                  Whitespace@133..137 "\n\t\t\t"
                  RBrace@137..138 "}"
                Whitespace@138..141 "\n\t\t"
              RBrace@141..142 "}"
          Semi@142..143 ";"
        Whitespace@143..145 "\n\t""#]]
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
      Whitespace@0..3 "\n\t\t"
      Item@3..260
        Fn@3..260
          FnKw@3..5 "fn"
          Whitespace@5..6 " "
          Name@6..10
            Ident@6..10 "main"
          ParamList@10..12
            LParen@10..11 "("
            RParen@11..12 ")"
          Whitespace@12..13 " "
          Block@13..258
            LBrace@13..14 "{"
            SemiExpr@14..22
              Whitespace@14..18 "\n\t\t\t"
              ParenExpr@18..21
                LParen@18..19 "("
                PathExpr@19..20
                  Path@19..20
                    Ident@19..20 "x"
                RParen@20..21 ")"
              Semi@21..22 ";"
            Whitespace@22..26 "\n\t\t\t"
            SemiExpr@26..29
              IntLit@26..28 "34"
              Semi@28..29 ";"
            Whitespace@29..30 " "
            SemiExpr@30..38
              StringLit@30..37 "\"hello\""
              Semi@37..38 ";"
            Whitespace@38..39 " "
            SemiExpr@39..43
              FloatLit@39..42 "4.5"
              Semi@42..43 ";"
            Whitespace@43..44 " "
            SemiExpr@44..48
              CharLit@44..47 "'c'"
              Semi@47..48 ";"
            Whitespace@48..49 " "
            SemiExpr@49..54
              BoolLit@49..53 "true"
              Semi@53..54 ";"
            Whitespace@54..55 " "
            SemiExpr@55..61
              BoolLit@55..60 "false"
              Semi@60..61 ";"
            Whitespace@61..65 "\n\t\t\t"
            SemiExpr@65..71
              BreakExpr@65..70
                BreakKw@65..70 "break"
              Semi@70..71 ";"
            Whitespace@71..75 "\n\t\t\t"
            SemiExpr@75..84
              ContinueKw@75..83 "continue"
              Semi@83..84 ";"
            Whitespace@84..88 "\n\t\t\t"
            SemiExpr@88..98
              ReturnExpr@88..97
                ReturnKw@88..94 "return"
                Whitespace@94..95 " "
                IntLit@95..97 "10"
              Semi@97..98 ";"
            Whitespace@98..102 "\n\t\t\t"
            LoopExpr@102..113
              LoopKw@102..106 "loop"
              Whitespace@106..107 " "
              Block@107..109
                LBrace@107..108 "{"
                RBrace@108..109 "}"
              Whitespace@109..113 "\n\t\t\t"
            SemiExpr@113..141
              LoopExpr@113..140
                LoopKw@113..117 "loop"
                Whitespace@117..118 " "
                Block@118..128
                  LBrace@118..119 "{"
                  SemiExpr@119..126
                    Whitespace@119..120 " "
                    PathExpr@120..125
                      Path@120..125
                        Ident@120..125 "hello"
                    Semi@125..126 ";"
                  Whitespace@126..127 " "
                  RBrace@127..128 "}"
                Whitespace@128..129 " "
                WhileKw@129..134 "while"
                Whitespace@134..135 " "
                BoolLit@135..140 "false"
              Semi@140..141 ";"
            Whitespace@141..145 "\n\t\t\t"
            WhileExpr@145..170
              WhileKw@145..150 "while"
              Whitespace@150..151 " "
              BoolLit@151..155 "true"
              Whitespace@155..156 " "
              Block@156..166
                LBrace@156..157 "{"
                SemiExpr@157..164
                  Whitespace@157..158 " "
                  PathExpr@158..163
                    Path@158..163
                      Ident@158..163 "hello"
                  Semi@163..164 ";"
                Whitespace@164..165 " "
                RBrace@165..166 "}"
              Whitespace@166..170 "\n\t\t\t"
            ForExpr@170..195
              ForKw@170..173 "for"
              Whitespace@173..174 " "
              Name@174..175
                Ident@174..175 "x"
              Whitespace@175..176 " "
              InKw@176..178 "in"
              Whitespace@178..179 " "
              PathExpr@179..181
                Path@179..180
                  Ident@179..180 "y"
                Whitespace@180..181 " "
              Block@181..191
                LBrace@181..182 "{"
                SemiExpr@182..189
                  Whitespace@182..183 " "
                  PathExpr@183..188
                    Path@183..188
                      Ident@183..188 "hello"
                  Semi@188..189 ";"
                Whitespace@189..190 " "
                RBrace@190..191 "}"
              Whitespace@191..195 "\n\t\t\t"
            IfExpr@195..257
              IfKw@195..197 "if"
              Whitespace@197..198 " "
              BoolLit@198..202 "true"
              Whitespace@202..203 " "
              Block@203..213
                LBrace@203..204 "{"
                SemiExpr@204..211
                  Whitespace@204..205 " "
                  PathExpr@205..210
                    Path@205..210
                      Ident@205..210 "hello"
                  Semi@210..211 ";"
                Whitespace@211..212 " "
                RBrace@212..213 "}"
              Whitespace@213..214 " "
              ElseKw@214..218 "else"
              Whitespace@218..219 " "
              IfExpr@219..257
                IfKw@219..221 "if"
                Whitespace@221..222 " "
                BoolLit@222..227 "false"
                Whitespace@227..228 " "
                Block@228..238
                  LBrace@228..229 "{"
                  SemiExpr@229..236
                    Whitespace@229..230 " "
                    PathExpr@230..235
                      Path@230..235
                        Ident@230..235 "hello"
                    Semi@235..236 ";"
                  Whitespace@236..237 " "
                  RBrace@237..238 "}"
                Whitespace@238..239 " "
                ElseKw@239..243 "else"
                Whitespace@243..244 " "
                Block@244..254
                  LBrace@244..245 "{"
                  SemiExpr@245..252
                    Whitespace@245..246 " "
                    PathExpr@246..251
                      Path@246..251
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
      Whitespace@0..3 "\n\t\t"
      Item@3..87
        Fn@3..87
          FnKw@3..5 "fn"
          Whitespace@5..6 " "
          Name@6..10
            Ident@6..10 "main"
          ParamList@10..12
            LParen@10..11 "("
            RParen@11..12 ")"
          Whitespace@12..13 " "
          Block@13..85
            LBrace@13..14 "{"
            SemiExpr@14..22
              PrefixExpr@14..21
                Whitespace@14..18 "\n\t\t\t"
                Minus@18..19 "-"
                IntLit@19..21 "10"
              Semi@21..22 ";"
            Whitespace@22..26 "\n\t\t\t"
            SemiExpr@26..32
              PrefixExpr@26..31
                Not@26..27 "!"
                BoolLit@27..31 "true"
              Semi@31..32 ";"
            Whitespace@32..36 "\n\t\t\t"
            SemiExpr@36..41
              PrefixExpr@36..40
                Star@36..37 "*"
                PathExpr@37..40
                  Path@37..40
                    Ident@37..40 "ptr"
              Semi@40..41 ";"
            Whitespace@41..45 "\n\t\t\t"
            SemiExpr@45..50
              RefExpr@45..49
                Amp@45..46 "&"
                PathExpr@46..49
                  Path@46..49
                    Ident@46..49 "ptr"
              Semi@49..50 ";"
            Whitespace@50..54 "\n\t\t\t"
            SemiExpr@54..63
              RefExpr@54..62
                Amp@54..55 "&"
                MutKw@55..58 "mut"
                Whitespace@58..59 " "
                PathExpr@59..62
                  Path@59..62
                    Ident@59..62 "ptr"
              Semi@62..63 ";"
            Whitespace@63..67 "\n\t\t\t"
            SemiExpr@67..81
              PrefixExpr@67..80
                Minus@67..68 "-"
                PrefixExpr@68..80
                  Minus@68..69 "-"
                  PrefixExpr@69..80
                    Not@69..70 "!"
                    RefExpr@70..80
                      Amp@70..71 "&"
                      PathExpr@71..80
                        Path@71..80
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
      Whitespace@0..3 "\n\t\t"
      Item@3..95
        Fn@3..95
          FnKw@3..5 "fn"
          Whitespace@5..6 " "
          Name@6..10
            Ident@6..10 "main"
          ParamList@10..12
            LParen@10..11 "("
            RParen@11..12 ")"
          Whitespace@12..13 " "
          Block@13..93
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
            Whitespace@26..30 "\n\t\t\t"
            SemiExpr@30..40
              InfixExpr@30..39
                IntLit@30..32 "10"
                Whitespace@32..33 " "
                Plus@33..34 "+"
                PrefixExpr@34..39
                  Whitespace@34..35 " "
                  Minus@35..36 "-"
                  PrefixExpr@36..39
                    Minus@36..37 "-"
                    IntLit@37..39 "20"
              Semi@39..40 ";"
            Whitespace@40..44 "\n\t\t\t"
            SemiExpr@44..55
              InfixExpr@44..54
                IntLit@44..46 "10"
                Whitespace@46..47 " "
                Plus@47..48 "+"
                InfixExpr@48..54
                  Whitespace@48..49 " "
                  IntLit@49..50 "4"
                  Whitespace@50..51 " "
                  Star@51..52 "*"
                  Whitespace@52..53 " "
                  IntLit@53..54 "5"
              Semi@54..55 ";"
            Whitespace@55..59 "\n\t\t\t"
            SemiExpr@59..71
              InfixExpr@59..70
                IntLit@59..60 "5"
                Whitespace@60..61 " "
                AmpAmp@61..63 "&&"
                InfixExpr@63..70
                  Whitespace@63..64 " "
                  IntLit@64..65 "6"
                  Whitespace@65..66 " "
                  EqEq@66..68 "=="
                  Whitespace@68..69 " "
                  IntLit@69..70 "3"
              Semi@70..71 ";"
            Whitespace@71..75 "\n\t\t\t"
            SemiExpr@75..89
              InfixExpr@75..88
                PathExpr@75..77
                  Path@75..76
                    Ident@75..76 "a"
                  Whitespace@76..77 " "
                Eq@77..78 "="
                InfixExpr@78..88
                  Whitespace@78..79 " "
                  PathExpr@79..81
                    Path@79..80
                      Ident@79..80 "b"
                    Whitespace@80..81 " "
                  Eq@81..82 "="
                  InfixExpr@82..88
                    Whitespace@82..83 " "
                    PathExpr@83..85
                      Path@83..84
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
      Whitespace@0..3 "\n\t\t"
      Item@3..79
        Fn@3..79
          FnKw@3..5 "fn"
          Whitespace@5..6 " "
          Name@6..10
            Ident@6..10 "main"
          ParamList@10..12
            LParen@10..11 "("
            RParen@11..12 ")"
          Whitespace@12..13 " "
          Block@13..77
            LBrace@13..14 "{"
            SemiExpr@14..22
              CallExpr@14..21
                Whitespace@14..18 "\n\t\t\t"
                PathExpr@18..19
                  Path@18..19
                    Ident@18..19 "x"
                ArgList@19..21
                  LParen@19..20 "("
                  RParen@20..21 ")"
              Semi@21..22 ";"
            Whitespace@22..26 "\n\t\t\t"
            SemiExpr@26..32
              IndexExpr@26..31
                PathExpr@26..27
                  Path@26..27
                    Ident@26..27 "x"
                LBracket@27..28 "["
                IntLit@28..30 "10"
                RBracket@30..31 "]"
              Semi@31..32 ";"
            Whitespace@32..36 "\n\t\t\t"
            SemiExpr@36..40
              PathExpr@36..39
                Path@36..39
                  Path@36..38
                    Path@36..37
                      Ident@36..37 "y"
                    Dot@37..38 "."
                  Ident@38..39 "a"
              Semi@39..40 ";"
            Whitespace@40..44 "\n\t\t\t"
            SemiExpr@44..59
              CallExpr@44..58
                FieldExpr@44..56
                  IndexExpr@44..54
                    CallExpr@44..51
                      PathExpr@44..49
                        Path@44..49
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
            Whitespace@59..63 "\n\t\t\t"
            SemiExpr@63..73
              CastExpr@63..72
                IntLit@63..65 "10"
                Whitespace@65..66 " "
                AsKw@66..68 "as"
                Whitespace@68..69 " "
                PathType@69..72
                  Path@69..72
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
      Whitespace@0..3 "\n\t\t"
      Item@3..74
        Fn@3..74
          FnKw@3..5 "fn"
          Whitespace@5..6 " "
          Name@6..10
            Ident@6..10 "main"
          ParamList@10..12
            LParen@10..11 "("
            RParen@11..12 ")"
          Whitespace@12..13 " "
          Block@13..72
            LBrace@13..14 "{"
            Whitespace@14..18 "\n\t\t\t"
            MatchExpr@18..68
              MatchKw@18..23 "match"
              Whitespace@23..24 " "
              PathExpr@24..32
                Path@24..31
                  Ident@24..31 "my_enum"
                Whitespace@31..32 " "
              LBrace@32..33 "{"
              Whitespace@33..38 "\n\t\t\t\t"
              MatchArm@38..47
                PathExpr@38..41
                  Path@38..40
                    Path@38..39
                      Dot@38..39 "."
                    Ident@39..40 "X"
                  Whitespace@40..41 " "
                FatArrow@41..43 "=>"
                Whitespace@43..44 " "
                StringLit@44..47 "\"X\""
              Comma@47..48 ","
              Whitespace@48..53 "\n\t\t\t\t"
              MatchArm@53..62
                PathExpr@53..56
                  Path@53..55
                    Path@53..54
                      Dot@53..54 "."
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
