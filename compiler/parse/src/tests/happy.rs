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
              Name@12..16
                Ident@12..16 "core"
              Dot@16..17 "."
              Name@17..20
                Ident@17..20 "vec"
              Dot@20..21 "."
              Name@21..24
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
                Name@38..39
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
                Name@53..54
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
                  Name@89..90
                    Ident@89..90 "T"
              RParen@90..91 ")"
            Whitespace@91..92 " "
            RetTy@92..96
              Arrow@92..94 "->"
              Whitespace@94..95 " "
              PathType@95..96
                Path@95..96
                  Name@95..96
                    Ident@95..96 "T"
          Semi@96..97 ";"
        Whitespace@97..99 "\n\t""#]]
	};

	let diags = expect![""];

	harness(source, ast, diags);
}

#[test]
fn items() {
	let source = r#"
		mod x;
		mod sus;

		struct Sus { inner: T, }

		enum X { A, B, C, }

		fn something(p: T) {}
		fn returns(x: i32, y: i32, z: i32) -> i32 { 10 }
	"#;

	let ast = {
		expect![[r#"
    File@0..149
      Whitespace@0..3 "\n\t\t"
      Item@3..12
        Error@3..9
          Ident@3..6 "mod"
          Whitespace@6..7 " "
          Ident@7..8 "x"
          Semi@8..9 ";"
        Whitespace@9..12 "\n\t\t"
      Item@12..24
        Error@12..20
          Ident@12..15 "mod"
          Whitespace@15..16 " "
          Ident@16..19 "sus"
          Semi@19..20 ";"
        Whitespace@20..24 "\n\n\t\t"
      Item@24..52
        Struct@24..48
          StructKw@24..30 "struct"
          Whitespace@30..31 " "
          Name@31..34
            Ident@31..34 "Sus"
          Whitespace@34..35 " "
          LBrace@35..36 "{"
          Whitespace@36..37 " "
          Param@37..45
            Name@37..42
              Ident@37..42 "inner"
            Colon@42..43 ":"
            Whitespace@43..44 " "
            PathType@44..45
              Path@44..45
                Name@44..45
                  Ident@44..45 "T"
          Comma@45..46 ","
          Whitespace@46..47 " "
          RBrace@47..48 "}"
        Whitespace@48..52 "\n\n\t\t"
      Item@52..75
        Enum@52..75
          EnumKw@52..56 "enum"
          Whitespace@56..57 " "
          Name@57..58
            Ident@57..58 "X"
          Whitespace@58..59 " "
          VariantList@59..71
            LBrace@59..60 "{"
            Whitespace@60..61 " "
            Name@61..62
              Ident@61..62 "A"
            Comma@62..63 ","
            Whitespace@63..64 " "
            Name@64..65
              Ident@64..65 "B"
            Comma@65..66 ","
            Whitespace@66..67 " "
            Name@67..68
              Ident@67..68 "C"
            Comma@68..69 ","
            Whitespace@69..70 " "
            RBrace@70..71 "}"
          Whitespace@71..75 "\n\n\t\t"
      Item@75..99
        Fn@75..99
          FnKw@75..77 "fn"
          Whitespace@77..78 " "
          Name@78..87
            Ident@78..87 "something"
          ParamList@87..93
            LParen@87..88 "("
            Param@88..92
              Name@88..89
                Ident@88..89 "p"
              Colon@89..90 ":"
              Whitespace@90..91 " "
              PathType@91..92
                Path@91..92
                  Name@91..92
                    Ident@91..92 "T"
            RParen@92..93 ")"
          Whitespace@93..94 " "
          Block@94..96
            LBrace@94..95 "{"
            RBrace@95..96 "}"
          Whitespace@96..99 "\n\t\t"
      Item@99..149
        Fn@99..149
          FnKw@99..101 "fn"
          Whitespace@101..102 " "
          Name@102..109
            Ident@102..109 "returns"
          ParamList@109..133
            LParen@109..110 "("
            Param@110..116
              Name@110..111
                Ident@110..111 "x"
              Colon@111..112 ":"
              Whitespace@112..113 " "
              PathType@113..116
                Path@113..116
                  Name@113..116
                    Ident@113..116 "i32"
            Comma@116..117 ","
            Whitespace@117..118 " "
            Param@118..124
              Name@118..119
                Ident@118..119 "y"
              Colon@119..120 ":"
              Whitespace@120..121 " "
              PathType@121..124
                Path@121..124
                  Name@121..124
                    Ident@121..124 "i32"
            Comma@124..125 ","
            Whitespace@125..126 " "
            Param@126..132
              Name@126..127
                Ident@126..127 "z"
              Colon@127..128 ":"
              Whitespace@128..129 " "
              PathType@129..132
                Path@129..132
                  Name@129..132
                    Ident@129..132 "i32"
            RParen@132..133 ")"
          Whitespace@133..134 " "
          RetTy@134..141
            Arrow@134..136 "->"
            Whitespace@136..137 " "
            PathType@137..141
              Path@137..141
                Name@137..140
                  Ident@137..140 "i32"
                Whitespace@140..141 " "
          Block@141..147
            LBrace@141..142 "{"
            Whitespace@142..143 " "
            IntLit@143..145 "10"
            Whitespace@145..146 " "
            RBrace@146..147 "}"
          Whitespace@147..149 "\n\t""#]]
	};

	let diags = expect![[r#"
    Error: expected item
       ,-[<unknown>:2:3]
       |
     2 |        mod x;
       |        ^|^  
       |         `--- found identifier
    ---'
    Error: expected item
       ,-[<unknown>:3:3]
       |
     3 |        mod sus;
       |        ^|^  
       |         `--- found identifier
    ---'
"#]];

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
                  Name@19..20
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
                  Name@41..44
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
                  Name@49..52
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
                  Name@57..60
                    Ident@57..60 "i32"
            RParen@60..61 ")"
          Whitespace@61..62 " "
          RetTy@62..69
            Arrow@62..64 "->"
            Whitespace@64..65 " "
            PathType@65..69
              Path@65..69
                Name@65..68
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
            Whitespace@9..10 " "
            Path@10..15
              Name@10..15
                Ident@10..15 "hello"
          Semi@15..16 ";"
        Whitespace@16..19 "\n\t\t"
      Item@19..41
        Import@19..38
          ImportKw@19..25 "import"
          RenameImport@25..37
            Whitespace@25..26 " "
            Path@26..37
              Name@26..31
                Ident@26..31 "hello"
              Dot@31..32 "."
              Name@32..37
                Ident@32..37 "hello"
          Semi@37..38 ";"
        Whitespace@38..41 "\n\t\t"
      Item@41..53
        Import@41..50
          ImportKw@41..47 "import"
          RenameImport@47..49
            Whitespace@47..48 " "
            Path@48..49
              Dot@48..49 "."
          Semi@49..50 ";"
        Whitespace@50..53 "\n\t\t"
      Item@53..70
        Import@53..67
          ImportKw@53..59 "import"
          RenameImport@59..66
            Whitespace@59..60 " "
            Path@60..66
              Dot@60..61 "."
              Name@61..66
                Ident@61..66 "hello"
          Semi@66..67 ";"
        Whitespace@67..70 "\n\t\t"
      Item@70..145
        Import@70..143
          ImportKw@70..76 "import"
          ListImport@76..142
            Whitespace@76..77 " "
            Path@77..82
              Name@77..82
                Ident@77..82 "hello"
            Dot@82..83 "."
            ImportTreeList@83..142
              LBrace@83..84 "{"
              RenameImport@84..93
                Whitespace@84..88 "\n\t\t\t"
                Path@88..93
                  Name@88..93
                    Ident@88..93 "hello"
              Comma@93..94 ","
              ListImport@94..141
                Whitespace@94..98 "\n\t\t\t"
                Path@98..103
                  Name@98..103
                    Ident@98..103 "hello"
                Dot@103..104 "."
                ImportTreeList@104..138
                  LBrace@104..105 "{"
                  RenameImport@105..115
                    Whitespace@105..110 "\n\t\t\t\t"
                    Path@110..115
                      Name@110..115
                        Ident@110..115 "hello"
                  Comma@115..116 ","
                  RenameImport@116..132
                    Whitespace@116..121 "\n\t\t\t\t"
                    Path@121..132
                      Name@121..126
                        Ident@121..126 "hello"
                      Dot@126..127 "."
                      Name@127..132
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
			[];
			[10];
			[10, 20, 30];
			[10; 30];
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
    File@0..306
      Whitespace@0..3 "\n\t\t"
      Item@3..306
        Fn@3..306
          FnKw@3..5 "fn"
          Whitespace@5..6 " "
          Name@6..10
            Ident@6..10 "main"
          ParamList@10..12
            LParen@10..11 "("
            RParen@11..12 ")"
          Whitespace@12..13 " "
          Block@13..304
            LBrace@13..14 "{"
            SemiExpr@14..22
              Whitespace@14..18 "\n\t\t\t"
              ParenExpr@18..21
                LParen@18..19 "("
                NameExpr@19..20
                  Name@19..20
                    Ident@19..20 "x"
                RParen@20..21 ")"
              Semi@21..22 ";"
            Whitespace@22..26 "\n\t\t\t"
            SemiExpr@26..29
              ArrayExpr@26..28
                LBracket@26..27 "["
                ArrayList@27..27
                RBracket@27..28 "]"
              Semi@28..29 ";"
            Whitespace@29..33 "\n\t\t\t"
            SemiExpr@33..38
              ArrayExpr@33..37
                LBracket@33..34 "["
                ArrayList@34..36
                  IntLit@34..36 "10"
                RBracket@36..37 "]"
              Semi@37..38 ";"
            Whitespace@38..42 "\n\t\t\t"
            SemiExpr@42..55
              ArrayExpr@42..54
                LBracket@42..43 "["
                ArrayList@43..53
                  IntLit@43..45 "10"
                  Comma@45..46 ","
                  Whitespace@46..47 " "
                  IntLit@47..49 "20"
                  Comma@49..50 ","
                  Whitespace@50..51 " "
                  IntLit@51..53 "30"
                RBracket@53..54 "]"
              Semi@54..55 ";"
            Whitespace@55..59 "\n\t\t\t"
            SemiExpr@59..68
              ArrayExpr@59..67
                LBracket@59..60 "["
                ArrayRepeat@60..66
                  IntLit@60..62 "10"
                  Semi@62..63 ";"
                  Whitespace@63..64 " "
                  IntLit@64..66 "30"
                RBracket@66..67 "]"
              Semi@67..68 ";"
            Whitespace@68..72 "\n\t\t\t"
            SemiExpr@72..75
              IntLit@72..74 "34"
              Semi@74..75 ";"
            Whitespace@75..76 " "
            SemiExpr@76..84
              StringLit@76..83 "\"hello\""
              Semi@83..84 ";"
            Whitespace@84..85 " "
            SemiExpr@85..89
              FloatLit@85..88 "4.5"
              Semi@88..89 ";"
            Whitespace@89..90 " "
            SemiExpr@90..94
              CharLit@90..93 "'c'"
              Semi@93..94 ";"
            Whitespace@94..95 " "
            SemiExpr@95..100
              BoolLit@95..99 "true"
              Semi@99..100 ";"
            Whitespace@100..101 " "
            SemiExpr@101..107
              BoolLit@101..106 "false"
              Semi@106..107 ";"
            Whitespace@107..111 "\n\t\t\t"
            SemiExpr@111..117
              BreakExpr@111..116
                BreakKw@111..116 "break"
              Semi@116..117 ";"
            Whitespace@117..121 "\n\t\t\t"
            SemiExpr@121..130
              ContinueKw@121..129 "continue"
              Semi@129..130 ";"
            Whitespace@130..134 "\n\t\t\t"
            SemiExpr@134..144
              ReturnExpr@134..143
                ReturnKw@134..140 "return"
                Whitespace@140..141 " "
                IntLit@141..143 "10"
              Semi@143..144 ";"
            Whitespace@144..148 "\n\t\t\t"
            LoopExpr@148..159
              LoopKw@148..152 "loop"
              Whitespace@152..153 " "
              Block@153..155
                LBrace@153..154 "{"
                RBrace@154..155 "}"
              Whitespace@155..159 "\n\t\t\t"
            SemiExpr@159..187
              LoopExpr@159..186
                LoopKw@159..163 "loop"
                Whitespace@163..164 " "
                Block@164..174
                  LBrace@164..165 "{"
                  SemiExpr@165..172
                    Whitespace@165..166 " "
                    NameExpr@166..171
                      Name@166..171
                        Ident@166..171 "hello"
                    Semi@171..172 ";"
                  Whitespace@172..173 " "
                  RBrace@173..174 "}"
                Whitespace@174..175 " "
                WhileKw@175..180 "while"
                Whitespace@180..181 " "
                BoolLit@181..186 "false"
              Semi@186..187 ";"
            Whitespace@187..191 "\n\t\t\t"
            WhileExpr@191..216
              WhileKw@191..196 "while"
              Whitespace@196..197 " "
              BoolLit@197..201 "true"
              Whitespace@201..202 " "
              Block@202..212
                LBrace@202..203 "{"
                SemiExpr@203..210
                  Whitespace@203..204 " "
                  NameExpr@204..209
                    Name@204..209
                      Ident@204..209 "hello"
                  Semi@209..210 ";"
                Whitespace@210..211 " "
                RBrace@211..212 "}"
              Whitespace@212..216 "\n\t\t\t"
            ForExpr@216..241
              ForKw@216..219 "for"
              Whitespace@219..220 " "
              Name@220..221
                Ident@220..221 "x"
              Whitespace@221..222 " "
              InKw@222..224 "in"
              Whitespace@224..225 " "
              NameExpr@225..227
                Name@225..226
                  Ident@225..226 "y"
                Whitespace@226..227 " "
              Block@227..237
                LBrace@227..228 "{"
                SemiExpr@228..235
                  Whitespace@228..229 " "
                  NameExpr@229..234
                    Name@229..234
                      Ident@229..234 "hello"
                  Semi@234..235 ";"
                Whitespace@235..236 " "
                RBrace@236..237 "}"
              Whitespace@237..241 "\n\t\t\t"
            IfExpr@241..303
              IfKw@241..243 "if"
              Whitespace@243..244 " "
              BoolLit@244..248 "true"
              Whitespace@248..249 " "
              Block@249..259
                LBrace@249..250 "{"
                SemiExpr@250..257
                  Whitespace@250..251 " "
                  NameExpr@251..256
                    Name@251..256
                      Ident@251..256 "hello"
                  Semi@256..257 ";"
                Whitespace@257..258 " "
                RBrace@258..259 "}"
              Whitespace@259..260 " "
              ElseKw@260..264 "else"
              Whitespace@264..265 " "
              IfExpr@265..303
                IfKw@265..267 "if"
                Whitespace@267..268 " "
                BoolLit@268..273 "false"
                Whitespace@273..274 " "
                Block@274..284
                  LBrace@274..275 "{"
                  SemiExpr@275..282
                    Whitespace@275..276 " "
                    NameExpr@276..281
                      Name@276..281
                        Ident@276..281 "hello"
                    Semi@281..282 ";"
                  Whitespace@282..283 " "
                  RBrace@283..284 "}"
                Whitespace@284..285 " "
                ElseKw@285..289 "else"
                Whitespace@289..290 " "
                Block@290..300
                  LBrace@290..291 "{"
                  SemiExpr@291..298
                    Whitespace@291..292 " "
                    NameExpr@292..297
                      Name@292..297
                        Ident@292..297 "hello"
                    Semi@297..298 ";"
                  Whitespace@298..299 " "
                  RBrace@299..300 "}"
                Whitespace@300..303 "\n\t\t"
            RBrace@303..304 "}"
          Whitespace@304..306 "\n\t""#]]
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
                NameExpr@37..40
                  Name@37..40
                    Ident@37..40 "ptr"
              Semi@40..41 ";"
            Whitespace@41..45 "\n\t\t\t"
            SemiExpr@45..50
              RefExpr@45..49
                Amp@45..46 "&"
                NameExpr@46..49
                  Name@46..49
                    Ident@46..49 "ptr"
              Semi@49..50 ";"
            Whitespace@50..54 "\n\t\t\t"
            SemiExpr@54..63
              RefExpr@54..62
                Amp@54..55 "&"
                MutKw@55..58 "mut"
                Whitespace@58..59 " "
                NameExpr@59..62
                  Name@59..62
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
                      NameExpr@71..80
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
                NameExpr@75..77
                  Name@75..76
                    Ident@75..76 "a"
                  Whitespace@76..77 " "
                Eq@77..78 "="
                InfixExpr@78..88
                  Whitespace@78..79 " "
                  NameExpr@79..81
                    Name@79..80
                      Ident@79..80 "b"
                    Whitespace@80..81 " "
                  Eq@81..82 "="
                  InfixExpr@82..88
                    Whitespace@82..83 " "
                    NameExpr@83..85
                      Name@83..84
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
                NameExpr@18..19
                  Name@18..19
                    Ident@18..19 "x"
                ArgList@19..21
                  LParen@19..20 "("
                  RParen@20..21 ")"
              Semi@21..22 ";"
            Whitespace@22..26 "\n\t\t\t"
            SemiExpr@26..32
              IndexExpr@26..31
                NameExpr@26..27
                  Name@26..27
                    Ident@26..27 "x"
                LBracket@27..28 "["
                IntLit@28..30 "10"
                RBracket@30..31 "]"
              Semi@31..32 ";"
            Whitespace@32..36 "\n\t\t\t"
            SemiExpr@36..40
              FieldExpr@36..39
                NameExpr@36..37
                  Name@36..37
                    Ident@36..37 "y"
                Dot@37..38 "."
                Name@38..39
                  Ident@38..39 "a"
              Semi@39..40 ";"
            Whitespace@40..44 "\n\t\t\t"
            SemiExpr@44..59
              CallExpr@44..58
                FieldExpr@44..56
                  IndexExpr@44..54
                    CallExpr@44..51
                      NameExpr@44..49
                        Name@44..49
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
                    Name@69..72
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
              NameExpr@24..32
                Name@24..31
                  Ident@24..31 "my_enum"
                Whitespace@31..32 " "
              LBrace@32..33 "{"
              Whitespace@33..38 "\n\t\t\t\t"
              MatchArm@38..47
                NameExpr@38..41
                  Dot@38..39 "."
                  Name@39..40
                    Ident@39..40 "X"
                  Whitespace@40..41 " "
                FatArrow@41..43 "=>"
                Whitespace@43..44 " "
                StringLit@44..47 "\"X\""
              Comma@47..48 ","
              Whitespace@48..53 "\n\t\t\t\t"
              MatchArm@53..62
                NameExpr@53..56
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
