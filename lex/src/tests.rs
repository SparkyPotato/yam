use intern::Id;

use crate::{Lexer, T};

#[test]
fn random_input_is_lossless() {
	let random_unicode = r#"
        㴍ꜟ⍽䋎攣搞ν溒ꁁ쾄眀뫢�ꈒ땥楚팡硝䫂喹怟Ѭ韗戵ꥩ㍚八춝㼊䊅桦滗ʧ휕䗒⃦呢❗᱁衽筕㾀홶䇁뛤꺽㡺閖⟃歆鴐숧䜠홁흷ꇪ㓿�⊚쥒䌬ⓠ⧦ĉ疞㋢⠇�剨�籎龟쀎☭샭霐䨛�罥㟨㬳ꀑ쏇揭歙愿�眫玏㯰햱选Ṋ⮡⯛�谐䁦슃Ꞿ㨖겛ᶑᇘ嫬罜횇䭣坚⎖䄆毋쿶ꣃ�趓ሦ㠥짛耯赀�氃ꗡៀ嵄೻ȃ핧꺏㍰憶쮴뽁�갼餟蟥⥌Ǽ䫃㕁㛥쟀窃ꥹ祐燧ﯞ㑸쉶Ზ膾狹笪궸閈轤҄뜜櫀ᓏ⒲㘘砊狥䝾㐱㿍긿᪰鰼ݵ縞졲�꒿琗꟭ᒹ殉骷ꔖ鷐䟘㾗ꗼ깣컜㉤ꗩ鼡稁蚘랤ᇒ띵ꁔఠ럤볻圚箧탬侑탌敮嫋쇩⺩祉满洜핟묕頳᱈䶐&蚃姑鏥�풦菁觇�쪜缲鞣熐㮵⵬ɋ䗍鋿ꐖ껫깡醣괌嚮퀪㵠褶▉龠召ⵊ蟒巾⒓慨爔揼㒪裵㝳閅㲌ƪꂓ⏥⿳㶱狑づ쵑㿶牅ꩈद쉆奴筛鼘徒�敱䱰ဵ꠬䆶⮱嬭돝郖퉕倂謞썟粯᝴䂳蹀㋫Ⲅ㘑茩悧᳥ḙ⏪Ṝ斐嘮铎⑉駃ᒨ悚衦ૂϤ뭖ᄉ쎺࿙$谺쓜௥붭ᒆ溯쉗ꬒᙧ勇危㥸鸠郕謹ᤷ섵ⵁ卿�툋鼾푼熹༦�㚚틨៖푟息疄ᥳ녴꟦鐈הּℂ屺䁚࠾엸臁⣎澺ᶖ�놎녮콱覍義�ꇿ䕸忐཭ю鐝퓪䉩㸾譃섖൞砉₇뀀鬒먚赥疹疓塭ꖯ↓ḃ昸쟆鐊༡᢯烢葿◟⬭ֈ쯚簮옞间䳰츊໷莉໾ᇧ杛屁簠億ᾣ㠖椏ߡٞ遰䷴ா긍᪉賽햾옻ﮍ㳏椓ᬘ쳇⤜믧麶ꞃ聾璓巕쯺膸以솔〡筯ѧ敡渴㛼ྜ�䃍吀竑愬嶞旻嫓䎱髺襔䯊땾ꇿ�㚒ퟣ涝胺⇄窆爾灀興寛合촐腩グ⑇鲦◚�ਜ਼婤婫粺ᶁ叄嗆哌쳺䘟瑡湰欬闋䂮噝媺跓黽ꩴ㤢䟡䯷늝⵸Ց酦妆쮹赓䤓௒횿꺐忩긦硶⤫峐輁禣✓᭝阒湵쁉㵪䡣⬳펍嬉筓嶴㮦⤪⧽擀㞀䒢⳿鼋⾫몥ૹ䭒硞帬㸪㫵孜딉Აࡓ鼒辧啺꫊绻砼芚탻뼳젥ޜນ㚗纪竵⨹꼀鴊⁠葖⮣尔બ밅駲൮ѷ⵱郗䛍憞띁鏉譻徱ꆧ䙈轭攓梗ᬇ淺쨵私穢嗯퀤꘭ᇠ慱ხ䠒镫咩ฑ풫㔈Əӷ㋅�馃뫦ड़頋஧῀获⚘捉銫两興컍׾ꔰ짖쐀赱⨻�㺍ꊍ襀曂䷣Ⲉሎ̀摩ኲ౗䎟ϵ诅耥鋚��䋝쪭窰랪稛蕄䦼둮ᷣۥ㨉噽䭂徆呎쩀꤅걓젯�ᗕ址ꂪ갨胆牉ᗫ�ꤌ鼾臖㉐੾⏏᜷穷걇鿰ᐾ�霌볁脈ꀈ뎡癁䵵ㄼ蝭⪁争伲鱾烁籓쭍旇꿖泀⶝諴陌镫楸瓇׬核눣髎侰ၒ铵霵铂浛춤䉶厱㛏骊뒐ᘾ┒嵔辀↗略䈋庑쮴喦⹹铟ູᬏ郺傾ꈋ᠏ⅸ屺涷骘桒孨砞✠뎘�벞鬫⣟紈�祈캅⥤槐㞹⺩穽⎱Ӂ絘貑꫒敮ꑅ럕븊눣竃ꏥ㱳э넀俘弐칄쀚䥼설ퟁ靹哠䵈�ࢡ衺髑۷ꇩ璅羙婾㣖䥬⫮㙚鵒嬳평冨倝㼀霭⬼閈扱趿鑆ㇼ哃ᔜ㡥懄犙꜂ね髋㔤戵Ƿ倨ꩫ嗲뫎祕㲱죩躿ᬜ✩쭾‌瑶ਔ럘ꖠ⛙�颕僈䊱㭮ݺ�嚳䰯붽岀끰퀥ᓅ屌࣢ⵖ䣬⊛射脫উꯘ뻮驢蝇굩얾ᑯꃡ偂⌫ᆝ㛦鑏ぃ蛓긨ꪡḔ떹ᨁθ푐
    "#;
	let lexer = Lexer::new(file(), random_unicode);
	verify_span_is_continuous(lexer);

	let random_ascii = r#"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"#;
	let lexer = Lexer::new(file(), random_ascii);
	verify_span_is_continuous(lexer);

	let random_code = r#"
		struct S {}

		fn main() {
			while true { break continue; }

			let x = 1;
			let y = 2;
			let z = x +-&<< y;

			// comment

			a();
		}
	"#;
	let lexer = Lexer::new(file(), random_code);
	verify_span_is_continuous(lexer);
}

fn verify_span_is_continuous(mut lexer: Lexer) {
	let mut next = lexer.next();
	let mut last_end = 0;

	while !matches!(next.kind, T![eof]) {
		let span = next.span;
		assert_eq!(last_end, span.start);
		last_end = span.end;
		next = lexer.next();
	}
}

fn file() -> Id<str> { Id::new(1) }
