use std::{
	path::{Path, PathBuf},
	process::Command,
};

use quote::{__private::TokenStream, format_ident, quote};
use ungrammar::Grammar;

fn main() {
	println!("cargo:rerun-if-changed=yam.ungram");
	let grammar: Grammar = std::fs::read_to_string("yam.ungram").unwrap().parse().unwrap();

	let out: PathBuf = (std::env::var("CARGO_MANIFEST_DIR").unwrap() + "/src/").into();

	let kind = out.join("kind.rs");
	std::fs::write(&kind, Generator::new().kind(&grammar).to_string()).unwrap();
	format(&kind);
}

struct Generator {}

impl Generator {
	fn new() -> Self { Self {} }

	fn kind(&mut self, grammar: &Grammar) -> TokenStream {
		let tokens: Vec<_> = grammar
			.tokens()
			.map(|n| {
				let n = format_ident!("{}", map_token(&grammar[n].name));
				quote! { #n, }
			})
			.collect();

		let nodes: Vec<_> = grammar
			.iter()
			.map(|n| {
				let n = format_ident!("{}", &grammar[n].name);
				quote! { #n, }
			})
			.collect();

		quote! {
			#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
			#[repr(u16)]
			pub enum SyntaxKind {
				/// Terminal tokens
				#(#tokens)*

				/// Non-terminal nodes
				#(#nodes)*

				#[doc(hidden)]
				/// Always keep last, is not actually allowed to appear anywhere.
				__Last,
			}
		}
	}
}

fn map_token(x: &str) -> String {
	match x {
		// Lexer tokens
		"bool" => "BoolLit".to_string(),
		"char" => "CharLit".to_string(),
		"float" => "FloatLit".to_string(),
		"int" => "IntLit".to_string(),
		"string" => "StringLit".to_string(),
		"ident" => "Ident".to_string(),
		"@" => "At".to_string(),
		"(" => "LParen".to_string(),
		"{" => "LBrace".to_string(),
		"[" => "LBracket".to_string(),
		")" => "RParen".to_string(),
		"}" => "RBrace".to_string(),
		"]" => "RBracket".to_string(),
		"=" => "Eq".to_string(),
		"." => "Dot".to_string(),
		"*" => "Star".to_string(),
		":" => "Colon".to_string(),
		"," => "Comma".to_string(),
		";" => "Semi".to_string(),
		"->" => "Arrow".to_string(),
		"=>" => "FatArrow".to_string(),
		"_" => "Underscore".to_string(),
		"op" => "Operator".to_string(),
		x => {
			let mut s = String::with_capacity(x.len() + 2);
			s.push_str(&x[0..1].to_ascii_uppercase());
			s.push_str(&x[1..]);
			s.push_str("Kw");
			s
		},
	}
}

fn format(file: &Path) { Command::new("rustfmt").arg(file).status().unwrap(); }
