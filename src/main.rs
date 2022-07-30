use clap::Parser;
use diag::{quick_diagnostic, DiagKind};

use crate::compile::CompileOptions;

mod compile;

fn main() {
	let res = std::panic::catch_unwind(|| {
		let opts = CompileOptions::parse();
		compile::compile(opts);
	});

	if res.is_err() {
		eprintln!();
		quick_diagnostic(DiagKind::Error, "ICE: Internal Compiler Error");
		eprintln!("This wasn't supposed to happen. Here's some information to help:");
		eprintln!("yamc version: under heavy development")
	}
}
