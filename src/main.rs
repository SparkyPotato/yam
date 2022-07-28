use clap::Parser;
use diag::{ariadne::ReportKind, quick_diagnostic};

use crate::compile::CompileOptions;

mod compile;

fn main() {
	if let Err(_) = std::panic::catch_unwind(|| {
		let opts = CompileOptions::parse();
		compile::compile(opts);
	}) {
		eprintln!();
		quick_diagnostic(ReportKind::Error, "ICE: Internal Compiler Error");
		eprintln!("This wasn't supposed to happen. Here's some information to help:");
		eprintln!("yamc version: under heavy development")
	}
}
