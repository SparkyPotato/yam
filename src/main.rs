use ariadne::ReportKind;
use clap::Parser;

use crate::{compile::CompileOptions, diagnostic::quick_diagnostic};

mod compile;
mod diagnostic;
mod parse;

fn main() {
	if let Err(_) = std::panic::catch_unwind(|| {
		let opts = CompileOptions::parse();
		compile::compile(opts);
	}) {
		eprintln!();
		quick_diagnostic(ReportKind::Error, "ICE: Internal Compiler Error");
		eprintln!("This wasn't supposed to happen. Here's some information to help:");
		eprintln!("yamc version: <unknown>")
	}
}
