use std::path::PathBuf;

use clap::clap_derive::Parser;
use lasso::Rodeo;
use yamd::{ariadne::ReportKind, emit_diagnostics, FileCacheBuilder};
use yamp::parse;

use crate::quick_diagnostic;

#[derive(Parser)]
#[clap(author, version, about)]
pub struct CompileOptions {
	/// The path of the main package file.
	path: PathBuf,
}

pub fn compile(opts: CompileOptions) {
	let mut rodeo = Rodeo::new();

	let mut cache = FileCacheBuilder::new();
	let mut diagnostics = Vec::new();

	let input = match std::fs::read_to_string(&opts.path) {
		Ok(file) => file,
		Err(err) => {
			quick_diagnostic(ReportKind::Error, format!("File could not be found: {}", err));
			return;
		},
	};
	let file = cache.add_file(&mut rodeo, &opts.path);
	let module = parse(file, &input, &mut rodeo, &mut diagnostics);
	cache.set_file(file, input);
	println!("{:#?}", module);

	let cache = cache.finish(&rodeo);
	emit_diagnostics(&cache, diagnostics);
}
