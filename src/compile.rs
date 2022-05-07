use std::{cell::RefCell, path::PathBuf};

use clap::clap_derive::Parser;
use lasso::Rodeo;
use yamd::{ariadne::ReportKind, emit_diagnostics, FileCache};
use yamp::parse;

use crate::quick_diagnostic;

#[derive(Parser)]
#[clap(author, version, about)]
pub struct CompileOptions {
	/// The path of the main package file.
	path: PathBuf,
}

pub fn compile(opts: CompileOptions) {
	let rodeo = RefCell::new(Rodeo::new());

	let mut cache = FileCache::new(&rodeo);
	let mut diagnostics = Vec::new();

	let input = match std::fs::read_to_string(&opts.path) {
		Ok(file) => file,
		Err(err) => {
			quick_diagnostic(ReportKind::Error, format!("File could not be found: {}", err));
			return;
		},
	};
	let file = cache.add_file(&opts.path);
	let module = parse(file, &input, &rodeo, &mut diagnostics);
	cache.set_file(file, input);
	println!("{:#?}", module);

	emit_diagnostics(&cache, diagnostics);
}
