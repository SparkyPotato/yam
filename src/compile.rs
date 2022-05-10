use std::path::{Path, PathBuf};

use clap::clap_derive::Parser;
use diag::{
	ariadne::{Report, ReportKind},
	emit_diagnostics,
	quick_diagnostic,
	FileCacheBuilder,
	Span,
};
use lasso::Rodeo;
use parse::{ast::Module, parse};

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

	let module = match load_and_parse(&opts.path, &mut rodeo, &mut cache, &mut diagnostics) {
		Ok(file) => file,
		Err(err) => {
			quick_diagnostic(ReportKind::Error, format!("File could not be loaded: {}", err));
			return;
		},
	};
	mod_verify::verify(&module, &mut diagnostics);

	let cache = cache.finish(&rodeo);
	emit_diagnostics(&cache, diagnostics);
}

pub fn load_and_parse(
	path: &Path, rodeo: &mut Rodeo, cache: &mut FileCacheBuilder, diags: &mut Vec<Report<Span>>,
) -> Result<Module, std::io::Error> {
	let input = std::fs::read_to_string(path)?;
	let file = cache.add_file(rodeo, path);
	let module = parse(file, &input, rodeo, diags);
	cache.set_file(file, input);
	Ok(module)
}
