use std::path::{Path, PathBuf};

// use cfg::lower::lower_to_cfg;
use clap::clap_derive::Parser;
// use codegen::codegen;
use diag::{quick_diagnostic, DiagKind, Diagnostics, FileCacheBuilder};
use hir::resolve;
use parse::{ast::Module, parse, Rodeo};
use tyck::type_check;

#[derive(Parser)]
#[clap(author, version, about)]
pub struct CompileOptions {
	/// The path of the main package file.
	path: PathBuf,
}

pub fn compile(opts: CompileOptions) {
	let mut rodeo = Rodeo::new();
	let mut cache = FileCacheBuilder::new();
	let mut diagnostics = Diagnostics::new();

	let module = match load_and_parse(&opts.path, &mut rodeo, &mut cache, &mut diagnostics) {
		Ok(file) => file,
		Err(err) => {
			quick_diagnostic(DiagKind::Error, format!("file could not be loaded: {}", err));
			return;
		},
	};

	let mut hir = resolve(module, rodeo, &mut diagnostics);
	type_check(&mut hir);

	println!("{:#?}", hir);

	if !diagnostics.was_error() {}

	let cache = cache.finish(hir.rodeo());
	diagnostics.emit(&cache);
}

pub fn load_and_parse(
	path: &Path, rodeo: &mut Rodeo, cache: &mut FileCacheBuilder, diags: &mut Diagnostics,
) -> Result<Module, std::io::Error> {
	let input = std::fs::read_to_string(path)?;
	let file = cache.add_file(rodeo, path);
	let module = parse(file, &input, rodeo, diags);
	cache.set_file(file, input);
	Ok(module)
}
