use std::path::PathBuf;

use clap::clap_derive::Parser;

#[derive(Parser)]
#[clap(author, version, about)]
pub struct CompileOptions {
	/// The path of the main package file.
	path: PathBuf,
}

pub fn compile(opt: CompileOptions) {}
