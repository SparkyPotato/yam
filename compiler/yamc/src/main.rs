use std::path::PathBuf;

use clap::Parser;
use driver::CompileInput;
use tracing::Level;
use tracing_subscriber::fmt;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Options {
	path: PathBuf,
}

struct Filesystem;

impl driver::Filesystem for Filesystem {
	fn read_file(&mut self, path: &std::path::Path) -> String { std::fs::read_to_string(path).unwrap() }
}

fn main() {
	let _ = tracing::subscriber::set_global_default(fmt().pretty().with_max_level(Level::TRACE).finish());

	let options = Options::parse();
	driver::compile(CompileInput {
		root: &options.path,
		filesystem: Filesystem,
	});
}
