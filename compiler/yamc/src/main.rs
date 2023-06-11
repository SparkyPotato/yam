use std::{borrow::Cow, io};

use clap::Parser;
use driver::CompileInput;
use tracing_subscriber::{fmt, EnvFilter};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Options {
	path: String,
}

struct Filesystem;

impl driver::Filesystem for Filesystem {
	type Error = io::Error;

	fn read_source(&mut self, path: &str) -> Result<String, Self::Error> { std::fs::read_to_string(path) }
}

fn main() {
	let _ =
		tracing::subscriber::set_global_default(fmt().pretty().with_env_filter(EnvFilter::from_env("YAMLOG")).finish());

	let options = Options::parse();
	driver::compile(CompileInput {
		files: Cow::Borrowed(&[Cow::Borrowed(options.path.as_str())]),
		filesystem: Filesystem,
	});
}
