use std::path::PathBuf;

use clap::Parser;

#[derive(Parser)]
pub struct Options {
	pub root: PathBuf,
	#[clap(long)]
	pub debug_trace: bool,
}
