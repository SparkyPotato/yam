use clap::Parser;

use crate::{options::Options, pipeline::run_pipeline};

mod database;
mod options;
mod pipeline;

fn main() {
	let res = std::panic::catch_unwind(|| {
		let opts: Options = Options::parse();

		if opts.debug_trace {
			tracing_forest::init();
		}

		run_pipeline(opts);
	});

	if let Err(err) = res {
		println!("Internal Compiler Error: {:?}", err);
	}
}
