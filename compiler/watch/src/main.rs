use clap::Parser;
use tracing_forest::ForestLayer;
use tracing_subscriber::{prelude::*, EnvFilter, Registry};

use crate::{options::Options, watch::watch};

mod options;
mod watch;

fn main() {
	let res = std::panic::catch_unwind(|| {
		let opts: Options = Options::parse();

		if opts.debug_trace {
			Registry::default()
				.with(ForestLayer::default())
				.with(EnvFilter::from_env("YAMLOG"))
				.init();
		}

		watch(opts);
	});

	if let Err(err) = res {
		println!("Internal Compiler Error: {:?}", err);
	}
}
