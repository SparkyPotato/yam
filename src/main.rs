use clap::Parser;

use crate::compile::CompileOptions;

mod compile;
mod parse;

fn main() { let opts = CompileOptions::parse(); }
