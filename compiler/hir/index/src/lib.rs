#![allow(incomplete_features)]
#![feature(trait_upcasting)]

use package::Module;

use crate::{
	generate::{generate_stub, generate_stubs, merge_stubs},
	stub::{FromStubs, Stub},
};

pub mod generate;
mod stub;

#[salsa::jar(db = Db)]
pub struct Jar(Index, Path, Stub, FromStubs, generate_stub, generate_stubs, merge_stubs);

pub trait Db: salsa::DbWithJar<Jar> + package::Db {}

impl<DB> Db for DB where DB: ?Sized + salsa::DbWithJar<Jar> + package::Db {}

#[salsa::interned]
pub struct Path {
	#[return_ref]
	segs: Vec<Module>,
}

#[salsa::tracked]
pub struct Index {
	stubs: Vec<Stub>,
}
