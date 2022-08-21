use std::collections::HashMap;

use ast::{Ident, Type};
use package::File;

use crate::Path;

#[salsa::tracked]
pub struct FromStubs {
	#[return_ref]
	pub stubs: HashMap<File, Stub>,
}

#[salsa::tracked]
pub struct Stub {
	#[id]
	path: Path,
	public: bool,
	items: Vec<StubItem>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StubItem {
	pub public: bool,
	pub kind: StubItemKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StubItemKind {
	Struct(Ident),
	Enum(Ident),
	Trait(Ident),
	TypeAlias(Ident),
	Fn(Ident),
	Impl {
		what: Type,
		on: Option<Type>,
		items: Vec<StubItem>,
	},
}
