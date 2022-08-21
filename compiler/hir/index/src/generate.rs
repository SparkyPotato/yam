use ast::{Item, ItemKind, Module};
use package::ModuleTree;
use tracing::{span, Level};

use crate::{
	stub::{StubItem, StubItemKind},
	Db,
	FromStubs,
	Index,
	Path,
	Stub,
};

pub fn generate_index(db: &dyn Db, tree: ModuleTree) -> Index {
	let s = span!(Level::TRACE, "generating index");
	let _enter = s.enter();

	let stubs = generate_stubs(db, tree);
	merge_stubs(db, stubs)
}

#[salsa::tracked]
pub fn generate_stubs(db: &dyn Db, tree: ModuleTree) -> FromStubs {
	let stubs = tree
		.modules(db)
		.iter()
		.map(|(f, d)| {
			let path = d.path(db);
			let public = d.public(db);
			let module = d.module(db);
			let path = Path::new(db, path.clone());

			let stub = generate_stub(db, path, public, module);

			(*f, stub)
		})
		.collect();

	FromStubs::new(db, stubs)
}

#[salsa::tracked]
pub fn merge_stubs(db: &dyn Db, stubs: FromStubs) -> Index {
	let s = span!(Level::TRACE, "merging stubs");
	let _enter = s.enter();

	Index::new(db, stubs.stubs(db).iter().map(|(_, s)| s.clone()).collect())
}

#[salsa::tracked]
pub fn generate_stub(db: &dyn Db, path: Path, public: bool, module: Module) -> Stub {
	let s = span!(Level::TRACE, "generating stub");
	let _enter = s.enter();

	let mut items = Vec::new();
	for item in module.items() {
		if let Some(item) = item_to_stub_item(item) {
			items.push(item);
		}
	}

	Stub::new(db, path, public, items)
}

fn item_to_stub_item(item: Item) -> Option<StubItem> {
	if let Some(kind) = item.kind() {
		let kind = match kind {
			ItemKind::Struct(x) => {
				if let Some(ident) = x.ident() {
					Some(StubItemKind::Struct(ident))
				} else {
					None
				}
			},
			ItemKind::Enum(x) => {
				if let Some(ident) = x.ident() {
					Some(StubItemKind::Enum(ident))
				} else {
					None
				}
			},
			ItemKind::Trait(x) => {
				if let Some(ident) = x.ident() {
					Some(StubItemKind::Trait(ident))
				} else {
					None
				}
			},
			ItemKind::TypeAlias(x) => {
				if let Some(ident) = x.ident() {
					Some(StubItemKind::TypeAlias(ident))
				} else {
					None
				}
			},
			ItemKind::Fn(x) => {
				if let Some(ident) = x.ident() {
					Some(StubItemKind::Fn(ident))
				} else {
					None
				}
			},
			ItemKind::Impl(x) => {
				if let Some(what) = x.what() {
					let on = x.on();
					let items = x
						.items()
						.map(|m| m.items().filter_map(|x| item_to_stub_item(x)).collect())
						.unwrap_or(Vec::new());
					Some(StubItemKind::Impl { what, on, items })
				} else {
					None
				}
			},
			ItemKind::Mod(_) => None,
		};
		let public = item.visibility().is_some();

		if let Some(kind) = kind {
			Some(StubItem { public, kind })
		} else {
			None
		}
	} else {
		None
	}
}
