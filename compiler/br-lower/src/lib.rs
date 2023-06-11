use arena::Arena;
use br::{ast::ItemBuilder, AbsolutePath, BrModule, Module, ModuleMap};
use diagnostics::FilePath;
use syntax::{ast, AstToken};
use verde::{Db, Id};

// TODO: Imports and visibility.

pub fn lower_ast(db: &dyn Db, ast: ast::File, file: FilePath, path: Id<AbsolutePath>) -> BrModule {
	let mut module = Module {
		path,
		items: Vec::with_capacity(ast.items().count()),
	};
	let mut map = ModuleMap::new(ast.clone(), file);

	let mut lowerer = ItemLowerer { db, path, map };

	for item in ast.items() {
		let item = lowerer.item(item);
		module.items.extend(item);
	}

	BrModule {
		module: db.set_input(module),
		map: lowerer.map,
	}
}

struct ItemLowerer<'a> {
	db: &'a dyn Db,
	path: Id<AbsolutePath>,
	map: ModuleMap,
}

impl ItemLowerer<'_> {
	fn item(&mut self, item: ast::Item) -> Option<br::Item> {
		let name = match item.item_kind()? {
			ast::ItemKind::Fn(f) => f.name(),
			ast::ItemKind::Struct(s) => s.name(),
			ast::ItemKind::Enum(e) => e.name(),
			ast::ItemKind::TypeAlias(t) => t.name(),
			ast::ItemKind::Static(c) => c.name(),
			ast::ItemKind::Import(_) => return None,
		}?;
		let builder = self.map.add(name.text()?, item.clone());
		let mut lowerer = Lowerer { builder };
		lowerer.item(item)
	}
}

struct Lowerer<'a> {
	builder: ItemBuilder<'a>,
}

impl Lowerer<'_> {
	fn item(&mut self, item: ast::Item) -> Option<br::Item> {
		let mut exprs = Arena::new();
		let mut types = Arena::new();
		let mut locals = Arena::new();

		let (name, attrs, kind) = match item.item_kind()? {
			ast::ItemKind::Fn(f) => {
				let name = self.name(f.name()?)?;
				let attrs = self.attrs(item.attributes());
				(name, attrs)
			},
			ast::ItemKind::Struct(s) => {
				let name = self.name(s.name()?)?;
				let attrs = self.attrs(item.attributes());
				(name, attrs)
			},
			ast::ItemKind::Enum(e) => {
				let name = self.name(e.name()?)?;
				let attrs = self.attrs(item.attributes());
				(name, attrs)
			},
			ast::ItemKind::TypeAlias(t) => {
				let name = self.name(t.name()?)?;
				let attrs = self.attrs(item.attributes());
				(name, attrs)
			},
			ast::ItemKind::Static(s) => {
				let name = self.name(s.name()?)?;
				let attrs = self.attrs(item.attributes());
				(name, attrs)
			},
			ast::ItemKind::Import(_) => return None,
		};

		Some(br::Item {
			name,
			attrs,
			kind,
			exprs,
			types,
			locals,
		})
	}

	fn attrs(&mut self, attrs: impl IntoIterator<Item = ast::Attribute>) -> Vec<br::Attr> {}

	fn name(&mut self, name: ast::Name) -> Option<br::Name> {
		let name = name.ident()?;
		Some(br::Name {
			name: name.text(),
			id: self.builder.add(name),
		})
	}
}
