use std::collections::{hash_map::Entry, HashMap};

use diag::Diagnostics;
use intern::Id;
use parse::{ast, ast::Ident, Cst};

use crate::Db;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct File(Id<str>);

impl File {
	pub fn from_id(id: Id<str>) -> Self { Self(id) }

	pub fn id(self) -> Id<str> { self.0 }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Module(Id<str>);

impl Module {
	pub fn from_ident(ident: Ident) -> Self { Self(ident.name()) }

	pub fn id(self) -> Id<str> { self.0 }
}

pub struct ModuleMeta {
	pub cst: Cst,
	pub diags: Diagnostics,
	pub in_tree: bool,
}

#[salsa::input]
pub struct ModuleData {
	#[return_ref]
	pub path: Vec<Module>,
	pub public: bool,
	pub module: ast::Module,
}

#[salsa::input]
pub struct ModuleTree {
	#[return_ref]
	modules: HashMap<File, ModuleData>,
	#[return_ref]
	children_of: HashMap<File, HashMap<Module, File>>,
}

pub struct ModuleTreeBuilder {
	inner: ModuleTree,
	modules: HashMap<File, ModuleData>,
	children_of: HashMap<File, HashMap<Module, File>>,
	meta: HashMap<File, ModuleMeta>,
}

impl ModuleTreeBuilder {
	pub fn new(db: &mut dyn Db) -> Self {
		Self {
			inner: ModuleTree::new(db, HashMap::new(), HashMap::new()),
			modules: HashMap::new(),
			children_of: HashMap::new(),
			meta: HashMap::new(),
		}
	}

	pub fn does_file_exist(&self, file: File) -> bool { self.modules.contains_key(&file) }

	pub fn all_diags(&self) -> impl Iterator<Item = &Diagnostics> { self.meta.values().map(|m| &m.diags) }

	pub fn get_path_vis(&self, db: &dyn Db, file: File) -> Option<(Vec<Module>, bool)> {
		self.modules.get(&file).map(|x| (x.path(db).clone(), x.public(db)))
	}

	pub fn invalidate_children(&mut self, file: File) {
		if let Some(meta) = self.meta.get_mut(&file) {
			meta.in_tree = false;
		}
	}

	pub fn child_is_valid(&mut self, file: File, child: Module) -> bool {
		if let Some(x) = self.children_of.get(&file) {
			if let Some(x) = x.get(&child) {
				self.meta.get_mut(x).unwrap().in_tree = true;
				true
			} else {
				false
			}
		} else {
			false
		}
	}

	pub fn add_file(
		&mut self, db: &mut dyn Db, file: File, path: Vec<Module>, public: bool, cst: Cst, diags: Diagnostics,
	) {
		match self.modules.entry(file) {
			Entry::Occupied(o) => {
				let o = o.into_mut();
				o.set_path(db, path);
				o.set_public(db, public);
				o.set_module(db, cst.to_module());
			},
			Entry::Vacant(v) => {
				v.insert(ModuleData::new(db, path, public, cst.to_module()));
			},
		}

		match self.meta.entry(file) {
			Entry::Occupied(o) => {
				let o = o.into_mut();
				o.cst = cst;
				o.diags = diags;
				o.in_tree = true;
			},
			Entry::Vacant(v) => {
				v.insert(ModuleMeta {
					cst,
					diags,
					in_tree: true,
				});
			},
		}

		self.children_of.entry(file).or_default();
	}

	pub fn add_child(&mut self, file: File, child: Module, child_file: File) {
		self.children_of.entry(file).or_default().insert(child, child_file);
	}

	pub fn flush_tree(&mut self, db: &mut dyn Db) {
		let files = self
			.meta
			.iter()
			.filter_map(|x| if x.1.in_tree { Some(*x.0) } else { None });

		let mut modules = HashMap::new();
		let mut children_of = HashMap::new();

		for file in files {
			modules.insert(file, self.modules.get(&file).unwrap().clone());
			children_of.insert(file, self.children_of.get(&file).unwrap().clone());
		}

		self.inner.set_modules(db, self.modules.clone());
		self.inner.set_children_of(db, self.children_of.clone());
	}

	pub fn tree(&self) -> ModuleTree { self.inner }
}
