#![allow(incomplete_features)]
#![feature(trait_upcasting)]

use diag::Diagnostics;
use intern::Id;
pub use parse::syntax::intern::TextIntern;
use parse::{ast::ItemKind, syntax::builder::TreeBuilderContext, Cst};
use tracing::{span, Level};

use crate::graph::ModuleTreeBuilder;
pub use crate::graph::{File, Module, ModuleData, ModuleTree};

mod graph;

#[salsa::jar(db = Db)]
pub struct Jar(ModuleData, ModuleTree);

pub trait Db: salsa::DbWithJar<Jar> {}

impl<DB> Db for DB where DB: ?Sized + salsa::DbWithJar<Jar> {}

pub trait FileResolver {
	fn push_module(&mut self, module: Id<str>, interner: &mut TextIntern);

	fn pop_module(&mut self);

	fn resolve_module(&mut self, module: Id<str>, interner: &mut TextIntern) -> Option<(Id<str>, String)>;

	fn done_with_data(&mut self, file: Id<str>, data: String, interner: &mut TextIntern);
}

pub struct Package<T> {
	builder: TreeBuilderContext<'static>,
	tree: ModuleTreeBuilder,
	resolver: T,
}

impl<T: FileResolver> Package<T> {
	pub fn new(db: &mut dyn Db, resolver: T) -> Self {
		Self {
			builder: TreeBuilderContext::new(),
			tree: ModuleTreeBuilder::new(db),
			resolver,
		}
	}

	pub fn interner(&self) -> &TextIntern { self.builder.interner() }

	pub fn interner_mut(&mut self) -> &mut TextIntern { self.builder.interner_mut() }

	pub fn resolver(&self) -> &T { &self.resolver }

	pub fn resolver_mut(&mut self) -> &mut T { &mut self.resolver }

	pub fn tree(&self) -> ModuleTree { self.tree.tree() }

	pub fn does_file_exist(&self, file: Id<str>) -> bool { self.tree.does_file_exist(File::from_id(file)) }

	pub fn parse_file(&mut self, db: &mut dyn Db, file: Id<str>, source: &str) {
		let file = File::from_id(file);

		let (path, public) = if let Some(curr) = self.tree.get_path_vis(db, file) {
			curr
		} else {
			(Vec::new(), true)
		};

		self.parse_recursive_inner(db, path, public, file, source);
		self.tree.flush_tree(db);
	}

	pub fn get_all_diags(&self) -> impl Iterator<Item = &Diagnostics> { self.tree.all_diags() }

	pub fn done(self) -> (TextIntern, T) { (self.builder.finalize(), self.resolver) }

	fn parse_recursive_inner(&mut self, db: &mut dyn Db, path: Vec<Module>, public: bool, file: File, source: &str) {
		self.tree.invalidate_children(file);

		let mut diags = Diagnostics::new();
		let cst = self.parse_inner(file, source, &mut diags);

		for (ident, vis) in cst.to_module().items().filter_map(|x| match x.kind() {
			Some(ItemKind::Mod(m)) => Some((m.ident()?, x.visibility())),
			_ => None,
		}) {
			let m = Module::from_ident(ident);
			if self.tree.child_is_valid(file, m) {
				continue;
			}

			if let Some((f, data)) = self.resolver.resolve_module(ident.name(), self.builder.interner_mut()) {
				let f = File::from_id(f);
				self.resolver.push_module(ident.name(), self.builder.interner_mut());

				let path = path
					.iter()
					.copied()
					.chain(std::iter::once(Module::from_ident(ident)))
					.collect();
				self.parse_recursive_inner(db, path, vis.is_some(), f, &data);
				self.resolver.done_with_data(f.id(), data, self.builder.interner_mut());
				self.tree.add_child(file, m, f);

				self.resolver.pop_module();
			} else {
				diags.push(
					ident
						.span()
						.error("could not find file for module")
						.label(ident.span().mark()),
				);
			}
		}

		self.tree.add_file(db, file, path, public, cst, diags);
	}

	fn parse_inner(&mut self, file: File, source: &str, diags: &mut Diagnostics) -> Cst {
		let s = span!(Level::TRACE, "parse", file = self.builder.resolve(file.id()));
		let _e = s.enter();

		Cst::parse(&mut self.builder, file.id(), source, diags)
	}
}
