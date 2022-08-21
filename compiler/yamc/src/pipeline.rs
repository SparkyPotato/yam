use std::path::PathBuf;

use database::Database;
use diag::{quick_diagnostic, DiagKind, FileCacheBuilder};
use index::generate_index;
use intern::{Id, Resolver};
use module::{Package, TextIntern};
use tracing::{event, Level};

use crate::Options;

pub fn run_pipeline(opts: Options) {
	let mut db = Database::default();

	let pkg = match parse_tree(&mut db, &opts.root) {
		Some(pkg) => pkg,
		None => return,
	};

	let index = generate_index(&mut db, &pkg);

	emit_diags(pkg);
}

fn parse_tree(db: &mut Database, root: &PathBuf) -> Option<Package<FileResolver>> {
	let root_file = std::fs::read_to_string(root);

	let root_file = match root_file {
		Ok(root_file) => root_file,
		Err(err) => {
			quick_diagnostic(DiagKind::Error, format!("could not load file: {}", err));
			return None;
		},
	};

	std::env::set_current_dir(root.parent().unwrap()).expect("Failed to set current directory");

	let mut pkg = Package::new(FileResolver {
		stack: Vec::new(),
		cache: None,
	});
	let mut cache = FileCacheBuilder::new();
	let id = cache.add_file(pkg.interner_mut(), root);

	pkg.resolver_mut().cache = Some(cache);
	pkg.parse_file(db, id, &root_file);
	pkg.resolver_mut().cache.as_mut().unwrap().set_file(id, root_file);

	Some(pkg)
}

fn emit_diags(ctx: Package<FileResolver>) {
	let (interner, resolver, files) = ctx.done();
	let cache = resolver.cache.as_ref().unwrap().cache(&interner);
	for diags in files.filter_map(|(_, d)| d.in_tree.then_some(d.diags)) {
		diags.emit(&cache);
	}
}

pub struct FileResolver {
	cache: Option<FileCacheBuilder>,
	stack: Vec<Id<str>>,
}

impl module::FileResolver for FileResolver {
	fn push_module(&mut self, file: Id<str>, _: &mut TextIntern) { self.stack.push(file); }

	fn pop_module(&mut self) { self.stack.pop(); }

	fn resolve_module(&mut self, module: Id<str>, interner: &mut TextIntern) -> Option<(Id<str>, String)> {
		let mut path = self.stack.clone();
		path.push(module);

		let mut p1 = PathBuf::new();
		let mut p2 = PathBuf::new();
		for part in path.iter() {
			p1.push(interner.resolve(*part));
			p2.push(interner.resolve(*part));
		}
		p1.set_extension("yam");
		p2.push("mod.yam");

		match {
			event!(Level::TRACE, "searching for package at {:?}", p1);
			std::fs::read_to_string(&p1)
		} {
			Ok(data) => Some((self.cache.as_mut().unwrap().add_file(interner, &p1), data)),
			Err(_) => match {
				event!(Level::TRACE, "searching for package at {:?}", p2);
				std::fs::read_to_string(&p2)
			} {
				Ok(data) => Some((self.cache.as_mut().unwrap().add_file(interner, &p2), data)),
				Err(_) => None,
			},
		}
	}

	fn done_with_data(&mut self, file: Id<str>, data: String, _: &mut TextIntern) {
		self.cache.as_mut().unwrap().set_file(file, data)
	}
}
