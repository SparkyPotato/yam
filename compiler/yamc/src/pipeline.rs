use std::path::PathBuf;

use diag::{quick_diagnostic, DiagKind, FileCacheBuilder};
use intern::{Id, Resolver};
use parse::{syntax::intern::TextIntern, TreeContext};
use tracing::{event, Level};

use crate::{database::Database, Options};

pub fn run_pipeline(opts: Options) {
	let root_file = std::fs::read_to_string(&opts.root);

	let root_file = match root_file {
		Ok(root_file) => root_file,
		Err(err) => {
			quick_diagnostic(DiagKind::Error, format!("could not load file: {}", err));
			return;
		},
	};

	let path = opts.root;
	std::env::set_current_dir(&path.parent().unwrap()).expect("Failed to set current directory");

	let mut db = Database::default();

	let mut ctx = TreeContext::new(FileResolver {
		stack: Vec::new(),
		cache: None,
	});
	let mut cache = FileCacheBuilder::new();
	let id = cache.add_file(ctx.interner(), &path);

	ctx.resolver().cache = Some(cache);
	let diags = ctx.parse_file(&mut db, id, &root_file);
	ctx.resolver().cache.as_mut().unwrap().set_file(id, root_file);

	let (interner, resolver) = ctx.finalize();

	let cache = resolver.cache.unwrap().finish(&interner);
	diags.emit(&cache);
}

pub struct FileResolver {
	cache: Option<FileCacheBuilder>,
	stack: Vec<Id<str>>,
}

impl parse::FileResolver for FileResolver {
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
			event!(Level::TRACE, "searching for module at {:?}", p1);
			std::fs::read_to_string(&p1)
		} {
			Ok(data) => Some((self.cache.as_mut().unwrap().add_file(interner, &p1), data)),
			Err(_) => match {
				event!(Level::TRACE, "searching for module at {:?}", p2);
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
