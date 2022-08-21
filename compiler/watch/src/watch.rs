use std::{
	path::{Path, PathBuf},
	sync::mpsc::channel,
	time::Duration,
};

use database::Database;
use diag::{quick_diagnostic, DiagKind, FileCacheBuilder};
use index::generate::generate_index;
use intern::{Id, Resolver, UnsizedInterner};
use notify::{watcher, DebouncedEvent, RecursiveMode, Watcher};
use package::{Package, TextIntern};
use tracing::{event, Level};

use crate::Options;

pub fn watch(opts: Options) {
	let mut db = Database::default();
	let mut pkg = parse_root(&mut db, &opts.root);
	pipeline(&mut db, &pkg);

	let (tx, rx) = channel();
	let mut watcher = watcher(tx, Duration::from_millis(10)).unwrap();
	watcher.watch(".", RecursiveMode::Recursive).unwrap();
	let curr_dir = std::env::current_dir().unwrap();

	loop {
		if let Ok(event) = rx.recv() {
			match event {
				DebouncedEvent::Write(path) => {
					let relative = path.strip_prefix(&curr_dir).unwrap();
					event!(Level::TRACE, "File modified: {:?}", relative);
					parse(&mut db, &mut pkg, &relative, false);
					pipeline(&mut db, &pkg);
				},
				_ => {},
			}
		}
	}
}

fn pipeline(db: &mut Database, pkg: &Package<FileResolver>) {
	emit_diags(pkg);
	let index = generate_index(db, pkg.tree());
}

fn parse_root(db: &mut Database, root: &Path) -> Package<FileResolver> {
	std::env::set_current_dir(root.parent().unwrap()).expect("Failed to set current directory");
	let path = root.file_name().unwrap();

	let mut pkg = Package::new(
		db,
		FileResolver {
			stack: Vec::new(),
			cache: Some(FileCacheBuilder::new()),
		},
	);
	parse(db, &mut pkg, Path::new(path), true);

	pkg
}

fn parse(db: &mut Database, pkg: &mut Package<FileResolver>, file_p: &Path, force: bool) {
	let id = pkg.interner_mut().intern(file_p.to_str().unwrap());
	if !pkg.does_file_exist(id) && !force {
		return;
	}

	let file = std::fs::read_to_string(file_p);

	let file = match file {
		Ok(file) => file,
		Err(err) => {
			quick_diagnostic(DiagKind::Error, format!("could not load file: {}", err));
			return;
		},
	};
	pkg.parse_file(db, id, &file);

	pkg.resolver_mut().cache.as_mut().unwrap().set_file(id, file);
}

fn emit_diags(pkg: &Package<FileResolver>) {
	let cache = pkg.resolver().cache.as_ref().unwrap().cache(pkg.interner());
	for diags in pkg.get_all_diags() {
		diags.emit(&cache);
	}
}

pub struct FileResolver {
	cache: Option<FileCacheBuilder>,
	stack: Vec<Id<str>>,
}

impl package::FileResolver for FileResolver {
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
			Ok(data) => Some((interner.intern(p1.to_str().unwrap()), data)),
			Err(_) => match {
				event!(Level::TRACE, "searching for package at {:?}", p2);
				std::fs::read_to_string(&p2)
			} {
				Ok(data) => Some((interner.intern(p2.to_str().unwrap()), data)),
				Err(_) => None,
			},
		}
	}

	fn done_with_data(&mut self, file: Id<str>, data: String, _: &mut TextIntern) {
		self.cache.as_mut().unwrap().set_file(file, data)
	}
}
