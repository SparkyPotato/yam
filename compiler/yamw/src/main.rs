use std::{
	ffi::OsStr,
	path::{Path, PathBuf},
	time::Duration,
};

use clap::Parser;
use diagnostics::{emit, quick_diagnostic, DiagKind, FileCache, FileDiagnostic, FilePath, FullSpan};
use hir_lower::{
	index::{generate_index, InnerIndex},
	tree::generate_tree,
	Module,
};
use notify_debouncer_mini::{new_debouncer, notify::RecursiveMode};
use parse::{syntax::ast::File, ParseContext};
use rustc_hash::FxHashMap;
use tracing::trace;
use tracing_subscriber::{fmt, fmt::format::FmtSpan, EnvFilter};
use verde::{db, Db, Id};
use walkdir::WalkDir;

#[db]
struct Database(hir::Storage, hir_lower::Storage);

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Options {
	path: String,
}

struct FileData {
	file: FilePath,
	ast: File,
	diags: Vec<FileDiagnostic>,
}

struct WatchContext<'a> {
	db: &'a mut dyn Db,
	cache: FileCache,
	file_map: FxHashMap<FilePath, usize>,
	files: Vec<FileData>,
	indices: Vec<Id<InnerIndex>>,
	parser: ParseContext,
	root: FilePath,
}

impl<'a> WatchContext<'a> {
	fn new(db: &'a mut dyn Db, root: FilePath) -> Self {
		Self {
			db,
			cache: FileCache::new(),
			file_map: FxHashMap::default(),
			files: Vec::new(),
			indices: Vec::new(),
			parser: ParseContext::new(),
			root,
		}
	}

	fn on_file_change(&mut self, path: &Path) {
		if !matches!(path.extension().and_then(OsStr::to_str), Some("yam")) {
			return;
		}
		let path = path.canonicalize().unwrap();

		trace!("file changed: {}", path.display());

		let Ok(source) = std::fs::read_to_string(&path) else {
			return;
		};
		let Some(path) = path.as_os_str().to_str() else {
			quick_diagnostic(DiagKind::Error, "paths are required to be UTF-8");
			return;
		};
		let file = FilePath::new(path);
		let (ast, mut diags) = self.parser.parse_file(&source);
		self.cache.set_file(file, source);

		let (index, index_diags) = generate_index(Module { file, ast: ast.clone() });
		diags.extend(index_diags);

		let index = self.db.set_input(index);
		match self.file_map.get(&file) {
			Some(id) => {
				self.files[*id] = FileData { file, ast, diags };
				self.indices[*id] = index;
			},
			None => {
				self.file_map.insert(file, self.files.len());
				self.files.push(FileData { file, ast, diags });
				self.indices.push(index);
			},
		}
	}

	fn recompile(&mut self) {
		for data in self.files.iter() {
			emit(data.diags.iter().cloned(), &self.cache, &data.file);
		}
		self.order_files();

		let tree = self.db.execute(|db| generate_tree(db, &self.indices));
	}

	fn order_files(&mut self) {
		let root_id = *self.file_map.get(&self.root).unwrap();
		if root_id != 0 {
			self.file_map.insert(self.root, 0);
			self.file_map.insert(self.files[0].file, root_id);
			self.files.swap(0, root_id);
		}
	}
}

fn main() {
	let _ = tracing::subscriber::set_global_default(
		fmt()
			.pretty()
			.with_env_filter(EnvFilter::from_env("YAMLOG"))
			.with_span_events(FmtSpan::CLOSE)
			.finish(),
	);
	let mut db = Database::default();
	let db = &mut db as &mut dyn Db;

	let options = Options::parse();
	let Ok(root) = Path::new(&options.path).canonicalize() else {
		quick_diagnostic(DiagKind::Error, format!("`{}` does not exist", options.path));
		return;
	};
	if root.file_name().is_none() {
		quick_diagnostic(DiagKind::Error, format!("`{}` is not a file", root.display()));
	}
	let watch = root.parent().expect("file doesn't have a parent directory");

	let (s, r) = crossbeam_channel::unbounded();

	let Ok(mut watcher) = new_debouncer(Duration::from_millis(500), None, s) else {
		quick_diagnostic(DiagKind::Error, "failed to create file watcher");
		return;
	};

	let mut ctx = WatchContext::new(db, FilePath::new(root.to_str().unwrap()));

	for entry in WalkDir::new(watch) {
		let Ok(entry) = entry else {
			continue;
		};

		ctx.on_file_change(entry.path());
	}
	ctx.recompile();

	let Ok(_) = watcher.watcher().watch(watch, RecursiveMode::Recursive) else {
		quick_diagnostic(DiagKind::Error, "failed to watch directory");
		return;
	};
	while let Ok(event) = r.recv() {
		let Ok(events) = event else {
			continue;
		};

		for event in events {
			ctx.on_file_change(&event.path);
		}
		ctx.recompile();
	}
}
