use std::{borrow::Cow, fmt::Display};

use diagnostics::{emit, quick_diagnostic, DiagKind, FileCache, FilePath, FullDiagnostic};
use hir_lower::{index::generate_index, tree::generate_tree, Module};
use parse::ParseContext;
use verde::{db, Db};

#[db]
struct Database(hir::Storage, hir_lower::Storage);

/// A filesystem interface for the compiler.
pub trait Filesystem {
	/// Possible errors that can occur when interfacing with the filesystem.
	type Error: Display;

	/// Read the source file at `path` to memory.
	fn read_source(&mut self, path: &str) -> Result<String, Self::Error>;
}

/// The input to the compiler.
pub struct CompileInput<'a, F> {
	pub filesystem: F,
	/// The files to compile. The root file is the first one.
	pub files: Cow<'a, [Cow<'a, str>]>,
}

pub struct CompileOutput {}

/// Compile.
pub fn compile<F: Filesystem>(mut input: CompileInput<F>) -> CompileOutput {
	let mut db = Database::default();
	let db = &mut db as &mut dyn Db;

	let Parsed { modules, cache } = parse(&mut input);
	let indices: Vec<_> = modules
		.iter()
		.map(|x| {
			let (index, diags) = generate_index(x);
			emit(diags, &cache, &x.file);
			db.set_input(index)
		})
		.collect();

	// We seem to have no files, bail.
	if indices.is_empty() {
		return CompileOutput {};
	}

	let tree = db.execute(|db| generate_tree(db, &indices));

	CompileOutput {}
}

struct Parsed {
	modules: Vec<Module>,
	cache: FileCache,
}

fn parse<F: Filesystem>(input: &mut CompileInput<F>) -> Parsed {
	let mut parser = Parser::new(&mut input.filesystem);
	let modules: Vec<_> = input.files.iter().flat_map(|x| parser.parse(x.as_ref())).collect();
	let (diags, cache) = parser.finish();
	emit(diags, &cache, &());
	Parsed { modules, cache }
}

struct Parser<'a, F> {
	cache: FileCache,
	context: ParseContext,
	filesystem: &'a mut F,
	diagnostics: Vec<FullDiagnostic>,
}

impl<'a, F: Filesystem> Parser<'a, F> {
	fn new(filesystem: &'a mut F) -> Self {
		Self {
			cache: FileCache::new(),
			context: ParseContext::new(),
			filesystem,
			diagnostics: Vec::new(),
		}
	}

	fn parse(&mut self, path: &str) -> Option<Module> {
		let file = FilePath::new(path);
		let source = match self.filesystem.read_source(path) {
			Ok(source) => source,
			Err(err) => {
				quick_diagnostic(DiagKind::Error, format!("Failed to read file `{}`: {}", path, err));
				return None;
			},
		};
		let (ast, diags) = self.context.parse_file(&source);
		self.cache.set_file(file, source);
		self.diagnostics
			.extend(diags.into_iter().map(|x| x.map_span(|x| x.with(file))));
		Some(Module { file, ast })
	}

	fn finish(self) -> (Vec<FullDiagnostic>, FileCache) { (self.diagnostics, self.cache) }
}
