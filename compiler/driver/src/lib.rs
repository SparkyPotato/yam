use diagnostics::{emit, FileCache, FilePath, FullDiagnostic};
use hir::ident::{AbsPath, PackageId};
use hir_lower::{
	index::generate_index,
	module::{Module, ModuleTree},
};
use parse::ParseContext;
use verde::{db, Db};

#[db]
pub struct Database(hir::Storage, hir_lower::Storage);

pub struct SourceFile {
	pub path: FilePath,
	pub source: String,
}

/// The input to the compiler.
pub struct CompileInput {
	/// The incremental database. If there was no previous compilation, use `Database::default()`.
	pub db: Database,
	/// The files to compile. The root file is the first one. The order of the others doesn't matter.
	pub files: Vec<SourceFile>,
}

/// The output of the compilation
pub struct CompileOutput {
	/// The incremental database to cache between compilations.
	pub db: Database,
}

pub fn compile(input: CompileInput) -> CompileOutput {
	assert!(!input.files.is_empty(), "no files to compile");

	let mut dbc = input.db;
	let db = &mut dbc as &mut dyn Db;

	let Parsed { modules, cache } = parse(db, input.files);
	let indices: Vec<_> = modules
		.iter()
		.map(|x| {
			let (index, diags) = generate_index(db, x);
			emit(diags, &cache, &x.file);
			index
		})
		.collect(); 
	let tree = ModuleTree::new(db, indices);

	CompileOutput { db: dbc }
}

struct Parsed {
	modules: Vec<Module>,
	cache: FileCache,
}

fn parse(db: &dyn Db, files: Vec<SourceFile>) -> Parsed {
	let mut parser = Parser::new(db, files[0].path);
	let modules: Vec<_> = files.into_iter().map(|x| parser.parse(x)).collect();
	let (diags, cache) = parser.finish();
	emit(diags, &cache, &());
	Parsed { modules, cache }
}

struct Parser<'a> {
	db: &'a dyn Db,
	root: FilePath,
	cache: FileCache,
	context: ParseContext,
	diagnostics: Vec<FullDiagnostic>,
}

impl<'a> Parser<'a> {
	fn new(db: &'a dyn Db, root: FilePath) -> Self {
		Self {
			db,
			root,
			cache: FileCache::new(),
			context: ParseContext::new(),
			diagnostics: Vec::new(),
		}
	}

	fn parse(&mut self, file: SourceFile) -> Module {
		let (ast, diags) = self.context.parse_file(&file.source);
		self.cache.set_file(file.path, file.source);

		self.diagnostics
			.extend(diags.into_iter().map(|x| x.map_span(|x| x.with(file.path))));

		let prefix = self.db.add(AbsPath {
			package: PackageId(0),
			path: None,
		});
		Module::from_file(self.db, self.root, ast, file.path, prefix)
	}

	fn finish(self) -> (Vec<FullDiagnostic>, FileCache) { (self.diagnostics, self.cache) }
}
