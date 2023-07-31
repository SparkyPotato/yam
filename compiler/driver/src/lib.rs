use std::sync::Mutex;

use diagnostics::{emit, FileCache, FilePath, FullDiagnostic};
use hir::{ident::PackageId, ItemDiagnostic};
use hir_lower::{
	index::{
		build_ast_map,
		canonical::canonicalize_tree,
		local::{build_package_tree, generate_index},
		ModuleMap,
	},
	lower::{build_hir_sea, lower_to_hir},
	Module,
	Packages,
	TempDiagnostic,
	VisiblePackages,
};
use parse::ParseContext;
use rayon::prelude::*;
use rustc_hash::FxHashMap;
use text::Text;
use tracing::{span, Level};
use verde::{db, Db, Id};

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

	let dbc = input.db;
	let db = &dbc as &(dyn Db + Send + Sync);

	// Parse
	let (modules, cache) = parse(db, input.files);

	// Generate stage 1 indices.
	let mut indices = Vec::with_capacity(modules.len());
	let mut maps = Vec::with_capacity(modules.len());
	modules
		.par_iter()
		.map(|&x| {
			db.execute(|ctx| {
				let m = db.get(x);
				let mut map = ModuleMap::new(m.path, m.file);
				drop(m);

				let index = generate_index(ctx, x, &mut map);
				(index, map)
			})
		})
		.unzip_into_vecs(&mut indices, &mut maps);

	// Build tree for canonicalization.
	let tree = db.execute(|ctx| build_package_tree(ctx, &indices));

	// Build package tree.
	let packages = db.set_input(VisiblePackages {
		package: PackageId(0),
		packages: {
			let mut p = FxHashMap::default();
			p.insert(Text::new("root"), PackageId(0));
			p
		},
	});
	let vis_packages = db.set_input(Packages {
		id: (),
		packages: {
			let mut p = FxHashMap::default();
			p.insert(PackageId(0), packages);
			p
		},
	});

	// Canonicalize tree.
	let tree = db.execute(|ctx| canonicalize_tree(ctx, tree, vis_packages));

	// Lower to HIR: Exposing visible packages, lowering each module, collecting all items, and then generating the
	// global AST map.

	// TODO: Parallelize this (fix deadlock).
	let modules: Vec<_> = modules
		// .into_par_iter().zip(maps.par_iter_mut())
		.into_iter().zip(maps.iter_mut())
		.map(move |(x, map)| db.execute(|ctx| lower_to_hir(ctx, x, packages, tree, map)))
		.collect();

	let items = build_hir_sea(db, modules);
	let (amap, tmap) = build_ast_map(maps);

	// Emit all possible diagnostics now.
	emit(db.get_all::<FullDiagnostic>().cloned(), &cache, &());
	emit(db.get_all::<TempDiagnostic>().cloned(), &cache, &tmap);
	emit(db.get_all::<ItemDiagnostic>().cloned(), &cache, &amap);

	CompileOutput { db: dbc }
}

fn parse(db: &(dyn Db + Send + Sync), files: Vec<SourceFile>) -> (Vec<Id<Module>>, FileCache) {
	let parser = Parser::new(db, files.get(0).expect("No source files provided").path);
	let modules: Vec<_> = files.into_par_iter().map(|x| parser.parse(x)).collect();
	let cache = parser.finish();
	(modules, cache)
}

struct Parser<'a> {
	db: &'a (dyn Db + Send + Sync),
	root: FilePath,
	cache: Mutex<FileCache>,
}

impl<'a> Parser<'a> {
	fn new(db: &'a (dyn Db + Send + Sync), root: FilePath) -> Self {
		Self {
			db,
			root,
			cache: Mutex::new(FileCache::new()),
		}
	}

	fn parse(&self, file: SourceFile) -> Id<Module> {
		let s = span !(Level::DEBUG, "parse", path = %file.path);
		let _e = s.enter();

		let (ast, diags) = ParseContext::new().parse_file(&file.source);
		{
			let mut cache = self.cache.lock().unwrap();
			cache.set_file(file.path, file.source);
			emit(diags, &cache, &file.path);
		}

		self.db
			.set_input(Module::from_file(self.db, self.root, ast, file.path, PackageId(0)))
	}

	fn finish(self) -> FileCache { self.cache.into_inner().unwrap() }
}
