use std::sync::Mutex;

use codegen::codegen_declare;
pub use codegen::{target, CodegenOptions};
use diagnostics::{emit, DiagKind, FileCache, FilePath, FullDiagnostic};
use hir::{
	ast::AstMap,
	ident::{AbsPath, PackageId},
	lang_item::{build_lang_item_map, LangItemMap},
	ItemDiagnostic,
};
use hir_lower::{
	index::{
		build_ast_map,
		canonical::{canonicalize_tree, CanonicalTree},
		local::{build_package_tree, generate_index, Index},
		ModuleMap,
		TempMap,
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
use tycheck::type_check;
use verde::{db, Db, Id};

#[db]
pub struct Database(hir::Storage, hir_lower::Storage, thir::Storage, tycheck::Storage);

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
	/// Options controlling code generation.
	pub codegen_options: CodegenOptions,
	pub output: FilePath,
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

	let (modules, cache) = parse(db, input.files);
	let (indices, maps) = index_gen(db, &modules);
	let (packages, tree) = packages(db, &indices);
	let (items, lang_item_map, amap, tmap) = hir(db, &modules, maps, packages, tree);
	let thir = tyck(db, items, lang_item_map);

	if should_codegen(db) {
		let package = codegen(db, &input.codegen_options, &thir);
		std::fs::write(input.output.path(), package).unwrap();
	}

	emit_all(db, &cache, &amap, &tmap);

	CompileOutput { db: dbc }
}

fn parse(db: &(dyn Db + Send + Sync), files: Vec<SourceFile>) -> (Vec<Id<Module>>, FileCache) {
	let f = files.get(0).expect("No source files provided");
	let parser = Parser::new(db, f.path);
	let modules: Vec<_> = files.into_par_iter().map(|x| parser.parse(x)).collect();
	let cache = parser.finish();
	(modules, cache)
}

fn index_gen(db: &(dyn Db + Send + Sync), modules: &[Id<Module>]) -> (Vec<Id<Index>>, Vec<ModuleMap>) {
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

	(indices, maps)
}

fn packages(db: &(dyn Db + Send + Sync), indices: &[Id<Index>]) -> (Id<VisiblePackages>, Id<CanonicalTree>) {
	let s = span!(Level::DEBUG, "build package tree");
	let _e = s.enter();

	let tree = db.execute(|ctx| build_package_tree(ctx, &indices));
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
	let tree = db.execute(|ctx| canonicalize_tree(ctx, tree, vis_packages));
	(packages, tree)
}

fn hir(
	db: &(dyn Db + Send + Sync), modules: &[Id<Module>], mut maps: Vec<ModuleMap>, packages: Id<VisiblePackages>,
	tree: Id<CanonicalTree>,
) -> (FxHashMap<Id<AbsPath>, Id<hir::Item>>, Id<LangItemMap>, AstMap, TempMap) {
	let modules: Vec<_> = modules
		.into_par_iter()
		.zip(maps.par_iter_mut())
		.map(move |(&x, map)| db.execute(|ctx| lower_to_hir(ctx, x, packages, tree, map)))
		.collect();
	let (amap, tmap) = build_ast_map(maps);
	let items = build_hir_sea(db, modules);
	let lang_item_map = db.execute(|ctx| build_lang_item_map(ctx, &items));
	(items, lang_item_map, amap, tmap)
}

fn tyck(
	db: &(dyn Db + Send + Sync), hir: FxHashMap<Id<AbsPath>, Id<hir::Item>>, lang_item_map: Id<LangItemMap>,
) -> thir::Thir {
	let decls: FxHashMap<_, _> = hir
		.par_iter()
		.map(|(&path, &item)| {
			(
				path,
				db.execute(|ctx| tycheck::decl::type_decl(ctx, item, lang_item_map, &hir)),
			)
		})
		.collect();
	let items = hir
		.par_iter()
		.map(|(&path, &item)| {
			(
				path,
				db.execute(|ctx| type_check(ctx, item, decls[&path], lang_item_map, &hir, &decls)),
			)
		})
		.collect();
	thir::Thir { hir, decls, items }
}

fn should_codegen(db: &dyn Db) -> bool {
	let r = db.get_all::<FullDiagnostic>().any(|x| x.kind == DiagKind::Error)
		|| db.get_all::<TempDiagnostic>().any(|x| x.kind == DiagKind::Error)
		|| db.get_all::<ItemDiagnostic>().any(|x| x.kind == DiagKind::Error);
	!r
}

fn codegen(db: &(dyn Db + Send + Sync), options: &CodegenOptions, thir: &thir::Thir) -> Vec<u8> {
	let decls = codegen_declare(db, options, thir);
	thir.hir
		.par_iter()
		.for_each(|(id, &hir)| codegen::codegen_item(db, options, &decls, thir, hir, thir.items[id]));
	decls.finish()
}

fn emit_all(db: &(dyn Db + Send + Sync), cache: &FileCache, amap: &AstMap, tmap: &TempMap) {
	emit(db.get_all::<FullDiagnostic>().cloned(), &cache, &());
	emit(db.get_all::<TempDiagnostic>().cloned(), &cache, &tmap);
	emit(db.get_all::<ItemDiagnostic>().cloned(), &cache, &amap);
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
