use std::path::Path;

use diagnostics::{FileCache, FilePath, FullSpan};
use hir::RawPathInner;
use hir_lower::{index::generate_index, Module};
use parse::ParseContext;
use text::Text;
use verde::{db, Db};

#[db]
struct Database(hir::Storage, hir_lower::Storage);

pub trait Filesystem {
	fn read_file(&mut self, path: &Path) -> String;
}

pub struct CompileInput<'a, F> {
	pub filesystem: F,
	pub root: &'a Path,
}

pub struct CompileOutput {}

pub fn compile<F: Filesystem>(mut input: CompileInput<F>) -> CompileOutput {
	let mut cache = FileCache::new();
	let mut db = Database::default();
	let db = &mut db as &mut dyn Db;

	let mut parser = ParseContext::new();
	let root = FilePath::new(input.root.to_str().expect("Invalid path"));
	let source = input.filesystem.read_file(input.root);
	let (file, diags) = parser.parse_file(&source);

	cache.set_file(root, source);
	let module = db.set_input(Module { path: root, ast: file });
	let path = db.add(RawPathInner {
		prec: None,
		ident: Text::new("root"),
	});

	let index = db.execute(|ctx| generate_index(ctx, path, module));
	println!("{:#?}", *db.get(index));

	for diag in diags {
		diag.map_span(|x| FullSpan {
			start: x.start,
			end: x.end,
			relative: root,
		})
		.emit(&cache, &());
	}

	CompileOutput {}
}
