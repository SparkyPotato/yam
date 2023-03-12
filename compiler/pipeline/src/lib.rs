use std::path::Path;

use diagnostics::FilePath;
use rustc_hash::{FxHashMap, FxHashSet};
use syntax::ast::File;
use text::Text;

pub type HashMap<K, V> = FxHashMap<K, V>;
pub type HashSet<T> = FxHashSet<T>;

pub struct ModuleTree {
	files: HashMap<FilePath, File>,
	root: Module,
	root_path: &'static Path,
}

#[derive(Default)]
struct Module {
	file: Option<FilePath>,
	children: HashMap<Text, Module>,
}

impl ModuleTree {
	pub fn new(path: FilePath, file: File) -> Self {
		Self {
			files: {
				let mut h = HashMap::default();
				h.insert(path, file);
				h
			},
			root: Module {
				file: Some(path),
				children: HashMap::default(),
			},
			root_path: path.path().parent().expect("Root file path has no parent"),
		}
	}

	pub fn add_file(&mut self, path: FilePath, file: File) {
		self.files.insert(path, file);

		// Ensure path is something like `src/x/x.yam`.
		let p = path.path();
		assert_eq!(p.extension(), Some("yam".as_ref()), "Path is not a .yam file");
		let relative = p.strip_prefix(self.root_path).expect("Path is not a child of the root");
		// `relative` is now `x/x.yam`.

		let mut module = &mut self.root;
		let mut last_name = String::new();
		for component in relative.components() {
			let path: &Path = component.as_ref(); // `x` or `x.yam`.
			let path = path.to_str().expect("Path is not valid UTF-8");

			let file_name = path.strip_suffix(".yam");
			if file_name != Some(last_name.as_str()) {
				// Create a child module if we aren't in `x/x.yam`.
				last_name = path.to_string();
				let name = Text::new(path);
				module = module.children.entry(name).or_default();
			}
		}
		module.file = Some(path);
	}

	pub fn get_file(&self, path: FilePath) -> Option<&File> { self.files.get(&path) }
}
