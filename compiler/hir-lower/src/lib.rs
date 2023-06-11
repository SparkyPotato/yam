use std::path::Path;

use diagnostics::FilePath;
use syntax::ast::File;
use verde::{storage, Db, Id};

pub mod index;

#[storage]
pub struct Storage(index::PublicIndex, index::PrivateIndex);

#[derive(Clone)]
pub struct Module {
	pub ast: File,
	pub file: FilePath,
	pub path: Id<RawPath>,
}

impl Module {
	pub fn new(ast: File, file: FilePath, path: Id<RawPath>) -> Self { Self { path, file, ast } }

	/// Figure out the module's path from it's relative file path from a root file.
	pub fn from_file(db: &mut dyn Db, ast: File, file: FilePath, root: FilePath) -> Self {
		// Ensure path is something like `src/x/x.yam`.
		let p = file.path();
		assert_eq!(p.extension(), Some("yam".as_ref()), "Path is not a .yam file");
		let relative = p.strip_prefix(root.path()).expect("Path is not a child of the root");
		// `relative` is now `x/x.yam`.

		let mut prec = self.root;
		let mut last_name = String::new();
		for component in relative.components() {
			let path: &Path = component.as_ref(); // `x` or `x.yam`.
			let path = path.to_str().expect("Path is not valid UTF-8");

			let file_name = path.strip_suffix(".yam");
			if file_name != Some(last_name.as_str()) {
				// Create a child module if we aren't in `x/x.yam`.
				last_name = path.to_string();
				let name = Text::new(path);
				let path = self.modules[prec].path;
				prec = if let Some(&prec) = self.modules[prec].children.get(&name) {
					prec
				} else {
					let ret = self.modules.push(ModuleData {
						path: Some(db.add(RawPath {
							prec: path,
							ident: name,
						})),
						index: None,
						children: FxHashMap::default(),
					});
					self.modules[prec].children.insert(name, prec);
					ret
				};
			}
		}

		let prec = &mut self.modules[prec];
		prec.index = Some(index);
	}
}
