use std::collections::HashMap;

use intern::Id;

pub struct FileGraph {
	files: HashMap<Id<str>, Vec<Id<str>>>,
}

impl FileGraph {
	pub fn new() -> Self { Self { files: HashMap::new() } }

	pub fn add_file(&mut self, file: Id<str>) { self.files.entry(file).or_default(); }

	pub fn add_child(&mut self, file: Id<str>, child: Id<str>) { self.files.entry(file).or_default().push(child); }

	pub fn get_children(&self, file: Id<str>) -> &[Id<str>] {
		self.files.get(&file).map(|v| v.as_slice()).unwrap_or(&[])
	}
}
