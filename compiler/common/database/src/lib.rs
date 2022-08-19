#[salsa::db(ast::Jar)]
#[derive(Default)]
pub struct Database {
	storage: salsa::Storage<Self>,
}

impl salsa::Database for Database {
	fn salsa_runtime(&self) -> &salsa::Runtime { self.storage.runtime() }
}

impl salsa::ParallelDatabase for Database {
	fn snapshot(&self) -> salsa::Snapshot<Self> {
		salsa::Snapshot::new(Database {
			storage: self.storage.snapshot(),
		})
	}
}
