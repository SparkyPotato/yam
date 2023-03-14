use std::sync::Mutex;

use rustc_hash::FxHashSet;
use tokio::{
	runtime::{Builder, Runtime},
	task::AbortHandle,
};

use crate::{
	internal::{
		storage::{ErasedId, Get, Route, RoutingTable, RoutingTableBuilder},
		Storage,
	},
	Db,
	Id,
	Tracked,
};

/// The core of the database runtime. This is stored inside your custom database struct, alongside the type storages.
pub struct DatabaseCore {
	pub(crate) rt: Runtime,
	pub(crate) pending_queries: Mutex<Vec<AbortHandle>>,
}

impl Default for DatabaseCore {
	fn default() -> Self {
		Self {
			rt: Builder::new_multi_thread().thread_name("tango-worker").build().unwrap(),
			pending_queries: Mutex::new(Vec::new()),
		}
	}
}

impl DatabaseCore {
	pub(crate) fn cancel_all(&self) {
		let mut pending_queries = self.pending_queries.lock().unwrap();
		for handle in pending_queries.drain(..) {
			handle.abort();
		}
	}
}

impl Drop for DatabaseCore {
	fn drop(&mut self) { self.cancel_all(); }
}

pub struct DbForQuery<'a> {
	pub db: &'a dyn Db,
	pub dependencies: Mutex<Option<FxHashSet<ErasedId>>>,
}

impl dyn Db + '_ {
	pub(crate) async fn get_inner<T: Tracked>(&self, id: Id<T>) -> Get<'_, T> {
		let storage = self
			.storage_struct(id.inner.route.storage)
			.tracked_storage(id.inner.route.index)
			.unwrap();
		unsafe { storage.get(id.inner.index).await }
	}

	pub(crate) async fn insert<T: Tracked>(&self, query: Route, value: T) -> Id<T> {
		let route = self.routing_table().route::<T>();
		let storage = self.storage_struct(route.storage).tracked_storage(route.index).unwrap();
		let id = unsafe { storage.insert(value, query).await };
		Id::new(id, route)
	}

	pub(crate) async fn get_generation<T: Tracked>(&self, id: Id<T>) -> u64 {
		let storage = self
			.storage_struct(id.inner.route.storage)
			.tracked_storage(id.inner.route.index)
			.unwrap();
		unsafe { storage.get_generation::<T>(id.inner.index).await }
	}

	pub(crate) async fn get_generation_erased(&self, id: ErasedId) -> u64 {
		let storage = self
			.storage_struct(id.route.storage)
			.tracked_storage(id.route.index)
			.unwrap();
		storage.get_erased_generation(id.index).await
	}
}

impl Db for DbForQuery<'_> {
	fn core(&self) -> &DatabaseCore { self.db.core() }

	fn routing_table(&self) -> &RoutingTable { self.db.routing_table() }

	fn storage_struct(&self, storage: u16) -> &dyn Storage { self.db.storage_struct(storage) }

	fn register_dependency(&self, id: ErasedId) { self.dependencies.lock().unwrap().as_mut().unwrap().insert(id); }

	fn start_query(&self) -> DbForQuery<'_> {
		DbForQuery {
			db: self.db,
			dependencies: Mutex::new(Some(Default::default())),
		}
	}

	fn init_routing(_: &mut RoutingTableBuilder) { panic!("Invalid method called on `DbForQuery`") }
}
