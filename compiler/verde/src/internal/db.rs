use std::{marker::PhantomData, pin::Pin, sync::Mutex};

use rustc_hash::FxHashSet;
use tokio::{
	runtime::{Builder, Runtime},
	task::AbortHandle,
};

use crate::{
	internal::{
		storage::{ErasedId, ErasedQueryId, Get, Route, RoutingTable, RoutingTableBuilder},
		Storage,
	},
	span,
	Db,
	Id,
	Instrument,
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
			rt: Builder::new_multi_thread().enable_all().build().unwrap(),
			pending_queries: Mutex::new(Vec::new()),
		}
	}
}

impl DatabaseCore {
	pub(crate) fn cancel_all(&self) {
		span!(enter debug, "cancelling all queries");

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
	pub curr_query: ErasedQueryId,
}

impl dyn Db + '_ {
	pub(crate) async fn get_inner<T: Tracked>(&self, id: Id<T>) -> Get<'_, T> {
		async move {
			let storage = self
				.storage_struct(id.inner.route.storage)
				.tracked_storage(id.inner.route.index)
				.unwrap();
			unsafe { storage.get(id.inner.index).await }
		}
		.instrument(span!(
			trace,
			"fetching value",
			ty = std::any::type_name::<T>(),
			id = id.inner.index
		))
		.await
	}

	pub(crate) async fn insert<T: Tracked>(&self, query: Route, value: T, target_gen: Option<u64>) -> Id<T> {
		let span = span!(
			trace,
			"inserting value",
			ty = std::any::type_name::<T>(),
			id = tracing::field::Empty
		);
		let (route, storage) = span.in_scope(|| {
			let route = self.routing_table().route::<T>();
			let storage = self.storage_struct(route.storage).tracked_storage(route.index).unwrap();
			(route, storage)
		});
		let id = unsafe { storage.insert(value, query, target_gen).await };
		span.record("id", id);
		Id::new(id, route)
	}

	pub(crate) async fn get_generation<T: Tracked>(&self, id: Id<T>) -> u64 {
		async move {
			let storage = self
				.storage_struct(id.inner.route.storage)
				.tracked_storage(id.inner.route.index)
				.unwrap();
			unsafe { storage.get_generation::<T>(id.inner.index).await }
		}
		.instrument(span!(
			trace,
			"fetching generation",
			ty = std::any::type_name::<T>(),
			id = id.inner.index
		))
		.await
	}

	pub(crate) async fn get_generation_erased(&self, id: ErasedId) -> u64 {
		async move {
			let storage = self
				.storage_struct(id.route.storage)
				.tracked_storage(id.route.index)
				.unwrap();
			storage.get_erased_generation(id.index).await
		}
		.instrument(span!(
			trace,
			"fetching generation",
			ty = self.routing_table().name(id.route),
			id = id.index
		))
		.await
	}
}

impl Db for DbForQuery<'_> {
	fn register_dependency(&self, id: ErasedId) {
		span!(
			enter debug, "registering dependency",
			query = self.routing_table().name(self.curr_query.route),
			ty = self.routing_table().name(id.route),
			id = id.index
		);
		self.dependencies.lock().unwrap().as_mut().unwrap().insert(id);
	}

	fn get_current_query_id(&self) -> ErasedQueryId { self.curr_query }

	fn parent_db(&self) -> &dyn Db { self.db }

	fn new() -> Pin<Box<Self>>
	where
		Self: Sized,
	{
		panic!("Invalid method called on `DbForQuery`")
	}

	fn builder() -> DbBuilder<Self>
	where
		Self: Sized,
	{
		panic!("Invalid method called on `DbForQuery`")
	}

	fn build_with_core(_: DatabaseCore) -> Pin<Box<Self>>
	where
		Self: Sized,
	{
		panic!("Invalid method called on `DbForQuery`")
	}

	fn init_routing(_: &mut RoutingTableBuilder) { panic!("Invalid method called on `DbForQuery`") }

	#[cfg(feature = "serde")]
	fn serialize<S: serde::Serializer>(self, _: S) -> Result<S::Ok, S::Error>
	where
		Self: Sized,
	{
		panic!("Invalid method called on `DbForQuery`")
	}

	#[cfg(feature = "serde")]
	fn deserialize_with_core<'de, D: serde::Deserializer<'de>>(
		_: DatabaseCore, _: D,
	) -> Result<Pin<Box<Self>>, D::Error>
	where
		Self: Sized,
	{
		panic!("Invalid method called on `DbForQuery`");
	}

	fn core(&self) -> &DatabaseCore { self.db.core() }

	fn routing_table(&self) -> &RoutingTable { self.db.routing_table() }

	fn storage_struct(&self, storage: u16) -> &dyn Storage { self.db.storage_struct(storage) }

	fn shutdown(&mut self) {
		panic!("Invalid method called on `DbForQuery`");
	}
}

pub struct DbBuilder<T> {
	builder: Builder,
	_phantom: PhantomData<T>,
}

impl<T: Db> DbBuilder<T> {
	pub fn new() -> Self {
		let mut builder = Builder::new_multi_thread();
		builder.thread_name("tango-worker");
		Self {
			builder,
			_phantom: PhantomData,
		}
	}

	pub fn thread_name(&mut self, name: impl Into<String>) -> &mut Self {
		self.builder.thread_name(name);
		self
	}

	/// Sets the number of worker threads for the runtime.
	/// If not set, the number of logical cores on the system will be used.
	/// If the value is 0, it will panic.
	pub fn worker_threads(&mut self, threads: usize) -> &mut Self {
		self.builder.worker_threads(threads);
		self
	}

	pub fn build(&mut self) -> Pin<Box<T>> { T::build_with_core(self.make_core()) }

	#[cfg(feature = "serde")]
	pub fn deserialize<'de, D: serde::Deserializer<'de>>(&mut self, deserializer: D) -> Result<Pin<Box<T>>, D::Error> {
		let core = self.make_core();
		span!(enter trace, "deserializing database");
		T::deserialize_with_core(core, deserializer)
	}

	fn make_core(&mut self) -> DatabaseCore {
		span!(enter trace, "initializing database");
		DatabaseCore {
			rt: self.builder.build().unwrap(),
			pending_queries: Mutex::new(Vec::new()),
		}
	}
}

impl<T: Db> Db for Pin<Box<T>> {
	fn parent_db(&self) -> &dyn Db { self.as_ref().get_ref().parent_db() }

	fn build_with_core(_: DatabaseCore) -> Pin<Box<Self>>
	where
		Self: Sized,
	{
		panic!("Invalid method called on `Pin<Box<Db>>`");
	}

	#[cfg(feature = "serde")]
	fn deserialize_with_core<'de, D: serde::Deserializer<'de>>(
		_: DatabaseCore, _: D,
	) -> Result<Pin<Box<Self>>, D::Error>
	where
		Self: Sized,
	{
		panic!("Invalid method called on `Pin<Box<Db>>`");
	}

	fn init_routing(_: &mut RoutingTableBuilder)
	where
		Self: Sized,
	{
		panic!("Invalid method called on `Pin<Box<Db>>`");
	}

	#[cfg(feature = "serde")]
	fn serialize<S: serde::Serializer>(mut self, serializer: S) -> Result<S::Ok, S::Error> {
		span!(enter trace, "serializing database");
		self.shutdown();
		// SAFETY: We don't need to be pinned anymore.
		let inner = unsafe { Pin::into_inner_unchecked(self) };
		(*inner).serialize(serializer)
	}

	fn core(&self) -> &DatabaseCore { self.as_ref().get_ref().core() }

	fn routing_table(&self) -> &RoutingTable { self.as_ref().get_ref().routing_table() }

	fn storage_struct(&self, storage: u16) -> &dyn Storage { self.as_ref().get_ref().storage_struct(storage) }

	fn shutdown(&mut self) {
		span!(enter trace, "shutting down database");
		unsafe {
			self.as_mut().get_unchecked_mut().shutdown();
		}
	}
}
