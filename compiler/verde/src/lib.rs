#![feature(allocator_api)]
#![feature(ptr_metadata)]
#![feature(strict_provenance)]

use std::{cell::RefCell, future::Future, hash::Hash};

use futures::{
	executor::{block_on, ThreadPool},
	future::{AbortHandle, RemoteHandle},
};
use rustc_hash::FxHashSet;
pub use verde_derive::{db, query, storage, Tracked};

pub use crate::storage::{Id, QueryStorage, TrackedStorage};
use crate::{
	arena::Arena,
	storage::{
		routing::{Route, RouteBuilder, RoutingTable, RoutingTableBuilder},
		ErasedId,
		ErasedQueryStorage,
		ErasedTrackedStorage,
		Get,
	},
};

mod arena;
pub mod storage;

/// An asynchronously running query.
pub struct PendingQuery<T> {
	handle: RemoteHandle<T>,
}

pub struct DbForQuery<'a> {
	pub db: &'a dyn Db,
	pub dependencies: RefCell<Option<FxHashSet<ErasedId>>>,
}

/// A database. This trait provides most of the functionality of the concrete database type.
pub trait Db {
	fn core(&self) -> &DatabaseCore;

	fn core_mut(&mut self) -> &mut DatabaseCore;

	fn routing_table(&self) -> &RoutingTable;

	/// Get the storage struct with route index `storage`.
	fn storage_struct(&self, storage: u16) -> &dyn Storage;

	/// Register a dependency of the currently executing query.
	fn register_dependency(&self, id: ErasedId) { let _ = id; }

	/// Get a derived database for a query.
	fn start_query(&self) -> DbForQuery<'_>;

	/// Initialize the routing table at database initialization.
	fn init_routing(table: &mut RoutingTableBuilder)
	where
		Self: Sized;

	/// Set an input value. This will cancel all asynchronously running queries.
	fn set_input<T: Tracked>(&mut self, value: T) -> Id<T>
	where
		Self: Sized,
	{
		for handle in self.core_mut().pending_queries.drain(..) {
			handle.abort();
		}

		block_on((self as &dyn Db).insert(Route::input(), value))
	}

	fn get<T: Tracked>(&mut self, id: Id<T>) -> Get<'_, T>
	where
		Self: Sized,
	{
		let fut = (self as &dyn Db).get(id);
		block_on(fut)
	}

	fn execute<F: Future>(&self, fut: F) -> F::Output
	where
		Self: Sized,
	{
		block_on(fut)
	}
}

impl<'a> dyn Db + 'a {
	pub async fn get<T: Tracked>(&self, id: Id<T>) -> Get<'_, T> {
		self.register_dependency(id.inner);
		let storage = self
			.storage_struct(id.inner.route.storage)
			.tracked_storage(id.inner.route.index)
			.unwrap();
		unsafe { storage.get(id.inner.index).await }
	}

	async fn insert<T: Tracked>(&self, query: Route, value: T) -> Id<T> {
		let route = self.routing_table().route::<T>();
		let storage = self.storage_struct(route.storage).tracked_storage(route.index).unwrap();
		let id = unsafe { storage.insert(value, query).await };
		Id::new(id, route)
	}

	async fn get_generation<T: Tracked>(&self, id: Id<T>) -> u64 {
		let storage = self
			.storage_struct(id.inner.route.storage)
			.tracked_storage(id.inner.route.index)
			.unwrap();
		unsafe { storage.get_generation::<T>(id.inner.index).await }
	}

	async fn get_generation_erased(&self, id: ErasedId) -> u64 {
		let storage = self
			.storage_struct(id.route.storage)
			.tracked_storage(id.route.index)
			.unwrap();
		storage.get_erased_generation(self.core(), id.index).await
	}

	pub async fn end_query<T, F>(&self, input: T::Input, ctx: &DbForQuery<'_>, fut: F) -> Id<T::Output>
	where
		T: Query,
		F: Future<Output = T::Output>,
	{
		let query = self.routing_table().route::<T>();
		let storage = self.storage_struct(query.storage).query_storage(query.index).unwrap();
		unsafe { storage.execute::<T, F>(query, input, ctx, fut).await }
	}
}

impl Db for DbForQuery<'_> {
	fn core(&self) -> &DatabaseCore { self.db.core() }

	fn core_mut(&mut self) -> &mut DatabaseCore { panic!("Invalid method called on `DbForQuery`") }

	fn routing_table(&self) -> &RoutingTable { self.db.routing_table() }

	fn storage_struct(&self, storage: u16) -> &dyn Storage { self.db.storage_struct(storage) }

	fn register_dependency(&self, id: ErasedId) { self.dependencies.borrow_mut().as_mut().unwrap().insert(id); }

	fn start_query(&self) -> DbForQuery<'_> {
		DbForQuery {
			db: self.db,
			dependencies: RefCell::new(Some(Default::default())),
		}
	}

	fn init_routing(_: &mut RoutingTableBuilder) { panic!("Invalid method called on `DbForQuery`") }
}

/// A type that can be tracked by the database.
///
/// Can be automatically derived using the `#[derive(Tracked)]` attribute. Use `#[id]` on a field to
/// specify the field that uniquely identifies each tracked instance.
pub trait Tracked: Eq + TrackedOrQuery<ToStore = TrackedStorage<Self>> {
	type Id: Eq + Hash + Clone + Send + Sync;

	fn id(&self) -> &Self::Id;
}

/// A query that can execute on the database.
///
/// Can be automatically derived using the `#[query]` attribute on an `async fn`
pub trait Query: TrackedOrQuery<ToStore = QueryStorage<Self>> {
	/// The inputs to the query. The query is a pure function of these inputs.
	type Input: Clone + Eq + Hash + Send + Sync;
	/// The output of the query.
	type Output: Tracked + Send + Sync;
}

/// A type that is either a [`Tracked`] struct or a query.
/// Types that implement this trait can be stored inside `Storage`.
pub trait TrackedOrQuery: Sized + Send + Sync + 'static {
	/// The type that should actually be stored inside the `Storage`.
	/// If the type is a `Tracked` struct, this should be [`TrackedStorage<Self>`].
	/// If the type is a query, this should be [`QueryStorage<Self>`].
	type ToStore: Default;

	/// Cast to a `&dyn TrackedStorage<Self>` if `Self` is a tracked struct.
	fn tracked_storage(store: &Self::ToStore) -> Option<&dyn ErasedTrackedStorage>;

	/// Cast to a `&dyn QueryStorage<Self>` if `Self` is a query.
	fn query_storage(store: &Self::ToStore) -> Option<&dyn ErasedQueryStorage>;
}

/// A struct that contains [`TrackedStorage<T>`] or [`QueryStorage<T>`]
/// depending on whether `T` is a tracked struct or a query.
pub trait StorageOf<T: TrackedOrQuery> {
	/// Get the route index of `T::ToStore` in this storage struct.
	fn storage_index(&self) -> u16;
}

/// A struct that contains type storages. Must be implemented in conjunction with [`StorageOf<T>`]
pub trait Storage {
	fn init_routing(table: &mut RouteBuilder)
	where
		Self: Sized;

	/// Get a `&dyn TrackedStorage<T>` if the route with `index` is a tracked struct.
	fn tracked_storage(&self, index: u16) -> Option<&dyn ErasedTrackedStorage>;

	/// Get a `&dyn QueryStorage<T>` if the route with `index` is a query.
	fn query_storage(&self, index: u16) -> Option<&dyn ErasedQueryStorage>;
}

/// A database that contains a storage struct `S`,
/// implementing [`StorageOf<T>`] for each type `T` stored in `S`.
pub trait DbWith<S> {
	/// The route index of `S` in this database.
	fn storage_struct_index(&self) -> u16;
}

/// The core of the database runtime. This is stored inside your custom database struct, alongside the type storages.
pub struct DatabaseCore {
	pool: ThreadPool,
	arena: Arena,
	pending_queries: Vec<AbortHandle>,
}

impl Default for DatabaseCore {
	fn default() -> Self {
		Self {
			pool: ThreadPool::builder().name_prefix("tango-worker-").create().unwrap(),
			arena: Arena::new(),
			pending_queries: Vec::new(),
		}
	}
}
