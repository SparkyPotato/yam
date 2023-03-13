#![feature(type_alias_impl_trait)]

use std::{future::Future, hash::Hash, sync::Mutex};

use rustc_hash::FxHashSet;
use tokio::{
	runtime::{Builder, Runtime},
	task::{AbortHandle, JoinHandle},
};
pub use verde_derive::{db, query, storage, Tracked};

use crate::storage::{
	routing::{Route, RouteBuilder, RoutingTable, RoutingTableBuilder},
	ErasedId,
	ErasedQueryStorage,
	ErasedTrackedStorage,
	Get,
};
pub use crate::storage::{Id, QueryStorage, TrackedStorage};

pub mod storage;

/// An asynchronously running query.
pub struct PendingQuery<T> {
	handle: JoinHandle<Id<T>>,
}

pub struct DbForQuery<'a> {
	pub db: &'a dyn Db,
	pub dependencies: Mutex<Option<FxHashSet<ErasedId>>>,
}

/// A database. This trait provides most of the functionality of the concrete database type.
pub trait Db: Send + Sync {
	fn core(&self) -> &DatabaseCore;

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
		self.core().cancel_all();

		let fut = (self as &dyn Db).insert(Route::input(), value);
		self.core().rt.block_on(fut)
	}

	fn get_ext<T: Tracked>(&self, id: Id<T>) -> Get<'_, T>
	where
		Self: Sized,
	{
		let fut = (self as &dyn Db).get_inner(id);
		self.core().rt.block_on(fut)
	}

	fn execute<Q: Query>(&self, fut: Q::Future) -> PendingQuery<Q::Output>
	where
		Self: Sized,
	{
		let handle = self.core().rt.spawn(fut);
		self.core().pending_queries.lock().unwrap().push(handle.abort_handle());
		PendingQuery { handle }
	}

	fn block_on<T>(&self, pending: PendingQuery<T>) -> Id<T>
	where
		Self: Sized,
	{
		match self.core().rt.block_on(pending.handle) {
			Ok(v) => v,
			Err(err) => {
				if err.is_panic() {
					std::panic::resume_unwind(err.into_panic());
				} else {
					panic!("query was aborted");
				}
			},
		}
	}
}

type EndQueryFutureInner<'a, 'b, T: Query, F: Future<Output = T::Output> + 'b> =
	impl Future<Output = Id<T::Output>> + 'b;
pub struct EndQueryFuture<'a, 'b, T: Query, F: Future<Output = T::Output> + 'b>(EndQueryFutureInner<'a, 'b, T, F>);
unsafe impl<T: Query, F: Future<Output = T::Output>> Send for EndQueryFuture<'_, '_, T, F> {}
impl<'a, 'b, T: Query, F: Future<Output = T::Output> + 'a> Future for EndQueryFuture<'a, 'b, T, F> {
	type Output = Id<T::Output>;

	fn poll(self: std::pin::Pin<&mut Self>, cx: &mut std::task::Context<'_>) -> std::task::Poll<Self::Output> {
		let this = unsafe { self.get_unchecked_mut() };
		unsafe { std::pin::Pin::new_unchecked(&mut this.0) }.poll(cx)
	}
}

type GetFutureInner<'a, T: Tracked> = impl Future<Output = Get<'a, T>> + 'a;
pub struct GetFuture<'a, T: Tracked>(GetFutureInner<'a, T>);
unsafe impl<'a, T: Tracked> Send for GetFuture<'a, T> {}
impl<'a, T: Tracked> Future for GetFuture<'a, T> {
	type Output = Get<'a, T>;

	fn poll(self: std::pin::Pin<&mut Self>, cx: &mut std::task::Context<'_>) -> std::task::Poll<Self::Output> {
		let this = unsafe { self.get_unchecked_mut() };
		unsafe { std::pin::Pin::new_unchecked(&mut this.0) }.poll(cx)
	}
}

impl dyn Db + '_ {
	pub fn get<T: Tracked>(&self, id: Id<T>) -> GetFuture<'_, T> {
		GetFuture(async move {
			self.register_dependency(id.inner);
			self.get_inner(id).await
		})
	}

	pub fn end_query<'a, 'b, T, F>(
		&'a self, input: T::Input, ctx: &'b DbForQuery<'a>, fut: F,
	) -> EndQueryFuture<'a, 'b, T, F>
	where
		T: Query,
		F: Future<Output = T::Output> + 'b,
	{
		EndQueryFuture(async move {
			let query = self.routing_table().route::<T>();
			let storage = self.storage_struct(query.storage).query_storage(query.index).unwrap();
			unsafe { storage.execute::<T, F>(query, input, ctx, fut).await }
		})
	}

	async fn get_inner<T: Tracked>(&self, id: Id<T>) -> Get<'_, T> {
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
	/// The future type returned by the query.
	type Future: Future<Output = Id<Self::Output>> + Send + 'static;
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
	rt: Runtime,
	pending_queries: Mutex<Vec<AbortHandle>>,
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
	fn cancel_all(&self) {
		let mut pending_queries = self.pending_queries.lock().unwrap();
		for handle in pending_queries.drain(..) {
			handle.abort();
		}
	}
}

impl Drop for DatabaseCore {
	fn drop(&mut self) { self.cancel_all(); }
}

pub struct DbWrapper(pub *const dyn Db);
impl DbWrapper {
	pub unsafe fn to_ref(&self) -> &dyn Db { unsafe { &*self.0 } }
}
unsafe impl Send for DbWrapper {}
