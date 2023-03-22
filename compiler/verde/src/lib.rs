use std::{future::Future, hash::Hash, pin::Pin, sync::Mutex};

#[cfg(feature = "serde")]
pub use serde;
use tokio::task::JoinHandle;
pub use verde_derive::{db, query, storage, Pushable, Tracked};

#[cfg(feature = "serde")]
pub use crate::internal::serde::*;
pub use crate::{cursed::DbWrapper, internal::storage::Id};
use crate::{
	cursed::{EndQueryFuture, GetFuture, PushFuture, StartQueryFuture},
	internal::{
		storage::{
			ErasedId,
			ErasedQueryId,
			Get,
			PushableStorage,
			Route,
			RoutingTable,
			RoutingTableBuilder,
			TrackedStorage,
		},
		DatabaseCore,
		DbBuilder,
		DbForQuery,
		Query,
		Storable,
		Storage,
	},
};

mod cursed;
pub mod internal;

/// A database. This trait provides most of the functionality of the concrete database type.
pub trait Db: Send + Sync {
	/// Set an input value. This will cancel all asynchronously running queries.
	fn set_input<T: Tracked>(&mut self, value: T) -> Id<T>
	where
		Self: Sized,
	{
		self.core().cancel_all();

		let fut = (self as &dyn Db).insert(Route::input(), value, None);
		self.core().rt.block_on(fut)
	}

	fn get_ext<T: Tracked>(&self, id: Id<T>) -> Get<'_, T>
	where
		Self: Sized,
	{
		let fut = (self as &dyn Db).get_inner(id);
		self.core().rt.block_on(fut)
	}

	fn get_all<T: Pushable>(&self) -> Vec<T>
	where
		Self: Sized,
	{
		let fut = async move {
			let route = self.routing_table().route::<T>();
			let storage = self
				.storage_struct(route.storage)
				.pushable_storage(route.index)
				.unwrap();
			unsafe { storage.get_all().await }
		};
		self.core().rt.block_on(fut)
	}

	fn execute<F, T>(&self, query: F) -> PendingQuery<T>
	where
		Self: Sized,
		F: Future<Output = Id<T>> + Send + 'static,
		T: 'static,
	{
		let handle = self.core().rt.spawn(query);
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

	/// Register a dependency of the currently executing query.
	fn register_dependency(&self, id: ErasedId) { let _ = id; }

	/// Get the query ID of the currently executing query.
	fn get_current_query_id(&self) -> ErasedQueryId {
		panic!("Cannot get query ID from main database");
	}

	/// Get the parent database.
	fn parent_db(&self) -> &dyn Db;

	fn new() -> Pin<Box<Self>>
	where
		Self: Sized,
	{
		Self::builder().build()
	}

	fn builder() -> DbBuilder<Self>
	where
		Self: Sized,
	{
		DbBuilder::new()
	}

	fn build_with_core(core: DatabaseCore) -> Pin<Box<Self>>
	where
		Self: Sized;

	#[cfg(feature = "serde")]
	fn deserialize_with_core<'de, D: serde::Deserializer<'de>>(
		core: DatabaseCore, deserializer: D,
	) -> Result<Pin<Box<Self>>, D::Error>
	where
		Self: Sized;

	/// Initialize the routing table at database initialization.
	fn init_routing(table: &mut RoutingTableBuilder)
	where
		Self: Sized;

	#[cfg(feature = "serde")]
	fn serialize<S: serde::Serializer>(self, serializer: S) -> Result<S::Ok, S::Error>
	where
		Self: Sized;

	fn core(&self) -> &DatabaseCore;

	fn routing_table(&self) -> &RoutingTable;

	/// Get the storage struct with route index `storage`.
	fn storage_struct(&self, storage: u16) -> &dyn Storage;

	fn shutdown(&mut self);
}

/// An asynchronously running query.
pub struct PendingQuery<T> {
	handle: JoinHandle<Id<T>>,
}

/// A type that can be tracked by the database.
///
/// Can be automatically derived using the `#[derive(Tracked)]` attribute. Use `#[id]` on a field to
/// specify the field that uniquely identifies each tracked instance.
pub trait Tracked: Eq + Storable<Storage = TrackedStorage<Self>> {
	type Id: Eq + Hash + Clone + Send + Sync;

	fn id(&self) -> &Self::Id;
}

pub trait Pushable: Clone + Storable<Storage = PushableStorage<Self>> {}

type StartQueryFutureInner<'a, T: Query> = impl Future<Output = DbForQuery<'a>> + 'a;
type EndQueryFutureInner<'a, 'b, T: Query, F: Future<Output = T::Output> + 'b> =
	impl Future<Output = Id<T::Output>> + 'b;
type GetFutureInner<'a, T: Tracked> = impl Future<Output = Get<'a, T>> + 'a;
type PushFutureInner<'a, T: Pushable> = impl Future<Output = ()> + 'a;

impl dyn Db + '_ {
	pub fn get<T: Tracked>(&self, id: Id<T>) -> GetFuture<'_, T> {
		GetFuture(async move {
			self.register_dependency(id.inner);
			self.get_inner(id).await
		})
	}

	pub fn push<T: Pushable>(&self, value: T) -> PushFuture<T> {
		PushFuture(async move {
			let span = span!(debug, "push", ty = std::any::type_name::<T>());
			let (query, storage) = span.in_scope(|| {
				let query = self.get_current_query_id();
				let route = self.routing_table().route::<T>();
				let storage = self
					.storage_struct(route.storage)
					.pushable_storage(route.index)
					.unwrap();
				(query, storage)
			});
			unsafe {
				storage.push(query, value).await;
			}
		})
	}

	pub fn start_query<T: Query>(&self, input: T::Input) -> StartQueryFuture<T> {
		StartQueryFuture(
			async move {
				let route = self.routing_table().route::<T>();
				let storage = self.storage_struct(route.storage).query_storage(route.index).unwrap();
				unsafe {
					let index = storage.start_query::<T>(input).await;
					let curr_query = ErasedQueryId { route, index };
					async move {
						for route in self.routing_table().pushables() {
							let storage = self
								.storage_struct(route.storage)
								.pushable_storage(route.index)
								.unwrap();
							storage.clear(curr_query).await;
						}
					}
					.instrument(span!(trace, "clear pushables", query = std::any::type_name::<T>()))
					.await;

					DbForQuery {
						db: self.parent_db(),
						dependencies: Mutex::new(Some(Default::default())),
						curr_query,
					}
				}
			}
			.instrument(span!(trace, "initialize query", query = std::any::type_name::<T>())),
		)
	}

	pub fn end_query<'a, 'b, T, F>(&'a self, ctx: &'b DbForQuery<'a>, fut: F) -> EndQueryFuture<'a, 'b, T, F>
	where
		T: Query,
		F: Future<Output = T::Output> + 'b,
	{
		EndQueryFuture(async move {
			let query = self.routing_table().route::<T>();
			let storage = self.storage_struct(query.storage).query_storage(query.index).unwrap();
			unsafe { storage.execute::<T, F>(ctx, fut).await }
		})
	}
}

#[cfg(not(feature = "serde"))]
pub trait Serde {}

#[cfg(feature = "tracing")]
use tracing_futures::Instrument;

#[cfg(not(feature = "tracing"))]
trait Instrument {
	fn instrument(self, _: Span) -> Self;
}

#[cfg(not(feature = "tracing"))]
impl<T> Instrument for T {
	fn instrument(self, _: Span) -> Self { self }
}

#[cfg(feature = "tracing")]
macro_rules! span {
	(enter $($x:tt)*) => {
		let _e = crate::span!($($x)*).entered();
	};

	(trace, $($x:tt)*) => {
		tracing::span!(tracing::Level::TRACE, $($x)*)
	};
    (debug, $($x:tt)*) => {
		tracing::span!(tracing::Level::DEBUG, $($x)*)
	};
	(info, $($x:tt)*) => {
		tracing::span!(tracing::Level::INFO, $($x)*)
	};
	(warn, $($x:tt)*) => {
		tracing::span!(tracing::Level::WARN, $($x)*)
	};
	(error, $($x:tt)*) => {
		tracing::span!(tracing::Level::ERROR, $($x)*)
	};
}

#[cfg(not(feature = "tracing"))]
macro_rules! span {
	($($x:tt)*) => {{
		let x = crate::Span;
		x
	}};
}

use span;

#[cfg(feature = "tracing")]
macro_rules! event {
	(trace, $($x:tt)*) => {
		tracing::event!(tracing::Level::TRACE, $($x)*)
	};
    (debug, $($x:tt)*) => {
		tracing::event!(tracing::Level::DEBUG, $($x)*)
	};
	(info, $($x:tt)*) => {
		tracing::event!(tracing::Level::INFO, $($x)*)
	};
	(warn, $($x:tt)*) => {
		tracing::event!(tracing::Level::WARN, $($x)*)
	};
	(error, $($x:tt)*) => {
		tracing::event!(tracing::Level::ERROR, $($x)*)
	};
}

#[cfg(not(feature = "tracing"))]
macro_rules! event {
	($($x:tt)*) => {
		()
	};
}

use event;

#[cfg(not(feature = "tracing"))]
struct Span;

#[cfg(not(feature = "tracing"))]
impl Span {
	fn record<Q: ?Sized, V>(&self, _: &Q, _: V) -> &Self { self }

	fn in_scope<F: FnOnce() -> T, T>(&self, f: F) -> T { f() }
}
