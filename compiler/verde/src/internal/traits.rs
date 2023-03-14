use std::{future::Future, hash::Hash};

use crate::{
	internal::storage::{ErasedQueryStorage, ErasedTrackedStorage, QueryStorage, RouteBuilder},
	Id,
	Tracked,
};

/// A struct that contains [`TrackedStorage<T>`] or [`QueryStorage<T>`]
/// depending on whether `T` is a tracked struct or a query.
pub trait StorageOf<T: Storable> {
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

/// A query that can execute on the database.
///
/// Can be automatically derived using the `#[query]` attribute on an `async fn`
pub trait Query: Storable<Storage = QueryStorage<Self>> {
	/// The inputs to the query. The query is a pure function of these inputs.
	type Input: Clone + Eq + Hash + Send + Sync;
	/// The output of the query.
	type Output: Tracked + Send + Sync;
	/// The future type returned by the query.
	type Future: Future<Output = Id<Self::Output>> + Send + 'static;
}

/// A type that is either a [`Tracked`] struct or a query.
/// Types that implement this trait can be stored inside `Storage`.
pub trait Storable: Sized + Send + Sync + 'static {
	/// The type that should actually be stored inside the `Storage`.
	/// If the type is a `Tracked` struct, this should be [`TrackedStorage<Self>`].
	/// If the type is a query, this should be [`QueryStorage<Self>`].
	type Storage: Default;

	/// Cast to a `&dyn TrackedStorage<Self>` if `Self` is a tracked struct.
	fn tracked_storage(store: &Self::Storage) -> Option<&dyn ErasedTrackedStorage>;

	/// Cast to a `&dyn QueryStorage<Self>` if `Self` is a query.
	fn query_storage(store: &Self::Storage) -> Option<&dyn ErasedQueryStorage>;
}
