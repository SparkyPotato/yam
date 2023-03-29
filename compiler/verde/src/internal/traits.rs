use std::hash::Hash;

use crate::internal::storage::{
	ErasedPushableStorage,
	ErasedQueryStorage,
	ErasedTrackedStorage,
	PushableStorage,
	QueryStorage,
	RouteBuilder,
	TrackedStorage,
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

	/// Get a `&dyn PushableStorage<T>` if the route with `index` is a storage struct.
	fn pushable_storage(&self, index: u16) -> Option<&dyn ErasedPushableStorage>;
}

/// A database that contains a storage struct `S`,
/// implementing [`StorageOf<T>`] for each type `T` stored in `S`.
pub trait DbWith<S> {
	/// The route index of `S` in this database.
	fn storage_struct_index(&self) -> u16;
}

/// A type that can be tracked by the database.
///
/// Can be automatically derived using the `#[derive(Tracked)]` attribute. Use `#[id]` on a field to
/// specify the field that uniquely identifies each tracked instance.
pub trait Tracked: Eq + Storable<Storage = TrackedStorage<Self>> {
	#[cfg(feature = "serde")]
	type Id: Eq + Hash + Clone + Send + Sync + serde::Serialize + for<'de> serde::Deserialize<'de>;

	#[cfg(not(feature = "serde"))]
	type Id: Eq + Hash + Clone + Send + Sync;

	fn id(&self) -> &Self::Id;
}

pub trait Pushable: Storable<Storage = PushableStorage<Self>> {}

/// A query that can execute on the database.
///
/// Can be automatically derived using the `#[query]` attribute on an `async fn`
pub trait Query: Storable<Storage = QueryStorage<Self>> {
	#[cfg(feature = "serde")]
	type Input: Eq + Hash + Send + Sync + serde::Serialize + for<'de> serde::Deserialize<'de>;

	#[cfg(not(feature = "serde"))]
	type Input: Eq + Hash + Send + Sync;

	type Output: Tracked + Send + Sync;
}

/// A type that is either a [`Tracked`] struct or a query.
/// Types that implement this trait can be stored inside `Storage`.
pub trait Storable: Sized + Send + 'static {
	/// The type that should actually be stored inside the `Storage`.
	/// If the type is a `Tracked` struct, this should be [`TrackedStorage<Self>`].
	/// If the type is a query, this should be [`QueryStorage<Self>`].
	type Storage: Default + extra::ExtraBound;

	const IS_PUSHABLE: bool;

	/// Cast to a `&dyn TrackedStorage<Self>` if `Self` is a tracked struct.
	fn tracked_storage(store: &Self::Storage) -> Option<&dyn ErasedTrackedStorage>;

	/// Cast to a `&dyn QueryStorage<Self>` if `Self` is a query.
	fn query_storage(store: &Self::Storage) -> Option<&dyn ErasedQueryStorage>;

	/// Get a `&dyn PushableStorage<T>` if the route with `index` is a storage struct.
	fn pushable_storage(store: &Self::Storage) -> Option<&dyn ErasedPushableStorage>;
}

#[cfg(feature = "test")]
mod extra {
	use crate::test::StorageType;

	pub trait ExtraBound: Into<StorageType> {}

	impl<T: Into<StorageType>> ExtraBound for T {}
}

#[cfg(not(feature = "test"))]
mod extra {
	pub trait ExtraBound {}

	impl<T> ExtraBound for T {}
}
