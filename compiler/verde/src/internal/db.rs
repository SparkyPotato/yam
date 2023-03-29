use std::{cell::RefCell, mem::MaybeUninit};

use rustc_hash::FxHashSet;

use crate::{
	internal::{
		storage::{interned, tracked, ErasedId, ErasedQueryId, Route, RoutingTable, RoutingTableBuilder},
		Query,
		Storage,
	},
	span,
	Id,
	Interned,
	Pushable,
	Tracked,
};

/// A database. This trait provides most of the functionality of the concrete database type.
pub trait Db {
	/// Initialize the routing table at database initialization.
	fn init_routing(table: &mut RoutingTableBuilder)
	where
		Self: Sized;

	fn routing_table(&self) -> &RoutingTable;

	/// Get the storage struct with route index `storage`.
	fn storage_struct(&self, storage: u16) -> &dyn Storage;
}

pub struct Ctx<'a> {
	pub db: &'a dyn Db,
	pub dependencies: RefCell<MaybeUninit<FxHashSet<(ErasedId, u64)>>>,
	pub curr_query: ErasedQueryId,
}

impl<'a> Ctx<'a> {
	fn new(db: &'a dyn Db, curr_query: ErasedQueryId) -> Self {
		Self {
			db,
			dependencies: RefCell::new(MaybeUninit::new(FxHashSet::default())),
			curr_query,
		}
	}

	pub fn get<T: Tracked>(&self, id: Id<T>) -> tracked::Get<'_, T> {
		span!(
			enter trace,
			"fetching value",
			ty = std::any::type_name::<T>(),
			id = id.inner.index
		);
		unsafe {
			let gen = self.get_generation(id.inner);
			self.dependencies
				.try_borrow_mut()
				.expect("Cannot call `get` within a `map` scope")
				.assume_init_mut()
				.insert((id.inner, gen));
		}
		let storage = self
			.db
			.storage_struct(id.inner.route.storage)
			.tracked_storage(id.inner.route.index)
			.unwrap();
		unsafe { storage.get(id.inner.index) }
	}

	pub fn geti<T: Interned>(&self, id: Id<T>) -> interned::Get<'_, T> {
		span!(
			enter trace,
			"fetching value",
			ty = std::any::type_name::<T>(),
			id = id.inner.index
		);
		let storage = self
			.db
			.storage_struct(id.inner.route.storage)
			.interned_storage(id.inner.route.index)
			.unwrap();
		unsafe { storage.get(id.inner.index) }
	}

	pub fn add<T: Interned>(&self, value: T) -> Id<T> {
		let span = span!(
			trace,
			"inserting value",
			ty = std::any::type_name::<T>(),
			id = tracing::field::Empty
		);
		let _e = span.enter();
		let route = self.db.routing_table().route::<T>();
		let storage = self
			.db
			.storage_struct(route.storage)
			.interned_storage(route.index)
			.unwrap();
		let id = unsafe { storage.insert(value) };
		span.record("id", id);
		Id::new(id, route)
	}

	pub fn push<T: Pushable>(&self, value: T) {
		span!(enter debug, "push", ty = std::any::type_name::<T>());
		let route = self.db.routing_table().route::<T>();
		let storage = self
			.db
			.storage_struct(route.storage)
			.pushable_storage(route.index)
			.unwrap();
		unsafe {
			storage.push(self.curr_query, value);
		}
	}

	pub fn start_query<T: Query>(&self, input: T::Input) -> Ctx<'_> {
		span!(enter trace, "initialize query", query = std::any::type_name::<T>());

		let route = self.db.routing_table().route::<T>();
		let storage = self
			.db
			.storage_struct(route.storage)
			.query_storage(route.index)
			.unwrap();
		unsafe {
			let index = storage.start_query::<T>(input);
			let curr_query = ErasedQueryId { route, index };
			{
				span!(enter trace, "clear pushables", query = std::any::type_name::<T>());
				for route in self.db.routing_table().pushables() {
					let storage = self
						.db
						.storage_struct(route.storage)
						.pushable_storage(route.index)
						.unwrap();
					storage.clear(curr_query);
				}
			}

			Ctx::new(self.db, curr_query)
		}
	}

	pub fn end_query<T: Query>(&self, f: impl FnOnce() -> T::Output) -> Id<T::Output> {
		let query = self.db.routing_table().route::<T>();
		let storage = self
			.db
			.storage_struct(query.storage)
			.query_storage(query.index)
			.unwrap();
		unsafe { storage.execute::<T>(self, f) }
	}

	pub(crate) fn get_generation(&self, id: ErasedId) -> u64 {
		span!(
			enter trace,
			"fetching generation",
			ty = self.db.routing_table().name(id.route),
			id = id.index
		);
		let storage = self
			.db
			.storage_struct(id.route.storage)
			.tracked_storage(id.route.index)
			.unwrap();
		storage.get_generation(id.index)
	}
}

impl dyn Db + '_ {
	/// Set an input value. This will cancel all asynchronously running queries.
	pub fn set_input<T: Tracked>(&mut self, value: T) -> Id<T> { (self as &dyn Db).insert(Route::input(), value) }

	pub fn get<T: Tracked>(&self, id: Id<T>) -> tracked::Get<'_, T> {
		span!(
			enter trace,
			"fetching value",
			ty = std::any::type_name::<T>(),
			id = id.inner.index
		);
		let storage = self
			.storage_struct(id.inner.route.storage)
			.tracked_storage(id.inner.route.index)
			.unwrap();
		unsafe { storage.get(id.inner.index) }
	}

	pub fn geti<T: Interned>(&self, id: Id<T>) -> interned::Get<'_, T> {
		span!(
			enter trace,
			"fetching value",
			ty = std::any::type_name::<T>(),
			id = id.inner.index
		);
		let storage = self
			.storage_struct(id.inner.route.storage)
			.interned_storage(id.inner.route.index)
			.unwrap();
		unsafe { storage.get(id.inner.index) }
	}

	pub fn add<T: Interned>(&self, value: T) -> Id<T> {
		let span = span!(
			trace,
			"inserting value",
			ty = std::any::type_name::<T>(),
			id = tracing::field::Empty
		);
		let _e = span.enter();
		let route = self.routing_table().route::<T>();
		let storage = self
			.storage_struct(route.storage)
			.interned_storage(route.index)
			.unwrap();
		let id = unsafe { storage.insert(value) };
		span.record("id", id);
		Id::new(id, route)
	}

	pub fn get_all<T: Pushable>(&self) -> impl Iterator<Item = &'_ T> {
		let route = self.routing_table().route::<T>();
		let storage = self
			.storage_struct(route.storage)
			.pushable_storage(route.index)
			.unwrap();
		unsafe { storage.get_all() }
	}

	pub fn get_query<Q: Query, T: Pushable>(&self) -> impl Iterator<Item = &'_ T> {
		let route = self.routing_table().route::<T>();
		let query = self.routing_table().route::<Q>();
		let storage = self
			.storage_struct(route.storage)
			.pushable_storage(route.index)
			.unwrap();
		unsafe { storage.get_query(query) }
	}

	pub fn execute<R>(&self, f: impl FnOnce(&Ctx) -> R) -> R {
		let ctx = Ctx::new(
			self,
			ErasedQueryId {
				route: Route::input(),
				index: 0,
			},
		);
		f(&ctx)
	}
}

impl dyn Db + '_ {
	pub(crate) fn insert<T: Tracked>(&self, query: Route, value: T) -> Id<T> {
		let span = span!(
			trace,
			"inserting value",
			ty = std::any::type_name::<T>(),
			id = tracing::field::Empty
		);
		let _e = span.enter();
		let route = self.routing_table().route::<T>();
		let storage = self.storage_struct(route.storage).tracked_storage(route.index).unwrap();
		let id = unsafe { storage.insert(value, query) };
		span.record("id", id);
		Id::new(id, route)
	}
}
