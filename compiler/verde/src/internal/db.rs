use std::{cell::RefCell, mem::MaybeUninit};

use rustc_hash::FxHashSet;

use crate::{
	internal::{
		storage::{ErasedId, ErasedQueryId, Get, Route, RoutingTable, RoutingTableBuilder},
		Query,
		Storage,
	},
	span,
	Id,
	Pushable,
	Tracked,
};

/// A database. This trait provides most of the functionality of the concrete database type.
pub trait Db {
	/// Set an input value. This will cancel all asynchronously running queries.
	fn set_input<T: Tracked>(&mut self, value: T) -> Id<T>
	where
		Self: Sized,
	{
		(self as &dyn Db).insert(Route::input(), value, None)
	}

	fn get<T: Tracked>(&self, id: Id<T>) -> Get<'_, T>
	where
		Self: Sized,
	{
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

	fn get_all<T: Pushable>(&self) -> Vec<T>
	where
		Self: Sized,
	{
		let route = self.routing_table().route::<T>();
		let storage = self
			.storage_struct(route.storage)
			.pushable_storage(route.index)
			.unwrap();
		unsafe { storage.get_all() }
	}

	fn execute<R>(&self, f: impl FnOnce(&Ctx) -> R) -> R
	where
		Self: Sized,
	{
		let ctx = Ctx::new(
			self,
			ErasedQueryId {
				route: Route::input(),
				index: 0,
			},
		);
		f(&ctx)
	}

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
	pub dependencies: RefCell<MaybeUninit<FxHashSet<ErasedId>>>,
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

	pub fn get<T: Tracked>(&self, id: Id<T>) -> Get<'_, T> {
		span!(
			enter trace,
			"fetching value",
			ty = std::any::type_name::<T>(),
			id = id.inner.index
		);
		unsafe {
			self.dependencies.borrow_mut().assume_init_mut().insert(id.inner);
		}
		let storage = self
			.db
			.storage_struct(id.inner.route.storage)
			.tracked_storage(id.inner.route.index)
			.unwrap();
		unsafe { storage.get(id.inner.index) }
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
		storage.get_erased_generation(id.index)
	}
}

impl dyn Db + '_ {
	pub(crate) fn insert<T: Tracked>(&self, query: Route, value: T, target_gen: Option<u64>) -> Id<T> {
		let span = span!(
			trace,
			"inserting value",
			ty = std::any::type_name::<T>(),
			id = tracing::field::Empty
		);
		let _e = span.enter();
		let route = self.routing_table().route::<T>();
		let storage = self.storage_struct(route.storage).tracked_storage(route.index).unwrap();
		let id = unsafe { storage.insert(value, query, target_gen) };
		span.record("id", id);
		Id::new(id, route)
	}
}
