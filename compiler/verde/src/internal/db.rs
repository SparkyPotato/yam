use std::{borrow::Borrow, cell::RefCell, hash::Hash, mem::MaybeUninit};

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
	#[doc(hidden)]
	fn init_routing(table: &mut RoutingTableBuilder)
	where
		Self: Sized;

	#[doc(hidden)]
	fn routing_table(&self) -> &RoutingTable;

	#[doc(hidden)]
	fn storage_struct(&self, storage: u16) -> &dyn Storage;
}

/// A context into the database. Used by query functions to access the database.
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

	/// Get a reference to the value `id` points to.
	pub fn get<T: Tracked>(&self, id: Id<T>) -> tracked::Get<'_, T> {
		let id = id.get();
		span!(
			enter trace,
			"fetching value",
			ty = std::any::type_name::<T>(),
			id = id.index
		);
		unsafe {
			let gen = self.get_generation(id);
			self.dependencies
				.try_borrow_mut()
				.expect("Cannot call `get` within a `map` scope")
				.assume_init_mut()
				.insert((id, gen));
		}
		let storage = self
			.db
			.storage_struct(id.route.storage)
			.tracked_storage(id.route.index)
			.unwrap();
		unsafe { storage.get(id.index) }
	}

	/// Get a reference to the value `id` points to.
	pub fn geti<T: Interned>(&self, id: Id<T>) -> interned::Get<'_, T> { self.db.geti(id) }

	/// Intern a value.
	pub fn add<T: Interned>(&self, value: T) -> Id<T> { self.db.add(value) }

	/// Intern a value through a reference.
	pub fn add_ref<T, U>(&self, value: &U) -> Id<T>
	where
		U: ToOwned<Owned = T> + Hash + Eq + ?Sized,
		T: Borrow<U> + Interned,
	{
		self.db.add_ref(value)
	}

	/// Push a value to the database from this query.
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

	#[doc(hidden)]
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

	#[doc(hidden)]
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
	/// Set an input value.
	pub fn set_input<T: Tracked>(&mut self, value: T) -> Id<T> { (self as &dyn Db).insert(Route::input(), value) }

	/// Get a reference to the value `id` points to.
	pub fn get<T: Tracked>(&self, id: Id<T>) -> tracked::Get<'_, T> {
		let id = id.get();
		span!(
			enter trace,
			"fetching value",
			ty = std::any::type_name::<T>(),
			id = id.index
		);
		let storage = self
			.storage_struct(id.route.storage)
			.tracked_storage(id.route.index)
			.unwrap();
		unsafe { storage.get(id.index) }
	}

	/// Get a reference to the value `id` points to.
	pub fn geti<T: Interned>(&self, id: Id<T>) -> interned::Get<'_, T> {
		let id = id.get();
		span!(
			enter trace,
			"fetching value",
			ty = std::any::type_name::<T>(),
			id = id.index
		);
		let storage = self
			.storage_struct(id.route.storage)
			.interned_storage(id.route.index)
			.unwrap();
		unsafe { storage.get(id.index) }
	}

	/// Intern a value.
	pub fn add<T: Interned>(&self, value: T) -> Id<T> {
		let span = span!(
			trace,
			"inserting value",
			ty = std::any::type_name::<T>(),
			id = tracing::field::Empty
		);
		#[allow(clippy::let_unit_value)]
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

	/// Intern a value through a reference.
	pub fn add_ref<T, U>(&self, value: &U) -> Id<T>
	where
		U: ToOwned<Owned = T> + Hash + Eq + ?Sized,
		T: Borrow<U> + Interned,
	{
		let span = span!(
			trace,
			"inserting value",
			ty = std::any::type_name::<T>(),
			id = tracing::field::Empty
		);
		#[allow(clippy::let_unit_value)]
		let _e = span.enter();
		let route = self.routing_table().route::<T>();
		let storage = self
			.storage_struct(route.storage)
			.interned_storage(route.index)
			.unwrap();
		let id = unsafe { storage.insert_ref(value) };
		span.record("id", id);
		Id::new(id, route)
	}

	/// Get all pushed `T`s.
	pub fn get_all<T: Pushable>(&self) -> impl Iterator<Item = &'_ T> {
		let route = self.routing_table().route::<T>();
		let storage = self
			.storage_struct(route.storage)
			.pushable_storage(route.index)
			.unwrap();
		unsafe { storage.get_all() }
	}

	/// Get all pushed `T`s from the query `Q`.
	pub fn get_query<Q: Query, T: Pushable>(&self) -> impl Iterator<Item = &'_ T> {
		let route = self.routing_table().route::<T>();
		let query = self.routing_table().route::<Q>();
		let storage = self
			.storage_struct(route.storage)
			.pushable_storage(route.index)
			.unwrap();
		unsafe { storage.get_query(query) }
	}

	#[doc(hidden)]
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
		#[allow(clippy::let_unit_value)]
		let _e = span.enter();
		let route = self.routing_table().route::<T>();
		let storage = self.storage_struct(route.storage).tracked_storage(route.index).unwrap();
		let id = unsafe { storage.insert(value, query) };
		span.record("id", id);
		Id::new(id, route)
	}
}
