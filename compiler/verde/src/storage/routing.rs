//! ### Storage routing
//!
//! The database has two tiers of storing data:
//! - The database itself, storing storage structs.
//! - Storage structs, which store the actual type storages.
//!
//! This allows for multi-crate compilation, where each crate exposes a storage struct, with only the main driver crate
//! using the database.

use std::any::TypeId;

use rustc_hash::FxHashMap;

use crate::TrackedOrQuery;

/// A type-erased route through the database storage.
/// Uniquely identifies the storage for a particular [`Tracked`](crate::Tracked) type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Route {
	/// The index of the storage struct in the database.
	/// A storage of `0` is reserved for fake queries generated by [`crate::Db::set_input`].
	pub storage: u16,
	/// The index of the type storage in the storage struct.
	pub index: u16,
}

impl Route {
	pub(crate) fn input() -> Self { Self { storage: 0, index: 0 } }
}

/// A static table that maps [`TypeId`]s to [`Route`]s, generated at database initialization.
/// This is required because `TypeId`s are not guaranteed to be stable across compilations, while `Route`s are.
pub struct RoutingTable {
	routes: FxHashMap<TypeId, Route>,
	inverse_routes: FxHashMap<Route, TypeId>,
}

impl RoutingTable {
	pub fn route<T: TrackedOrQuery>(&self) -> Route {
		match self.routes.get(&TypeId::of::<T>()) {
			Some(route) => *route,
			None => panic!("Database does not contain `{}`", std::any::type_name::<T>()),
		}
	}
}

#[derive(Default)]
pub struct RoutingTableBuilder {
	routes: FxHashMap<TypeId, Route>,
	inverse_routes: FxHashMap<Route, TypeId>,
}

impl RoutingTableBuilder {
	pub fn start_route(&mut self, storage: u16) -> RouteBuilder {
		RouteBuilder {
			routes: &mut self.routes,
			inverse_routes: &mut self.inverse_routes,
			storage,
		}
	}

	pub fn finish(self) -> RoutingTable {
		RoutingTable {
			routes: self.routes,
			inverse_routes: self.inverse_routes,
		}
	}
}

pub struct RouteBuilder<'a> {
	routes: &'a mut FxHashMap<TypeId, Route>,
	inverse_routes: &'a mut FxHashMap<Route, TypeId>,
	storage: u16,
}

impl RouteBuilder<'_> {
	pub fn add<T: TrackedOrQuery>(&mut self, index: u16) {
		let route = Route {
			storage: self.storage,
			index,
		};
		let id = TypeId::of::<T>();

		if self.routes.insert(id, route).is_some() {
			panic!("Duplicate route for type `{}`", std::any::type_name::<T>());
		}
		self.inverse_routes.insert(route, id);
	}
}
