#![allow(ambiguous_glob_reexports)]

use std::{fmt::Debug, hash::BuildHasherDefault, marker::PhantomData, num::NonZeroU64};

use rustc_hash::FxHasher;

pub mod interned;
mod pushable;
mod query;
mod routing;
pub mod tracked;

pub use interned::*;
pub use pushable::*;
pub use query::*;
pub use routing::*;
pub use tracked::*;

pub(crate) type DashMap<K, V> = dashmap::DashMap<K, V, BuildHasherDefault<FxHasher>>;

/// An instance of `T` stored in the database.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Id<T>(NonZeroU64, PhantomData<fn() -> T>);

impl<T> Debug for Id<T> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}({})", std::any::type_name::<T>(), self.get().index)
	}
}

impl<T> Clone for Id<T> {
	fn clone(&self) -> Self { *self }
}
impl<T> Copy for Id<T> {}

impl<T> PartialEq for Id<T> {
	fn eq(&self, other: &Self) -> bool { self.0 == other.0 }
}
impl<T> Eq for Id<T> {}

impl<T> std::hash::Hash for Id<T> {
	fn hash<H: std::hash::Hasher>(&self, state: &mut H) { self.0.hash(state); }
}

impl<T> Id<T> {
	pub(crate) fn new(index: u32, route: Route) -> Self {
		let route_storage = route.storage;
		let route_index = route.index;
		Self(
			NonZeroU64::new((index as u64) | ((route_index as u64) << 32) | ((route_storage as u64) << 48)).unwrap(),
			PhantomData,
		)
	}

	pub(crate) fn get(self) -> ErasedId {
		let index = self.0.get() as u32;
		let route_storage = (self.0.get() >> 48) as u16;
		let route_index = (self.0.get() >> 32) as u16;
		ErasedId {
			index,
			route: Route {
				storage: route_storage,
				index: route_index,
			},
		}
	}
}
