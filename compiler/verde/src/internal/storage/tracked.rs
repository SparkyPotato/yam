use std::{
	ops::Deref,
	sync::atomic::{AtomicU64, Ordering},
};

use parking_lot::{
	lock_api::{RawRwLock, RawRwLockFair},
	RwLock,
};

use crate::{
	internal::storage::{routing::Route, DashMap},
	Tracked,
};

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ErasedId {
	pub(crate) route: Route,
	pub(crate) index: u32,
}

pub trait ErasedTrackedStorage {
	fn get_generation(&self, index: u32) -> u64;
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Id<T> {
	pub(crate) inner: ErasedId,
	pub(crate) _phantom: std::marker::PhantomData<T>,
}
unsafe impl<T> Send for Id<T> {}
unsafe impl<T> Sync for Id<T> {}

pub struct Get<'a, T> {
	slot: &'a RwLock<T>,
	values: &'a RwLock<Vec<Slot<T>>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct TrackedStorage<T: Tracked> {
	pub(crate) map: DashMap<TrackedIdent<T>, u32>,
	pub(crate) values: RwLock<Vec<Slot<T>>>,
}

impl<T: Tracked> ErasedTrackedStorage for TrackedStorage<T> {
	fn get_generation(&self, index: u32) -> u64 {
		let values = self.values.read();
		let slot = &values[index as usize];
		slot.generation.load(Ordering::Acquire)
	}
}

impl<T: Tracked> TrackedStorage<T> {
	/// Insert a new value into the storage.
	pub fn insert(&self, value: T, query: Route) -> u32 {
		let ident = TrackedIdent {
			id: value.id().clone(),
			query,
		};
		match self.map.get(&ident) {
			Some(index) => {
				let index = *index;
				let values = self.values.read();
				let slot = &values[index as usize];
				let mut out = slot.value.write();

				if *out != value {
					*out = value;
					slot.generation.fetch_add(1, Ordering::Release);
				}

				index
			},
			None => {
				let mut values = self.values.write();
				let index = values.len() as u32;
				values.push(Slot {
					value: RwLock::new(value),
					generation: AtomicU64::new(0),
				});
				self.map.insert(ident, index);
				index
			},
		}
	}

	pub fn get(&self, index: u32) -> Get<'_, T> {
		unsafe {
			self.values.raw().lock_shared();
			let slot = &(*self.values.data_ptr())[index as usize].value;
			slot.raw().lock_shared();
			Get {
				slot,
				values: &self.values,
			}
		}
	}
}

impl<'a> dyn ErasedTrackedStorage + 'a {
	/// **Safety**: The type of `self` must be `TrackedStorage<T>`.
	pub unsafe fn insert<T: Tracked>(&self, value: T, query: Route) -> u32 {
		unsafe {
			let storage = self as *const dyn ErasedTrackedStorage as *const TrackedStorage<T>;
			(*storage).insert(value, query)
		}
	}

	/// **Safety**: The type of `self` must be `TrackedStorage<T>`.
	pub unsafe fn get<T: Tracked>(&self, index: u32) -> Get<'_, T> {
		unsafe {
			let storage = self as *const dyn ErasedTrackedStorage as *const TrackedStorage<T>;
			(*storage).get(index)
		}
	}
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub(crate) struct Slot<T> {
	pub(crate) value: RwLock<T>,
	pub(crate) generation: AtomicU64,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub(crate) struct TrackedIdent<T: Tracked> {
	pub(crate) id: T::Id,
	pub(crate) query: Route,
}

impl<T: Tracked> Clone for TrackedIdent<T> {
	fn clone(&self) -> Self {
		Self {
			id: self.id.clone(),
			query: self.query,
		}
	}
}
impl<T: Tracked> PartialEq for TrackedIdent<T> {
	fn eq(&self, other: &Self) -> bool { self.id == other.id && self.query == other.query }
}
impl<T: Tracked> Eq for TrackedIdent<T> {}
impl<T: Tracked> std::hash::Hash for TrackedIdent<T> {
	fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
		self.id.hash(state);
		self.query.hash(state);
	}
}

impl<T: Tracked> Default for TrackedStorage<T> {
	fn default() -> Self {
		Self {
			map: DashMap::default(),
			values: RwLock::new(Vec::new()),
		}
	}
}

impl<T> Clone for Id<T> {
	fn clone(&self) -> Self { *self }
}
impl<T> Copy for Id<T> {}
impl<T> PartialEq for Id<T> {
	fn eq(&self, other: &Self) -> bool { self.inner == other.inner }
}
impl<T> Eq for Id<T> {}
impl<T> std::hash::Hash for Id<T> {
	fn hash<H: std::hash::Hasher>(&self, state: &mut H) { self.inner.hash(state); }
}
impl<T> Id<T> {
	pub(crate) fn new(index: u32, route: Route) -> Self {
		Self {
			inner: ErasedId { index, route },
			_phantom: std::marker::PhantomData,
		}
	}
}

impl<T> Deref for Get<'_, T> {
	type Target = T;

	fn deref(&self) -> &Self::Target { unsafe { &*self.slot.data_ptr() } }
}

impl<T> Drop for Get<'_, T> {
	fn drop(&mut self) {
		unsafe {
			self.slot.raw().unlock_shared_fair();
			self.values.raw().unlock_shared_fair();
		}
	}
}
