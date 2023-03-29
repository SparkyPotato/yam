use std::ops::Deref;

use parking_lot::{
	lock_api::{RawRwLock, RawRwLockFair},
	RwLock,
};

use crate::internal::{storage::DashMap, Interned};

pub trait ErasedInternedStorage {}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct InternedStorage<T: Interned> {
	map: DashMap<T, u32>,
	values: RwLock<Vec<RwLock<T>>>,
}

pub struct Get<'a, T> {
	slot: &'a RwLock<T>,
	values: &'a RwLock<Vec<RwLock<T>>>,
}

impl<T: Interned> ErasedInternedStorage for InternedStorage<T> {}

impl<T: Interned> InternedStorage<T> {
	/// Insert a new value into the storage.
	pub fn insert(&self, value: T) -> u32 {
		match self.map.get(&value) {
			Some(index) => {
				let index = *index;
				let values = self.values.read();
				let slot = &values[index as usize];
				*slot.write() = value;

				index
			},
			None => {
				let mut values = self.values.write();
				let index = values.len() as u32;
				values.push(RwLock::new(value.clone()));
				self.map.insert(value, index);
				index
			},
		}
	}

	pub fn get(&self, index: u32) -> Get<'_, T> {
		unsafe {
			self.values.raw().lock_shared();
			let slot = &(*self.values.data_ptr())[index as usize];
			slot.raw().lock_shared();
			Get {
				slot,
				values: &self.values,
			}
		}
	}
}

impl<T: Interned> Default for InternedStorage<T> {
	fn default() -> Self {
		Self {
			map: DashMap::default(),
			values: RwLock::new(Vec::new()),
		}
	}
}

impl<'a> dyn ErasedInternedStorage + 'a {
	/// **Safety**: The type of `self` must be `TrackedStorage<T>`.
	pub unsafe fn insert<T: Interned>(&self, value: T) -> u32 {
		unsafe {
			let storage = self as *const dyn ErasedInternedStorage as *const InternedStorage<T>;
			(*storage).insert(value)
		}
	}

	/// **Safety**: The type of `self` must be `TrackedStorage<T>`.
	pub unsafe fn get<T: Interned>(&self, index: u32) -> Get<'_, T> {
		unsafe {
			let storage = self as *const dyn ErasedInternedStorage as *const InternedStorage<T>;
			(*storage).get(index)
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
