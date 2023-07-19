use std::{borrow::Borrow, hash::Hash, ops::Deref, time::Duration};

use parking_lot::{
	lock_api::{RawRwLockFair, RawRwLockTimed},
	RwLock,
};

use crate::{
	event,
	internal::{storage::DashMap, Interned},
};

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
			Some(index) => *index,
			None => {
				let mut values = self
					.values
					.try_write_for(Duration::from_secs(2))
					.expect("Interning timed out: perhaps you have a deadlock?");
				let index = values.len() as u32;
				values.push(RwLock::new(value.clone()));
				self.map.insert(value, index);
				index
			},
		}
	}

	pub fn insert_ref<U>(&self, value: &U) -> u32
	where
		U: ToOwned<Owned = T> + Hash + Eq + ?Sized,
		T: Borrow<U>,
	{
		match self.map.get(value) {
			Some(index) => *index,
			None => {
				event!(trace, "inserting new value");
				let mut values = self
					.values
					.try_write_for(Duration::from_secs(2))
					.expect("Interning timed out: perhaps you have a deadlock?");
				let index = values.len() as u32;
				values.push(RwLock::new(value.to_owned()));
				self.map.insert(value.to_owned(), index);
				index
			},
		}
	}

	pub fn get(&self, index: u32) -> Get<'_, T> {
		unsafe {
			if self.values.raw().try_lock_shared_for(Duration::from_secs(2)) {
				let slot = &(*self.values.data_ptr())[index as usize];

				if slot.raw().try_lock_shared_for(Duration::from_secs(2)) {
					Get {
						slot,
						values: &self.values,
					}
				} else {
					panic!("Interning timed out: perhaps you have a deadlock?");
				}
			} else {
				panic!("Interning timed out: perhaps you have a deadlock?");
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
	/// **Safety**: The type of `self` must be `InternedStorage<T>`.
	pub unsafe fn insert<T: Interned>(&self, value: T) -> u32 {
		unsafe {
			let storage = self as *const dyn ErasedInternedStorage as *const InternedStorage<T>;
			(*storage).insert(value)
		}
	}

	/// **Safety**: The type of `self` must be `InternedStorage<T>`.
	pub unsafe fn get<T: Interned>(&self, index: u32) -> Get<'_, T> {
		unsafe {
			let storage = self as *const dyn ErasedInternedStorage as *const InternedStorage<T>;
			(*storage).get(index)
		}
	}

	/// **Safety**: The type of `self` must be `InternedStorage<T>`.
	pub unsafe fn insert_ref<T, U>(&self, value: &U) -> u32
	where
		U: ToOwned<Owned = T> + Hash + Eq + ?Sized,
		T: Borrow<U> + Interned,
	{
		unsafe {
			let storage = self as *const dyn ErasedInternedStorage as *const InternedStorage<T>;
			(*storage).insert_ref(value)
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
