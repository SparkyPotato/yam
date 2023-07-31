use std::{borrow::Borrow, hash::Hash};

use parking_lot::{MappedRwLockReadGuard, Mutex, RwLock, RwLockReadGuard};
use rustc_hash::FxHashMap;

use crate::{event, internal::Interned};

pub trait ErasedInternedStorage {}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct InternedStorage<T: Interned> {
	/// Mutex is required for TOCTOU safety.
	map: Mutex<FxHashMap<T, u32>>,
	values: RwLock<Vec<T>>,
}

pub type Get<'a, T> = MappedRwLockReadGuard<'a, T>;

impl<T: Interned> ErasedInternedStorage for InternedStorage<T> {}

impl<T: Interned> InternedStorage<T> {
	/// Insert a new value into the storage.
	pub fn insert(&self, value: T) -> u32 {
		let mut map = self.map.lock();
		match map.get(&value) {
			Some(index) => *index,
			None => {
				event!(trace, "inserting new value");
				let mut values = self.values.write();
				let index = values.len() as u32;
				values.push(value.clone());
				map.insert(value, index);
				index
			},
		}
	}

	pub fn insert_ref<U>(&self, value: &U) -> u32
	where
		U: ToOwned<Owned = T> + Hash + Eq + ?Sized,
		T: Borrow<U>,
	{
		let mut map = self.map.lock();
		match map.get(value) {
			Some(index) => *index,
			None => {
				event!(trace, "inserting new value");
				let mut values = self.values.write();
				let index = values.len() as u32;
				values.push(value.to_owned());
				map.insert(value.to_owned(), index);
				index
			},
		}
	}

	pub fn get(&self, index: u32) -> Get<'_, T> {
		RwLockReadGuard::map(self.values.read_recursive(), |x| &x[index as usize])
	}
}

impl<T: Interned> Default for InternedStorage<T> {
	fn default() -> Self {
		Self {
			map: Mutex::default(),
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
