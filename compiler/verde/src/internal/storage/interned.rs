use std::{borrow::Borrow, hash::Hash, time::Duration};

use parking_lot::{MappedRwLockReadGuard, RwLock, RwLockReadGuard};

use crate::{
	event,
	internal::{storage::DashMap, Interned},
};

pub trait ErasedInternedStorage {}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct InternedStorage<T: Interned> {
	map: DashMap<T, u32>,
	values: RwLock<Vec<T>>,
}

pub type Get<'a, T> = MappedRwLockReadGuard<'a, T>;

impl<T: Interned> ErasedInternedStorage for InternedStorage<T> {}

impl<T: Interned> InternedStorage<T> {
	/// Insert a new value into the storage.
	pub fn insert(&self, value: T) -> u32 {
		match self.map.get(&value) {
			Some(index) => *index,
			None => {
				event!(trace, "inserting new value");
				let mut values = self
					.values
					.try_write_for(Duration::from_secs(2))
					.expect("Interning timed out: perhaps you have a deadlock?");
				let index = values.len() as u32;
				values.push(value.clone());
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
				values.push(value.to_owned());
				self.map.insert(value.to_owned(), index);
				index
			},
		}
	}

	pub fn get(&self, index: u32) -> Get<'_, T> {
		event!(trace, "reading value");
		RwLockReadGuard::map(
			self.values
				.try_read_for(Duration::from_secs(2))
				.expect("Interning timed out: perhaps you have a deadlock?"),
			|x| &x[index as usize],
		)
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
