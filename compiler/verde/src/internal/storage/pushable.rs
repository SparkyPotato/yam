use std::ops::Deref;

use dashmap::mapref::{multiple::RefMulti, one::Ref};
use parking_lot::{lock_api::RawMutex, Mutex};

use crate::{
	internal::storage::{query::ErasedQueryId, DashMap, Route},
	Pushable,
};

pub trait ErasedPushableStorage {
	fn clear(&self, query: ErasedQueryId);
}

impl<'a> dyn ErasedPushableStorage + 'a {
	/// **Safety**: The type of `self` must be `PushableStorage<T>`.
	pub unsafe fn push<T: Pushable>(&self, query: ErasedQueryId, value: T) {
		unsafe {
			let storage = self as *const dyn ErasedPushableStorage as *const PushableStorage<T>;
			(*storage).push(query, value);
		}
	}

	/// **Safety**: The type of `self` must be `PushableStorage<T>`.
	pub unsafe fn get_all<T: Pushable>(&self) -> impl Iterator<Item = &'_ T> {
		unsafe {
			let storage = self as *const dyn ErasedPushableStorage as *const PushableStorage<T>;
			(*storage).get_all()
		}
	}

	/// **Safety**: The type of `self` must be `PushableStorage<T>`.
	pub unsafe fn get_query<T: Pushable>(&self, query: Route) -> impl Iterator<Item = &'_ T> {
		unsafe {
			let storage = self as *const dyn ErasedPushableStorage as *const PushableStorage<T>;
			(*storage).get_query(query)
		}
	}

	/// **Safety**: The type of `self` must be `PushableStorage<T>`.
	pub unsafe fn get_query_invocation<T: Pushable>(&self, query: ErasedQueryId) -> impl Iterator<Item = &'_ T> {
		unsafe {
			let storage = self as *const dyn ErasedPushableStorage as *const PushableStorage<T>;
			(*storage).get_query_invocation(query)
		}
	}
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct PushableStorage<T> {
	pub(crate) map: DashMap<Route, Vec<Mutex<Vec<T>>>>,
}

impl<T: Pushable> ErasedPushableStorage for PushableStorage<T> {
	fn clear(&self, query: ErasedQueryId) {
		let mut data = self.map.entry(query.route).or_insert_with(Vec::new);
		Self::expand_to(&mut data, query.index);
		data[query.index as usize].lock().clear();
	}
}

impl<T: Pushable> PushableStorage<T> {
	pub fn new() -> Self {
		Self {
			map: DashMap::default(),
		}
	}

	pub fn push(&self, query: ErasedQueryId, value: T) {
		let mut data = self.map.entry(query.route).or_insert_with(Vec::new);
		Self::expand_to(&mut data, query.index);
		data[query.index as usize].lock().push(value);
	}

	pub fn get_all(&self) -> impl Iterator<Item = &'_ T> {
		self.map
			.iter()
			.flat_map(|x| {
				let value = unsafe { std::mem::transmute::<_, std::slice::Iter<'_, _>>(x.iter()) };
				RefMultiMap::new(x, value)
			})
			.flat_map(|vec| VecIter::new(vec))
	}

	pub fn get_query(&self, query: Route) -> impl Iterator<Item = &'_ T> {
		self.map
			.get(&query)
			.into_iter()
			.flat_map(|x| {
				let value = unsafe { std::mem::transmute::<_, std::slice::Iter<'_, _>>(x.iter()) };
				RefMap::new(x, value)
			})
			.flat_map(|vec| VecIter::new(vec))
	}

	pub fn get_query_invocation(&self, query: ErasedQueryId) -> impl Iterator<Item = &'_ T> {
		self.map
			.get(&query.route)
			.into_iter()
			.flat_map(|x| {
				let value = unsafe { std::mem::transmute::<_, std::slice::Iter<'_, _>>(x.iter()) };
				RefMap::new(x, value)
			})
			.flat_map(|vec| VecIter::new(vec))
	}

	fn expand_to(data: &mut Vec<Mutex<Vec<T>>>, index: u32) {
		let len = data.len();
		if len <= index as usize {
			data.extend(std::iter::repeat_with(|| Mutex::new(Vec::new())).take(index as usize - len + 1))
		}
	}
}

struct RefMultiMap<'a, K, V, S, T> {
	value: T,
	_inner: RefMulti<'a, K, V, S>,
}

impl<'a, K, V, S, T> RefMultiMap<'a, K, V, S, T> {
	fn new(_inner: RefMulti<'a, K, V, S>, value: T) -> Self { Self { _inner, value } }
}

impl<'a, K, V, S, T> Deref for RefMultiMap<'a, K, V, S, T> {
	type Target = T;

	fn deref(&self) -> &Self::Target { &self.value }
}

impl<'a, K, V, S, T: Iterator> Iterator for RefMultiMap<'a, K, V, S, T> {
	type Item = T::Item;

	fn next(&mut self) -> Option<Self::Item> { self.value.next() }
}

struct RefMap<'a, K, V, S, T> {
	value: T,
	_inner: Ref<'a, K, V, S>,
}

impl<'a, K, V, S, T> RefMap<'a, K, V, S, T> {
	fn new(_inner: Ref<'a, K, V, S>, value: T) -> Self { Self { _inner, value } }
}

impl<'a, K, V, S, T> Deref for RefMap<'a, K, V, S, T> {
	type Target = T;

	fn deref(&self) -> &Self::Target { &self.value }
}

impl<'a, K, V, S, T: Iterator> Iterator for RefMap<'a, K, V, S, T> {
	type Item = T::Item;

	fn next(&mut self) -> Option<Self::Item> { self.value.next() }
}

struct VecIter<'a, T> {
	vec: &'a Mutex<Vec<T>>,
	iter: std::slice::Iter<'a, T>,
}

impl<'a, T> VecIter<'a, T> {
	fn new(vec: &'a Mutex<Vec<T>>) -> Self {
		let iter = unsafe {
			vec.raw().lock();
			(*vec.data_ptr()).iter()
		};
		Self { vec, iter }
	}
}

impl<'a, T> Iterator for VecIter<'a, T> {
	type Item = &'a T;

	fn next(&mut self) -> Option<Self::Item> { self.iter.next() }
}

impl<T> Drop for VecIter<'_, T> {
	fn drop(&mut self) {
		unsafe {
			self.vec.raw().unlock();
		}
	}
}

impl<T> Default for PushableStorage<T> {
	fn default() -> Self {
		Self {
			map: DashMap::default(),
		}
	}
}
