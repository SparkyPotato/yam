use tokio::sync::Mutex;

use crate::{
	internal::storage::{query::ErasedQueryId, stack_future::StackFuture, DashMap, Route},
	Pushable,
};

pub trait ErasedPushableStorage {
	fn clear(&self, query: ErasedQueryId) -> StackFuture<'_, ()>;
}

impl<'a> dyn ErasedPushableStorage + 'a {
	/// **Safety**: The type of `self` must be `PushableStorage<T>`.
	pub async unsafe fn push<T: Pushable>(&self, query: ErasedQueryId, value: T) {
		unsafe {
			let storage = self as *const dyn ErasedPushableStorage as *const PushableStorage<T>;
			(*storage).push(query, value).await;
		}
	}

	/// **Safety**: The type of `self` must be `PushableStorage<T>`.
	pub async unsafe fn get_all<T: Pushable>(&self) -> Vec<T> {
		unsafe {
			let storage = self as *const dyn ErasedPushableStorage as *const PushableStorage<T>;
			(*storage).get_all().await
		}
	}
}

pub struct PushableStorage<T> {
	pub(crate) map: DashMap<Route, Vec<Mutex<Vec<T>>>>,
}

impl<T: Pushable> ErasedPushableStorage for PushableStorage<T> {
	fn clear(&self, query: ErasedQueryId) -> StackFuture<'_, ()> {
		StackFuture::new(async move {
			let mut data = self.map.entry(query.route).or_insert_with(|| Vec::new());
			Self::expand_to(&mut data, query.index);
			data[query.index as usize].lock().await.clear();
		})
	}
}

impl<T: Pushable> PushableStorage<T> {
	pub fn new() -> Self {
		Self {
			map: DashMap::default(),
		}
	}

	pub async fn push(&self, query: ErasedQueryId, value: T) {
		let mut data = self.map.entry(query.route).or_insert_with(|| Vec::new());
		Self::expand_to(&mut data, query.index);
		data[query.index as usize].lock().await.push(value);
	}

	pub async fn get_all(&self) -> Vec<T> {
		let mut result = Vec::new();
		for data in self.map.iter() {
			for mutex in data.iter() {
				let lock = mutex.lock().await;
				result.extend(lock.iter().cloned());
			}
		}
		result
	}

	fn expand_to(data: &mut Vec<Mutex<Vec<T>>>, index: u32) {
		let len = data.len();
		if len <= index as usize {
			data.extend(std::iter::repeat_with(|| Mutex::new(Vec::new())).take(index as usize - len + 1))
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
