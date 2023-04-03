use std::marker::PhantomData;

use crate::Ix;

#[derive(Clone, Eq, PartialEq)]
pub struct DenseMap<K, V> {
	map: Vec<Option<V>>,
	_phantom: PhantomData<fn() -> K>,
}

impl<K, V> DenseMap<K, V> {
	pub fn new() -> Self {
		Self {
			map: Vec::new(),
			_phantom: PhantomData,
		}
	}

	pub fn insert(&mut self, key: Ix<K>, value: V) -> Option<V> {
		let key = key.index();
		if key >= self.map.len() {
			self.map.resize_with(key + 1, || None);
		}
		self.map[key].replace(value)
	}

	pub fn get(&self, key: Ix<K>) -> Option<&V> { self.map.get(key.index()).and_then(|x| x.as_ref()) }

	pub fn get_mut(&mut self, key: Ix<K>) -> Option<&mut V> { self.map.get_mut(key.index()).and_then(|x| x.as_mut()) }
}

impl<K, V> Default for DenseMap<K, V> {
	fn default() -> Self { Self::new() }
}
