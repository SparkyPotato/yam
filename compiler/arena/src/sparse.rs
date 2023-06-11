use rustc_hash::FxHashMap;

use crate::Ix;

/// A map from `Ix<K>` to `V` that expects to have values sparsely populated for all possible `Ix<K>` values.
#[derive(Clone, Eq, PartialEq)]
pub struct SparseMap<K, V> {
	map: FxHashMap<Ix<K>, V>,
}

impl<K, V> SparseMap<K, V> {
	pub fn new() -> Self {
		Self {
			map: FxHashMap::default(),
		}
	}

	pub fn insert(&mut self, key: Ix<K>, value: V) -> Option<V> { self.map.insert(key, value) }

	pub fn get(&self, key: Ix<K>) -> Option<&V> { self.map.get(&key) }

	pub fn get_mut(&mut self, key: Ix<K>) -> Option<&mut V> { self.map.get_mut(&key) }
}

impl<K, V> Default for SparseMap<K, V> {
	fn default() -> Self { Self::new() }
}
