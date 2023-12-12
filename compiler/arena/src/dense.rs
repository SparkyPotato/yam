use std::{fmt::Debug, marker::PhantomData, ops::Index};

use crate::Ix;

/// A map from `Ix<K>` to `V` that expects to have values densely populated for all possible `Ix<K>` values.
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

	pub fn with_capacity(capacity: usize) -> Self {
		Self {
			map: Vec::with_capacity(capacity),
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

	pub fn len(&self) -> usize { self.map.iter().filter(|x| x.is_some()).count() }

	pub fn iter(&self) -> impl Iterator<Item = (Ix<K>, &V)> + '_ {
		self.map
			.iter()
			.enumerate()
			.filter_map(|(i, x)| x.as_ref().map(|x| (Ix::new(i), x)))
	}
}

impl<K, V> Default for DenseMap<K, V> {
	fn default() -> Self { Self::new() }
}

impl<K, V> Index<Ix<K>> for DenseMap<K, V> {
	type Output = V;

	fn index(&self, index: Ix<K>) -> &Self::Output { self.get(index).unwrap() }
}

impl<K, V: Debug> Debug for DenseMap<K, V> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { f.debug_map().entries(self.iter()).finish() }
}
