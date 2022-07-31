use std::{
	cell::{Cell, UnsafeCell},
	collections::HashMap,
	convert::identity,
	fmt::Debug,
	hash::Hash,
	marker::PhantomData,
	mem::MaybeUninit,
	ops::{Deref, DerefMut, Index, IndexMut},
};

pub trait Id: Copy + Hash + Eq {
	fn from_id(id: u32) -> Self;

	fn id(self) -> u32;
}

pub struct IdGen<T> {
	counter: u32,
	_phantom: PhantomData<T>,
}

impl<T> Default for IdGen<T> {
	fn default() -> Self {
		Self {
			counter: 0,
			_phantom: PhantomData,
		}
	}
}

impl<T: Id> IdGen<T> {
	pub fn new() -> Self { Self::default() }

	pub fn next(&mut self) -> T {
		let id = self.counter;
		self.counter += 1;
		T::from_id(id)
	}
}

impl<T: Id> Iterator for IdGen<T> {
	type Item = T;

	fn next(&mut self) -> Option<Self::Item> { Some(self.next()) }
}

pub struct DenseMap<T, U> {
	items: Vec<U>,
	_phantom: PhantomData<T>,
}

impl<T, U> Default for DenseMap<T, U> {
	fn default() -> Self {
		Self {
			items: Vec::new(),
			_phantom: PhantomData,
		}
	}
}

impl<T: Id + Debug, U: Debug> Debug for DenseMap<T, U> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let mut map = f.debug_map();

		for (id, item) in self.items.iter().enumerate() {
			map.entry(&T::from_id(id as u32), item);
		}

		map.finish()
	}
}

impl<T: Id, U> DenseMap<T, U> {
	pub fn builder() -> DenseMapBuilder<T, U> { DenseMapBuilder::default() }

	pub fn iter(&self) -> impl Iterator<Item = (T, &U)> + '_ {
		self.items
			.iter()
			.enumerate()
			.map(|(id, item)| (T::from_id(id as u32), item))
	}

	pub fn iter_mut(&mut self) -> impl Iterator<Item = (T, &mut U)> + '_ {
		self.items
			.iter_mut()
			.enumerate()
			.map(|(id, item)| (T::from_id(id as u32), item))
	}

	pub fn make_mut(self) -> DenseMut<T, U> {
		DenseMut {
			borrow_flags: vec![Cell::new(0); self.items.len()],
			items: self.items.into_iter().map(UnsafeCell::new).collect(),
			_phantom: PhantomData,
		}
	}
}

impl<T: Id, U> Index<T> for DenseMap<T, U> {
	type Output = U;

	fn index(&self, id: T) -> &Self::Output { &self.items[id.id() as usize] }
}

impl<T: Id, U> IndexMut<T> for DenseMap<T, U> {
	fn index_mut(&mut self, id: T) -> &mut Self::Output { &mut self.items[id.id() as usize] }
}

pub struct DenseMut<T, U> {
	items: Vec<UnsafeCell<U>>,
	borrow_flags: Vec<Cell<isize>>,
	_phantom: PhantomData<T>,
}

impl<T: Id, U> DenseMut<T, U> {
	pub fn try_get(&self, id: T) -> Option<Ref<T, U>> {
		let id = id.id() as usize;

		let flag = &self.borrow_flags[id];

		if flag.get() >= 0 {
			flag.set(flag.get() + 1);
			Some(Ref { id, source: self })
		} else {
			None
		}
	}

	pub fn try_get_mut(&self, id: T) -> Option<RefMut<T, U>> {
		let id = id.id() as usize;

		let flag = &self.borrow_flags[id];

		if flag.get() == 0 {
			flag.set(-1);
			Some(RefMut { id, source: self })
		} else {
			None
		}
	}

	pub fn iter_mut(&self) -> impl Iterator<Item = (T, RefMut<T, U>)> + '_ {
		(0..self.items.len()).map(|id| {
			let id = T::from_id(id as u32);
			(id, self.try_get_mut(id).expect("already borrowed"))
		})
	}

	pub fn make_imm(self) -> DenseMap<T, U> {
		DenseMap {
			items: self.items.into_iter().map(UnsafeCell::into_inner).collect(),
			_phantom: PhantomData,
		}
	}
}

pub struct Ref<'a, T, U> {
	id: usize,
	source: &'a DenseMut<T, U>,
}

impl<T, U> Deref for Ref<'_, T, U> {
	type Target = U;

	fn deref(&self) -> &Self::Target { unsafe { &*self.source.items[self.id].get() } }
}

impl<T, U> Drop for Ref<'_, T, U> {
	fn drop(&mut self) {
		let flag = &self.source.borrow_flags[self.id];
		flag.set(flag.get() - 1);
	}
}

pub struct RefMut<'a, T, U> {
	id: usize,
	source: &'a DenseMut<T, U>,
}

impl<T, U> Deref for RefMut<'_, T, U> {
	type Target = U;

	fn deref(&self) -> &Self::Target { unsafe { &*self.source.items[self.id].get() } }
}

impl<T, U> DerefMut for RefMut<'_, T, U> {
	fn deref_mut(&mut self) -> &mut Self::Target { unsafe { &mut *self.source.items[self.id].get() } }
}

impl<T, U> Drop for RefMut<'_, T, U> {
	fn drop(&mut self) { self.source.borrow_flags[self.id].set(0); }
}

pub struct DenseMapBuilder<T, U> {
	items: Vec<MaybeUninit<U>>,
	inserted: Vec<bool>,
	_phantom: PhantomData<T>,
}

impl<T: Id, U> Default for DenseMapBuilder<T, U> {
	fn default() -> Self {
		Self {
			items: Vec::new(),
			inserted: Vec::new(),
			_phantom: PhantomData,
		}
	}
}

impl<T: Id, U> DenseMapBuilder<T, U> {
	pub fn new() -> Self { Self::default() }

	pub fn add(&mut self, item: U) -> T {
		let index = self.items.len();
		self.items.push(MaybeUninit::new(item));
		self.inserted.push(true);
		T::from_id(index as u32)
	}

	pub fn reserve(&mut self) -> T {
		let index = self.items.len();
		self.items.push(MaybeUninit::uninit());
		self.inserted.push(false);
		T::from_id(index as u32)
	}

	pub fn already_inserted(&self, id: T) -> bool { self.inserted.get(id.id() as usize).copied().unwrap_or(false) }

	pub fn insert_at(&mut self, id: T, item: U) {
		let index = id.id() as usize;
		if self.items.len() < index + 1 {
			self.items.resize_with(index + 1, || MaybeUninit::uninit());
			self.inserted.resize(index + 1, false);
		}

		debug_assert!(!self.inserted[index], "IdMapBuilder::insert_at: id already inserted");

		self.inserted[index] = true;
		self.items[index] = MaybeUninit::new(item);
	}

	pub fn reset(&mut self) {
		self.items.clear();
		self.inserted.clear();
	}

	pub fn iter(&self) -> impl Iterator<Item = (T, &U)> + '_ {
		self.inserted
			.iter()
			.enumerate()
			.flat_map(|(i, x)| if *x { Some(i) } else { None })
			.map(|i| (T::from_id(i as _), unsafe { self.items[i].assume_init_ref() }))
	}

	pub fn build(self) -> DenseMap<T, U> {
		assert!(
			self.inserted.into_iter().all(identity),
			"IdMapBuilder::build: all ids not inserted"
		);

		let items = unsafe { self.items.into_iter().map(|item| item.assume_init()).collect() };

		DenseMap {
			items,
			_phantom: PhantomData,
		}
	}
}

impl<T: Id, U> Index<T> for DenseMapBuilder<T, U> {
	type Output = U;

	fn index(&self, id: T) -> &Self::Output {
		assert!(self.inserted[id.id() as usize], "IdMapBuilder::index: id not inserted");
		unsafe { self.items[id.id() as usize].assume_init_ref() }
	}
}

impl<T: Id, U> IndexMut<T> for DenseMapBuilder<T, U> {
	fn index_mut(&mut self, id: T) -> &mut Self::Output {
		assert!(
			self.inserted[id.id() as usize],
			"IdMapBuilder::index_mut: id not inserted"
		);
		unsafe { self.items[id.id() as usize].assume_init_mut() }
	}
}

pub struct SparseMap<T, U> {
	items: HashMap<T, U>,
}

impl<T: Id + Debug, U: Debug> Debug for SparseMap<T, U> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { write!(f, "{:?}", self.items) }
}

impl<T: Id, U> SparseMap<T, U> {
	pub fn builder() -> SparseMapBuilder<T, U> { SparseMapBuilder::default() }

	pub fn get(&self, id: T) -> Option<&U> { self.items.get(&id) }

	pub fn get_mut(&mut self, id: T) -> Option<&mut U> { self.items.get_mut(&id) }
}

impl<T: Id, U> Index<T> for SparseMap<T, U> {
	type Output = U;

	fn index(&self, id: T) -> &Self::Output { self.items.get(&id).unwrap() }
}

impl<T: Id, U> IndexMut<T> for SparseMap<T, U> {
	fn index_mut(&mut self, id: T) -> &mut Self::Output { self.items.get_mut(&id).unwrap() }
}

pub struct SparseMapBuilder<T, U> {
	items: HashMap<T, U>,
}

impl<T: Id, U> Default for SparseMapBuilder<T, U> {
	fn default() -> Self { Self { items: HashMap::new() } }
}

impl<T: Id, U> SparseMapBuilder<T, U> {
	pub fn new() -> Self { Self::default() }

	pub fn add(&mut self, id: T, item: U) { self.items.insert(id, item); }

	pub fn build(self) -> SparseMap<T, U> { SparseMap { items: self.items } }
}

impl<T: Id, U> Index<T> for SparseMapBuilder<T, U> {
	type Output = U;

	fn index(&self, id: T) -> &Self::Output { self.items.get(&id).unwrap() }
}

impl<T: Id, U> IndexMut<T> for SparseMapBuilder<T, U> {
	fn index_mut(&mut self, id: T) -> &mut Self::Output { self.items.get_mut(&id).unwrap() }
}