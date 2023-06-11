use std::{
	fmt::Debug,
	hash::Hash,
	marker::PhantomData,
	num::NonZeroU32,
	ops::{Index, IndexMut},
};

pub mod dense;
pub mod sparse;

/// An index into an arena.
pub struct Ix<T>(NonZeroU32, PhantomData<fn() -> T>);

impl<T> Ix<T> {
	pub fn new(index: usize) -> Self { Self(NonZeroU32::new(index as u32 + 1).expect("Arena overflow"), PhantomData) }

	pub fn index(self) -> usize { self.0.get() as usize - 1 }
}

/// An arena of elements of type `T`.
#[derive(Clone, Eq, PartialEq)]
pub struct Arena<T> {
	elems: Vec<T>,
}

impl<T> Arena<T> {
	pub fn new() -> Self { Self { elems: Vec::new() } }

	pub fn push(&mut self, elem: T) -> Ix<T> {
		let ix = self.elems.len();
		self.elems.push(elem);
		Ix::new(ix)
	}
}

impl<T> Index<Ix<T>> for Arena<T> {
	type Output = T;

	fn index(&self, ix: Ix<T>) -> &Self::Output { self.elems.index(ix.index()) }
}

impl<T> IndexMut<Ix<T>> for Arena<T> {
	fn index_mut(&mut self, ix: Ix<T>) -> &mut Self::Output { self.elems.index_mut(ix.index()) }
}

impl<T> Default for Arena<T> {
	fn default() -> Self { Self::new() }
}

impl<T: Debug> Debug for Arena<T> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { self.elems.fmt(f) }
}

impl<T> Clone for Ix<T> {
	fn clone(&self) -> Self { *self }
}
impl<T> Copy for Ix<T> {}

impl<T> PartialEq for Ix<T> {
	fn eq(&self, other: &Self) -> bool { self.0 == other.0 }
}
impl<T> Eq for Ix<T> {}

impl<T> Hash for Ix<T> {
	fn hash<H: std::hash::Hasher>(&self, state: &mut H) { self.0.hash(state) }
}

impl<T> Debug for Ix<T> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "Ix<{}>({})", std::any::type_name::<T>(), self.0)
	}
}
