use std::{fmt::Debug, hash::Hash, marker::PhantomData, num::NonZeroU32};

#[repr(transparent)]
pub struct Id<T: ?Sized>(NonZeroU32, PhantomData<T>);

impl<T: ?Sized> Default for Id<T> {
	fn default() -> Self { Self(NonZeroU32::new(1).unwrap(), PhantomData) }
}

impl<T: ?Sized> Clone for Id<T> {
	fn clone(&self) -> Self { Id(self.0, PhantomData) }
}

impl<T: ?Sized> Copy for Id<T> {}

impl<T: ?Sized> PartialEq for Id<T> {
	fn eq(&self, other: &Self) -> bool { self.0 == other.0 }
}

impl<T: ?Sized> Eq for Id<T> {}

impl<T: ?Sized> Hash for Id<T> {
	fn hash<H: std::hash::Hasher>(&self, state: &mut H) { self.0.hash(state) }
}

impl<T: ?Sized> Debug for Id<T> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { write!(f, "Id({})", self.0) }
}

impl<T: ?Sized> Id<T> {
	pub fn new(id: u32) -> Self { Self(NonZeroU32::new(id).expect("expected non-zero id"), PhantomData) }

	pub fn try_new(id: u32) -> Option<Self> { NonZeroU32::new(id).map(|x| Self(x, PhantomData)) }

	pub fn from_inner(id: NonZeroU32) -> Self { Self(id, PhantomData) }

	pub fn into_inner(self) -> NonZeroU32 { self.0 }

	pub fn value(self) -> u32 { self.0.get() }
}

pub trait UnsizedInterner<V: ?Sized>: Resolver<V> {
	type Resolver: Resolver<V>;

	fn intern(&mut self, value: &V) -> Id<V>;

	fn intern_static(&mut self, value: &'static V) -> Id<V>;

	fn freeze(self) -> Self::Resolver;
}

pub trait Resolver<V: ?Sized> {
	fn resolve(&self, id: Id<V>) -> &V;

	fn try_resolve(&self, id: Id<V>) -> Option<&V>;
}
