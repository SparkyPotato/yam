use intern::{Id, Resolver, UnsizedInterner};
use lasso::{LassoResult, Rodeo, RodeoResolver, Spur};

pub fn spur_to_id(spur: Spur) -> Id<str> {
	// SAFETY: `Spur` and `Id` have the same (transparent) representation.
	unsafe { std::mem::transmute(spur) }
}

pub fn id_to_spur(id: Id<str>) -> Spur {
	// SAFETY: `Spur` and `Id` have the same (transparent) representation.
	unsafe { std::mem::transmute(id) }
}

pub struct TextIntern {
	inner: Rodeo,
}

impl TextIntern {
	pub fn new() -> Self { Self { inner: Rodeo::new() } }
}

pub struct TextResolver {
	inner: RodeoResolver,
}

impl UnsizedInterner<str> for TextIntern {
	type Resolver = TextResolver;

	fn intern(&mut self, value: &str) -> Id<str> { spur_to_id(self.inner.get_or_intern(value)) }

	fn intern_static(&mut self, value: &'static str) -> Id<str> { spur_to_id(self.inner.get_or_intern_static(value)) }

	fn freeze(self) -> Self::Resolver {
		TextResolver {
			inner: self.inner.into_resolver(),
		}
	}
}

impl Resolver<str> for TextIntern {
	fn resolve(&self, id: Id<str>) -> &str { self.inner.resolve(&id_to_spur(id)) }

	fn try_resolve(&self, id: Id<str>) -> Option<&str> { self.inner.try_resolve(&id_to_spur(id)) }
}

impl Resolver<str> for TextResolver {
	fn resolve(&self, id: Id<str>) -> &str { self.inner.resolve(&id_to_spur(id)) }

	fn try_resolve(&self, id: Id<str>) -> Option<&str> { self.inner.try_resolve(&id_to_spur(id)) }
}

impl cstree::interning::Interner<Spur> for TextIntern {
	fn get_or_intern(&mut self, val: &str) -> Spur { self.inner.get_or_intern(val) }

	fn try_get_or_intern(&mut self, val: &str) -> LassoResult<Spur> { self.inner.try_get_or_intern(val) }

	fn get_or_intern_static(&mut self, val: &'static str) -> Spur { self.inner.get_or_intern_static(val) }

	fn try_get_or_intern_static(&mut self, val: &'static str) -> LassoResult<Spur> {
		self.inner.try_get_or_intern_static(val)
	}
}

impl cstree::interning::Resolver<Spur> for TextIntern {
	fn resolve(&self, key: &Spur) -> &str { self.inner.resolve(key) }

	fn try_resolve(&self, key: &Spur) -> Option<&str> { self.inner.try_resolve(key) }

	unsafe fn resolve_unchecked(&self, key: &Spur) -> &str { self.inner.resolve_unchecked(key) }

	fn contains_key(&self, key: &Spur) -> bool { self.inner.contains_key(key) }

	fn len(&self) -> usize { self.inner.len() }
}

impl cstree::interning::Reader<Spur> for TextIntern {
	fn get(&self, val: &str) -> Option<Spur> { self.inner.get(val) }

	fn contains(&self, val: &str) -> bool { self.inner.contains(val) }
}

impl cstree::interning::Resolver<Spur> for TextResolver {
	fn resolve(&self, key: &Spur) -> &str { self.inner.resolve(key) }

	fn try_resolve(&self, key: &Spur) -> Option<&str> { self.inner.try_resolve(key) }

	unsafe fn resolve_unchecked(&self, key: &Spur) -> &str { self.inner.resolve_unchecked(key) }

	fn contains_key(&self, key: &Spur) -> bool { self.inner.contains_key(key) }

	fn len(&self) -> usize { self.inner.len() }
}
