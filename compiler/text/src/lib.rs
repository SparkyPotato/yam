use std::fmt::Debug;

use once_cell::sync::Lazy;

pub type Interner = lasso::ThreadedRodeo;

static INTERNER: Lazy<Interner> = Lazy::new(Interner::new);

// Do not impl `Deref` to avoid excessive contention of `INTERNER`.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Default)]
pub struct Text(lasso::Spur);

impl Debug for Text {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { f.write_str(self.as_str()) }
}

impl Text {
	pub fn new(text: &str) -> Self { Self(INTERNER.get_or_intern(text)) }

	pub fn as_str(&self) -> &'static str { INTERNER.resolve(&self.0) }
}

pub fn get_interner() -> &'static Interner { &INTERNER }
