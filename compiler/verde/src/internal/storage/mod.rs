use std::hash::BuildHasherDefault;

use rustc_hash::FxHasher;

pub mod interned;
mod pushable;
mod query;
mod routing;
pub mod tracked;

pub use interned::*;
pub use pushable::*;
pub use query::*;
pub use routing::*;
pub use tracked::*;

pub(crate) type DashMap<K, V> = dashmap::DashMap<K, V, BuildHasherDefault<FxHasher>>;
