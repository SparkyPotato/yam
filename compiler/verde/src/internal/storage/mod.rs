use std::hash::BuildHasherDefault;

use rustc_hash::FxHasher;

mod pushable;
mod query;
mod routing;
mod tracked;

pub use pushable::*;
pub use query::*;
pub use routing::*;
pub use tracked::*;

pub(crate) type DashMap<K, V> = dashmap::DashMap<K, V, BuildHasherDefault<FxHasher>>;
