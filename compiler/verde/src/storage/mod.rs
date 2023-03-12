use std::hash::BuildHasherDefault;

use rustc_hash::FxHasher;

mod query;
pub mod routing;
mod tracked;

pub use query::{ErasedQueryStorage, QueryStorage};
pub use tracked::{ErasedId, ErasedTrackedStorage, Get, Id, TrackedStorage};

type DashMap<K, V> = dashmap::DashMap<K, V, BuildHasherDefault<FxHasher>>;
