use std::hash::BuildHasherDefault;

use rustc_hash::FxHasher;

mod stack_future;
mod pushable;
mod query;
mod routing;
mod tracked;

pub use pushable::{ErasedPushableStorage, PushableStorage};
pub use query::{ErasedQueryId, ErasedQueryStorage, QueryStorage};
pub use routing::{Route, RouteBuilder, RoutingTable, RoutingTableBuilder};
pub use tracked::{ErasedId, ErasedTrackedStorage, Get, Id, TrackedStorage};

type DashMap<K, V> = dashmap::DashMap<K, V, BuildHasherDefault<FxHasher>>;
