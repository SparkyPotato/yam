use std::cell::RefCell;

use rustc_hash::FxHashMap;

use crate::{
	internal::{
		storage::{
			ErasedPushableStorage,
			ErasedQueryStorage,
			ErasedTrackedStorage,
			PushableStorage,
			QueryStorage,
			RouteBuilder,
			RoutingTable,
			RoutingTableBuilder,
			TrackedStorage,
		},
		Query,
		Storage,
	},
	Db,
	Pushable,
	Tracked,
};

pub enum StorageType {
	Tracked(Box<dyn ErasedTrackedStorage>),
	Query(Box<dyn ErasedQueryStorage>),
	Pushable(Box<dyn ErasedPushableStorage>),
}

impl<T: Tracked> From<TrackedStorage<T>> for StorageType {
	fn from(storage: TrackedStorage<T>) -> Self { Self::Tracked(Box::new(storage)) }
}

impl<Q: Query> From<QueryStorage<Q>> for StorageType {
	fn from(storage: QueryStorage<Q>) -> Self { Self::Query(Box::new(storage)) }
}

impl<T: Pushable> From<PushableStorage<T>> for StorageType {
	fn from(storage: PushableStorage<T>) -> Self { Self::Pushable(Box::new(storage)) }
}

pub struct TestDatabase {
	table: RoutingTable,
	storage: RefCell<FxHashMap<u16, StorageType>>,
}

impl TestDatabase {
	pub fn new() -> Self {
		Self {
			table: RoutingTableBuilder::default().finish(),
			storage: RefCell::new(FxHashMap::default()),
		}
	}

	fn make(&self) {
		for make in self.table.make() {
			let (storage, index) = make();
			self.storage.borrow_mut().insert(index, storage);
		}
	}
}

impl Db for TestDatabase {
	fn init_routing(_: &mut RoutingTableBuilder)
	where
		Self: Sized,
	{
	}

	fn routing_table(&self) -> &RoutingTable { &self.table }

	fn storage_struct(&self, storage: u16) -> &dyn Storage {
		assert_eq!(storage, 1);
		self
	}
}

impl Storage for TestDatabase {
	fn init_routing(_: &mut RouteBuilder)
	where
		Self: Sized,
	{
	}

	fn tracked_storage(&self, index: u16) -> Option<&dyn ErasedTrackedStorage> {
		// SAFETY: Uh I think it should be fine.
		self.make();
		match self.storage.borrow().get(&index) {
			Some(StorageType::Tracked(storage)) => Some(unsafe { std::mem::transmute(storage.as_ref()) }),
			_ => None,
		}
	}

	fn query_storage(&self, index: u16) -> Option<&dyn ErasedQueryStorage> {
		self.make();
		match self.storage.borrow().get(&index) {
			Some(StorageType::Query(storage)) => Some(unsafe { std::mem::transmute(storage.as_ref()) }),
			_ => None,
		}
	}

	fn pushable_storage(&self, index: u16) -> Option<&dyn ErasedPushableStorage> {
		self.make();
		match self.storage.borrow().get(&index) {
			Some(StorageType::Pushable(storage)) => Some(unsafe { std::mem::transmute(storage.as_ref()) }),
			_ => None,
		}
	}
}