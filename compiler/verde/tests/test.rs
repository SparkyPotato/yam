#![feature(ptr_metadata)]
#![feature(type_alias_impl_trait)]

use std::sync::atomic::{AtomicBool, Ordering};

use verde::{db, query, storage, Db, Id, Tracked};

#[derive(Eq, PartialEq, Tracked)]
struct TrackedStruct {
	#[id]
	id: u32,
	value: u32,
}

#[storage]
struct Storage(TrackedStruct, double);

#[db]
struct Database(Storage);

static EXECUTED: AtomicBool = AtomicBool::new(false);

#[query]
async fn double(db: &dyn Db, id: Id<TrackedStruct>) -> TrackedStruct {
	if EXECUTED.load(Ordering::Relaxed) {
		panic!("double() was executed twice");
	}
	EXECUTED.store(true, Ordering::Relaxed);

	let s = db.get(id).await;
	TrackedStruct {
		id: s.id,
		value: s.value * 2,
	}
}

#[test]
fn main() {
	let mut db = Database::default();
	let id = db.set_input(TrackedStruct { id: 0, value: 1 });

	let doubled = db.block_on(db.execute(double(&db, id)));
	assert_eq!(db.get_ext(doubled).value, 2);

	let doubled = db.block_on(db.execute(double(&db, id)));
	assert_eq!(db.get_ext(doubled).value, 2);
}
