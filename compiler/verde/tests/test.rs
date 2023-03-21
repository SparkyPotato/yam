#![feature(ptr_metadata)]
#![feature(type_alias_impl_trait)]

use std::sync::atomic::{AtomicBool, Ordering};

use futures::stream::{self, StreamExt};
use serde::{Deserialize, Serialize};
use verde::{db, query, storage, Db, Id, Pushable, Tracked};

#[derive(Copy, Clone, Eq, PartialEq, Hash, Serialize, Deserialize, Tracked)]
struct TrackedStruct {
	#[id]
	id: u32,
	value: u32,
}

#[test]
fn doesnt_execute_twice() {
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

	let mut db = Database::new();
	let id = db.set_input(TrackedStruct { id: 0, value: 1 });

	let doubled = db.block_on(db.execute(double(&db, id)));
	assert_eq!(db.get_ext(doubled).value, 2);

	let doubled = db.block_on(db.execute(double(&db, id)));
	assert_eq!(db.get_ext(doubled).value, 2);
}

#[test]
fn correct_result() {
	#[derive(Clone, Pushable, Serialize, Deserialize)]
	struct Accum;

	#[storage]
	struct Storage(TrackedStruct, Accum, double, sum);

	#[db]
	struct Database(Storage);

	#[query]
	async fn double(db: &dyn Db, id: Id<TrackedStruct>) -> TrackedStruct {
		db.push(Accum).await;
		let s = db.get(id).await;
		TrackedStruct {
			id: s.id,
			value: s.value * 2,
		}
	}

	#[query]
	async fn sum(db: &dyn Db, id: u32, ids: Vec<Id<TrackedStruct>>) -> TrackedStruct {
		db.push(Accum).await;
		let val = stream::iter(ids)
			.then(|x| db.get(x))
			.fold(0, |acc, x| async move { acc + x.value });
		TrackedStruct { id, value: val.await }
	}

	let mut db = Database::new();
	let init: Vec<_> = (1..=100)
		.map(|x| db.set_input(TrackedStruct { id: x, value: x }))
		.collect();

	let doubled: Vec<_> = init
		.iter()
		.map(|x| db.execute(double(&db, *x)))
		.map(|x| db.block_on(x))
		.collect();
	let first_double_then_sum = db.block_on(db.execute(sum(&db, 0, doubled)));

	let first_sum = db.block_on(db.execute(sum(&db, 1, init)));
	let first_sum_then_double = db.block_on(db.execute(double(&db, first_sum)));

	let val1 = db.get_ext(first_double_then_sum).value;
	let val2 = db.get_ext(first_sum_then_double).value;
	assert_eq!(val1, val2,);
	assert_eq!(val1, 5050 * 2);

	let accums = db.get_all::<Accum>();
	assert_eq!(accums.len(), 103);
}
