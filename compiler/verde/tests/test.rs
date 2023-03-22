use std::sync::atomic::{AtomicBool, Ordering};

use verde::{db, query, storage, Db, Id, Pushable, Tracked};

#[derive(Copy, Clone, Eq, PartialEq, Hash, Tracked)]
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
	fn double(db: &dyn Db, id: Id<TrackedStruct>) -> TrackedStruct {
		if EXECUTED.load(Ordering::Relaxed) {
			panic!("double() was executed twice");
		}
		EXECUTED.store(true, Ordering::Relaxed);

		let s = db.get(id);
		TrackedStruct {
			id: s.id,
			value: s.value * 2,
		}
	}

	let mut db = Database::new();
	let id = db.set_input(TrackedStruct { id: 0, value: 1 });

	let doubled = double(&db, id);
	assert_eq!(db.get_ext(doubled).value, 2);

	let doubled = double(&db, id);
	assert_eq!(db.get_ext(doubled).value, 2);
}

#[test]
fn correct_result() {
	#[derive(Clone, Pushable)]
	struct Accum;

	#[storage]
	struct Storage(TrackedStruct, Accum, double, sum);

	#[db]
	struct Database(Storage);

	#[query]
	fn double(db: &dyn Db, id: Id<TrackedStruct>) -> TrackedStruct {
		db.push(Accum);
		let s = db.get(id);
		TrackedStruct {
			id: s.id,
			value: s.value * 2,
		}
	}

	#[query]
	fn sum(db: &dyn Db, id: u32, ids: Vec<Id<TrackedStruct>>) -> TrackedStruct {
		db.push(Accum);
		let mut value = 0;
		for x in ids.into_iter().map(|x| db.get(x)) {
			value += x.value;
		}
		TrackedStruct { id, value }
	}

	let mut db = Database::new();
	let init: Vec<_> = (1..=100)
		.map(|x| db.set_input(TrackedStruct { id: x, value: x }))
		.collect();

	let doubled: Vec<_> = init.iter().map(|x| double(&db, *x)).collect();
	let first_double_then_sum = sum(&db, 0, doubled);

	let first_sum = sum(&db, 1, init);
	let first_sum_then_double = double(&db, first_sum);

	let val1 = db.get_ext(first_double_then_sum).value;
	let val2 = db.get_ext(first_sum_then_double).value;
	assert_eq!(val1, val2,);
	assert_eq!(val1, 5050 * 2);

	let accums = db.get_all::<Accum>();
	assert_eq!(accums.len(), 103);
}
