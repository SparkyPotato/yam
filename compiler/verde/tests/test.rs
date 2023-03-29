use std::sync::atomic::{AtomicBool, Ordering};

use verde::{db, query, storage, test::TestDatabase, Ctx, Db, Id, Pushable, Tracked};

#[derive(Copy, Clone, Eq, PartialEq, Hash, Tracked)]
#[cfg_attr(feature = "serde", derive(::verde::serde::Serialize, ::verde::serde::Deserialize))]
struct TrackedStruct {
	#[id]
	id: u32,
	value: u32,
}

#[derive(Clone, Pushable)]
#[cfg_attr(feature = "serde", derive(::verde::serde::Serialize, ::verde::serde::Deserialize))]
struct Accum;

#[query]
fn double(db: &Ctx, id: Id<TrackedStruct>) -> TrackedStruct {
	db.push(Accum);
	let s = db.get(id);
	TrackedStruct {
		id: s.id,
		value: s.value * 2,
	}
}

#[query]
fn sum(db: &Ctx, id: u32, ids: Vec<Id<TrackedStruct>>) -> TrackedStruct {
	db.push(Accum);
	let mut value = 0;
	for x in ids.into_iter().map(|x| db.get(x)) {
		value += x.value;
	}
	TrackedStruct { id, value }
}

#[test]
fn correct_result() {
	#[storage]
	struct Storage(TrackedStruct, Accum, double, sum);

	#[db]
	struct Database(Storage);

	let mut db = Database::default();
	let db = &mut db as &mut dyn Db;
	let init: Vec<_> = (1..=100)
		.map(|x| db.set_input(TrackedStruct { id: x, value: x }))
		.collect();

	db.execute(|ctx| {
		let doubled: Vec<_> = init.iter().map(|x| double(ctx, *x)).collect();
		let first_double_then_sum = sum(ctx, 0, doubled);

		let first_sum = sum(ctx, 1, init);
		let first_sum_then_double = double(ctx, first_sum);

		let val1 = db.get(first_double_then_sum).value;
		let val2 = db.get(first_sum_then_double).value;
		assert_eq!(val1, val2);
		assert_eq!(val1, 5050 * 2);

		let accums = db.get_all::<Accum>();
		assert_eq!(accums.count(), 103);
		let double_accums = db.get_query::<double, Accum>();
		assert_eq!(double_accums.count(), 101);
		let sum_accums = db.get_query::<sum, Accum>();
		assert_eq!(sum_accums.count(), 2);
	});
}

#[test]
fn simple_memoize() {
	static EXECUTED: AtomicBool = AtomicBool::new(false);

	#[query]
	fn double(ctx: &Ctx, id: Id<TrackedStruct>) -> TrackedStruct {
		if EXECUTED.load(Ordering::Relaxed) {
			panic!("double() was executed twice");
		}
		EXECUTED.store(true, Ordering::Relaxed);

		let s = ctx.get(id);
		TrackedStruct {
			id: s.id,
			value: s.value * 2,
		}
	}

	let mut db = TestDatabase::new();
	let db = &mut db as &mut dyn Db;
	let id = db.set_input(TrackedStruct { id: 0, value: 1 });

	db.execute(|ctx| {
		let check = || {
			let doubled = double(ctx, id);
			assert_eq!(db.get(doubled).value, 2);
		};

		check();
		check();
	});
}

#[test]
fn memoizes_after_old_dependency() {
	static EXECUTED: AtomicBool = AtomicBool::new(false);
	#[query]
	fn triple(ctx: &Ctx, id: Id<TrackedStruct>) -> TrackedStruct {
		if EXECUTED.load(Ordering::Relaxed) {
			panic!("triple() was executed twice");
		}
		EXECUTED.store(true, Ordering::Relaxed);

		let s = ctx.get(id);
		TrackedStruct {
			id: s.id,
			value: s.value * 3,
		}
	}

	let mut db = TestDatabase::new();
	let db = &mut db as &mut dyn Db;

	let input = db.set_input(TrackedStruct { id: 0, value: 1 });

	let first = db.execute(|ctx| double(ctx, input));
	let trip = db.execute(|ctx| triple(ctx, first));
	assert_eq!(db.get(trip).value, 6);
	assert_eq!(input, db.set_input(TrackedStruct { id: 0, value: 2 }));

	let dub = || db.execute(|ctx| double(ctx, input));
	let second = dub();
	let third = dub();
	assert_eq!(db.get(second).value, db.get(third).value);

	EXECUTED.store(false, Ordering::Relaxed);
	let check = || {
		let trip = db.execute(|ctx| triple(ctx, third));
		assert_eq!(db.get(trip).value, 12);
	};
	check();
	check();
}

#[test]
fn executes_after_old_dependency() {
	let mut db = TestDatabase::new();
	let db = &mut db as &mut dyn Db;

	let mut input1 = db.set_input(TrackedStruct { id: 1, value: 1 });
	let mut doubled = db.execute(|ctx| double(ctx, input1));

	let mut input2 = db.set_input(TrackedStruct { id: 2, value: 2 });
	let mut s = db.execute(|ctx| sum(ctx, 0, vec![doubled, input2]));
	assert_eq!(db.get(s).value, 4);

	for value in 2..=10 {
		input1 = db.set_input(TrackedStruct { id: 1, value });
		doubled = db.execute(|ctx| double(ctx, input1));
		s = db.execute(|ctx| sum(ctx, 0, vec![doubled, input2]));
		assert_eq!(db.get(s).value, value * 2 + 2);
	}

	input2 = db.set_input(TrackedStruct { id: 2, value: 1 });
	let trip = db.execute(|ctx| sum(ctx, 0, vec![doubled, input2]));
	assert_eq!(db.get(trip).value, 21);
}
