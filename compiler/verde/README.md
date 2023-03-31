# verde

A refreshingly simple incremental computation engine.

### What is incremental computation?

Incremental computation deals with a problem that seems rather simple:

Given a pure function `f` and some input `x`, compute `f(x)`, and *memoize* the output.
The next time, if `f` is executed with the same inputs, return the memoized output instead of recomputing it.
However, if the input changes, recompute the value, and repeat.

### The terminology

The central component of verde is a *database*. 
The database stores all the memoized data (*tracked* types), 
as well as track the state of each memoized function (a *query*).
Furthermore, the database also allows for interning values such that they are deduplicated, 
and comparison can be done by a simple ID check (*interned* types).
Finally, the database also allows for a query-safe 'side-channel' so that diagnostics can be collected without having to be a part of the output of a query (*pushable* types).

### Getting started

First, we must define the structs that verde will track. This can be done as so:
```rust ignore
#[derive(verde::Tracked, Eq, PartialEq)]
struct MyTrackedStruct {
    #[id]
    id: UniqueIdentifier,
    value: u32,
}
```
The field marked with `#[id]` is used as a unique identifier for each tracked type. 
It must be unique in every query (a query function must not produce a struct with the same ID when given different input, but different queries can).
Note that the `Eq` trait is required for `Tracked` to be implemented, and the ID must implement `Clone`, `Eq`, and `Hash`.

Next, we must define any pushable types.
```rust ignore
#[derive(verde::Pushable)]
struct MyPushableStruct {
    value: u32,
}
```
No other traits are required for `Pushable` to be implemented.

Interning is provided as a convenience feature:
```rust ignore
#[derive(verde::Interned, Eq, PartialEq, Hash)]
struct MyInternedStruct {
    value: u32,
}
```
`Clone`, `Eq`, and `Hash` are required to be implemented.

Finally, we must define the query functions.
```rust ignore
#[verde::query]
fn double(ctx: &verde::Ctx, input: verde::Id<MyTrackedStruct>) -> MyTrackedStruct {
    let interned = ctx.add(MyInternedStruct { value: 5 });
    assert_eq!(ctx.geti(interned).value, 5);
    ctx.push(MyPushableStruct { value: 5 });
    let input = ctx.get(input);
    MyTrackedStruct {
        id: input.id,
        value: input.value * 2,
    }
}
```
Queries are normal functions that must take a `&Ctx` as the first parameter. 
They can simply be called as normal functions, with verde handling all the incremental logical transparently.

However, before we can call our queries, we must first create our database:
```rust ignore
#[verde::storage]
struct Storage(MyTrackedStruct, MyInternedStruct, MyPushableStruct, double);

#[verde::db]
struct Database(Storage);
```
The `storage` attribute is used to create a *storage struct* that holds types that the database will know about.
Several storage structs composed together form a database. This multi-tiered approach allows for a modular design, 
where each crate can define a storage struct, with the driver crate only having to include them.

Finally, we can run our queries:
```rust ignore
fn main() {
    let mut db = Database::default();
    let db = &mut db as &mut dyn verde::Db;
    let input = db.set(MyTrackedStruct { id: UniqueIdentifier::new(), value: 5 });
    let output = db.execute(|ctx| double(ctx, input));
    assert_eq!(db.get(output).value, 10);
    let mut pushables = db.get_all::<MyPushableStruct>();
    assert_eq!(pushables.next().map(|x| x.value), Some(5));
    assert_eq!(pushables.next(), None);
}
```
