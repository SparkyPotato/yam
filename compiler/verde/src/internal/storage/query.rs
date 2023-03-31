use parking_lot::{Mutex, RwLock};
use rustc_hash::FxHashSet;

use crate::{
	event,
	internal::{
		storage::{routing::Route, DashMap, ErasedId},
		Ctx,
		Query,
	},
	span,
	Id,
};

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct ErasedQueryId {
	pub(crate) index: u32,
	pub(crate) route: Route,
}

pub trait ErasedQueryStorage {}

impl<'a> dyn ErasedQueryStorage + 'a {
	pub unsafe fn start_query<T: Query>(&self, input: T::Input) -> u32 {
		unsafe {
			let storage = self as *const dyn ErasedQueryStorage as *const QueryStorage<T>;
			(*storage).start_query(input)
		}
	}

	/// **Safety**: The type of `self` must be `QueryStorage<T>`.
	pub unsafe fn execute<T: Query>(&self, ctx: &Ctx, f: impl FnOnce() -> T::Output) -> Id<T::Output> {
		unsafe {
			let storage = self as *const dyn ErasedQueryStorage as *const QueryStorage<T>;
			(*storage).execute(ctx, f)
		}
	}
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct QueryStorage<T: Query> {
	pub(crate) map: DashMap<T::Input, u32>,
	pub(crate) values: RwLock<Vec<Mutex<QueryData<T>>>>,
}

impl<T: Query> ErasedQueryStorage for QueryStorage<T> {}

impl<T: Query> QueryStorage<T> {
	pub fn start_query(&self, input: T::Input) -> u32 {
		match self.map.get(&input) {
			Some(index) => *index,
			None => {
				let mut values = self.values.write();
				let index = values.len() as u32;
				values.push(Mutex::new(QueryData {
					dependencies: Default::default(),
					output: None,
				}));
				self.map.insert(input, index);
				index
			},
		}
	}

	pub fn execute(&self, ctx: &Ctx, f: impl FnOnce() -> T::Output) -> Id<T::Output> {
		span!(enter trace, "fetch query output", query = std::any::type_name::<T>());

		let f = || {
			span!(enter trace, "execute query", query = std::any::type_name::<T>());
			f()
		};
		let query = ctx.curr_query.route;
		let index = ctx.curr_query.index;
		let values = self.values.read();
		let mut data = values[index as usize].lock();

		match data.output {
			Some(id) => {
				event!(debug, "query cache hit");
				for &(dep, gen) in data.dependencies.iter() {
					let dep_generation = ctx.get_generation(dep);
					if dep_generation > gen {
						event!(debug, "dependencies have changed, re-executing",);
						let ret = f();
						let output = ctx.db.insert(query, ret);
						let dependencies = unsafe { ctx.dependencies.borrow_mut().assume_init_read() };
						*data = QueryData {
							dependencies,
							output: Some(output),
						};
						return output;
					}
				}
				let _ = unsafe { ctx.dependencies.borrow_mut().assume_init_read() };
				id
			},
			None => {
				event!(debug, "first query execution");
				let ret = f();
				let output = ctx.db.insert(query, ret);
				let dependencies = unsafe { ctx.dependencies.borrow_mut().assume_init_read() };
				*data = QueryData {
					dependencies,
					output: Some(output),
				};
				output
			},
		}
	}
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub(crate) struct QueryData<T: Query> {
	pub(crate) dependencies: FxHashSet<(ErasedId, u64)>,
	pub(crate) output: Option<Id<T::Output>>,
}

impl<T: Query> Default for QueryStorage<T> {
	fn default() -> Self {
		Self {
			map: DashMap::default(),
			values: Default::default(),
		}
	}
}
