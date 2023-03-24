use std::{marker::PhantomData, num::NonZeroU64};

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
				let output_generation = ctx.get_generation(id.get().inner);
				event!(debug, "output has generation `{}`", output_generation);
				let mut max_dep_generation = 0;
				for &dep in data.dependencies.iter() {
					let dep_generation = ctx.get_generation(dep);
					max_dep_generation = max_dep_generation.max(dep_generation);
				}
				if output_generation < max_dep_generation {
					event!(
						debug,
						"dependencies have generation `{}`, re-executing",
						max_dep_generation
					);
					let ret = f();
					let output = ctx.db.insert(query, ret, Some(max_dep_generation));
					let dependencies = unsafe { ctx.dependencies.borrow_mut().assume_init_read() };
					*data = QueryData {
						dependencies,
						output: Some(OutputId::new(output)),
					};
					return output;
				}
				id.get()
			},
			None => {
				event!(debug, "first query execution");
				let ret = f();
				let mut max_dep_generation = 0;
				for &dep in data.dependencies.iter() {
					let dep_generation = ctx.get_generation(dep);
					max_dep_generation = max_dep_generation.max(dep_generation);
				}
				let output = ctx.db.insert(query, ret, Some(max_dep_generation));
				let dependencies = unsafe { ctx.dependencies.borrow_mut().assume_init_read() };
				*data = QueryData {
					dependencies,
					output: Some(OutputId::new(output)),
				};
				output
			},
		}
	}
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub(crate) struct QueryData<T: Query> {
	pub(crate) dependencies: FxHashSet<ErasedId>,
	pub(crate) output: Option<OutputId<T::Output>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub(crate) struct OutputId<T>(NonZeroU64, PhantomData<T>);

impl<T> Copy for OutputId<T> {}
impl<T> Clone for OutputId<T> {
	fn clone(&self) -> Self { *self }
}

impl<T> OutputId<T> {
	pub fn new(id: Id<T>) -> Self {
		let index = id.inner.index;
		let route_storage = id.inner.route.storage;
		let route_index = id.inner.route.index;
		Self(
			NonZeroU64::new((index as u64) | ((route_index as u64) << 32) | ((route_storage as u64) << 48)).unwrap(),
			PhantomData,
		)
	}

	pub fn get(self) -> Id<T> {
		let index = self.0.get() as u32;
		let route_storage = (self.0.get() >> 48) as u16;
		let route_index = (self.0.get() >> 32) as u16;
		Id {
			inner: ErasedId {
				index,
				route: Route {
					storage: route_storage,
					index: route_index,
				},
			},
			_phantom: PhantomData,
		}
	}
}

impl<T: Query> Default for QueryStorage<T> {
	fn default() -> Self {
		Self {
			map: DashMap::default(),
			values: Default::default(),
		}
	}
}

#[cfg(test)]
mod tests {
	use std::marker::PhantomData;

	use crate::internal::storage::{ErasedId, Route};

	#[test]
	fn output_id() {
		use super::OutputId;
		use crate::Id;

		let id = Id::<i32> {
			inner: ErasedId {
				index: 1,
				route: Route { storage: 2, index: 3 },
			},
			_phantom: PhantomData,
		};
		let output_id = OutputId::new(id);
		let ret = output_id.get();
		assert_eq!(ret.inner.index, 1);
		assert_eq!(ret.inner.route.storage, 2);
		assert_eq!(ret.inner.route.index, 3);
	}
}
