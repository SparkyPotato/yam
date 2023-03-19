use std::future::Future;

use rustc_hash::FxHashSet;
use tokio::sync::{Mutex, RwLock};

use crate::{
	internal::storage::{routing::Route, DashMap, ErasedId},
	DbForQuery,
	Id,
	Query,
};

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct ErasedQueryId {
	pub(crate) index: u32,
	pub(crate) route: Route,
}

pub trait ErasedQueryStorage {}

impl<'a> dyn ErasedQueryStorage + 'a {
	pub async unsafe fn start_query<T: Query>(&self, input: T::Input) -> u32 {
		unsafe {
			let storage = self as *const dyn ErasedQueryStorage as *const QueryStorage<T>;
			(*storage).start_query(input).await
		}
	}

	/// **Safety**: The type of `self` must be `QueryStorage<T>`.
	pub async unsafe fn execute<T, F>(&self, ctx: &DbForQuery<'_>, fut: F) -> Id<T::Output>
	where
		T: Query,
		F: Future<Output = T::Output>,
	{
		unsafe {
			let storage = self as *const dyn ErasedQueryStorage as *const QueryStorage<T>;
			(*storage).execute(ctx, fut).await
		}
	}
}

pub struct QueryStorage<T: Query> {
	id_map: DashMap<T::Input, u32>,
	values: RwLock<Vec<Mutex<QueryData<T>>>>,
}

impl<T: Query> ErasedQueryStorage for QueryStorage<T> {}

impl<T: Query> QueryStorage<T> {
	pub async fn start_query(&self, input: T::Input) -> u32 {
		match self.id_map.get(&input) {
			Some(index) => *index,
			None => {
				let mut values = self.values.write().await;
				let index = values.len() as u32;
				values.push(Mutex::new(QueryData {
					dependencies: Default::default(),
					output: None,
				}));
				self.id_map.insert(input, index);
				index
			},
		}
	}

	pub async fn execute<F: Future<Output = T::Output>>(&self, ctx: &DbForQuery<'_>, fut: F) -> Id<T::Output> {
		let query = ctx.curr_query.route;
		let index = ctx.curr_query.index;
		let values = self.values.read().await;
		let mut data = values[index as usize].lock().await;

		match data.output {
			Some(id) => {
				let output_generation = ctx.db.get_generation(id).await;
				for &dep in data.dependencies.iter() {
					if output_generation < ctx.db.get_generation_erased(dep).await {
						let ret = fut.await;
						let dependencies = ctx.dependencies.lock().unwrap().take().unwrap();
						let output = ctx.db.insert(query, ret).await;
						*data = QueryData {
							dependencies,
							output: Some(output),
						};
						return output;
					}
				}
				id
			},
			None => {
				let ret = fut.await;
				let dependencies = ctx.dependencies.lock().unwrap().take().unwrap();
				let output = ctx.db.insert(query, ret).await;
				*data = QueryData {
					dependencies,
					output: Some(output),
				};
				output
			},
		}
	}
}

struct QueryData<T: Query> {
	dependencies: FxHashSet<ErasedId>,
	output: Option<Id<T::Output>>,
}

impl<T: Query> Default for QueryStorage<T> {
	fn default() -> Self {
		Self {
			id_map: DashMap::default(),
			values: Default::default(),
		}
	}
}
