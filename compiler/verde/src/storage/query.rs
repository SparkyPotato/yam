use std::future::Future;

use rustc_hash::FxHashSet;

use crate::{
	storage::{routing::Route, DashMap, ErasedId},
	DbForQuery,
	Id,
	Query,
};

pub trait ErasedQueryStorage {}

impl<'a> dyn ErasedQueryStorage + 'a {
	/// **Safety**: The type of `self` must be `QueryStorage<T>`.
	pub async unsafe fn execute<T, F>(
		&self, query: Route, input: T::Input, ctx: &DbForQuery<'_>, fut: F,
	) -> Id<T::Output>
	where
		T: Query,
		F: Future<Output = T::Output>,
	{
		unsafe {
			let storage = self as *const dyn ErasedQueryStorage as *const QueryStorage<T>;
			(*storage).execute(query, input, ctx, fut).await
		}
	}
}

pub struct QueryStorage<T: Query> {
	map: DashMap<T::Input, QueryData<T>>,
}

impl<T: Query> ErasedQueryStorage for QueryStorage<T> {}

impl<T: Query> QueryStorage<T> {
	pub async fn execute<F: Future<Output = T::Output>>(
		&self, query: Route, input: T::Input, ctx: &DbForQuery<'_>, fut: F,
	) -> Id<T::Output> {
		match self.map.get(&input) {
			Some(data) => {
				let output_generation = ctx.db.get_generation(data.output).await;
				for &dep in data.dependencies.iter() {
					if output_generation < ctx.db.get_generation_erased(dep).await {
						let ret = fut.await;
						let dependencies = ctx.dependencies.lock().unwrap().take().unwrap();
						let output = ctx.db.insert(query, ret).await;
						self.map.insert(input, QueryData { dependencies, output });
						return output;
					}
				}
				data.output
			},
			None => {
				let ret = fut.await;
				let dependencies = ctx.dependencies.lock().unwrap().take().unwrap();
				let output = ctx.db.insert(query, ret).await;
				self.map.insert(input, QueryData { dependencies, output });
				output
			},
		}
	}
}

struct QueryData<T: Query> {
	dependencies: FxHashSet<ErasedId>,
	output: Id<T::Output>,
}

impl<T: Query> Default for QueryStorage<T> {
	fn default() -> Self {
		Self {
			map: DashMap::default(),
		}
	}
}
