use std::{
	future::Future,
	pin::Pin,
	task::{Context, Poll},
};

use pin_project::pin_project;

use crate::{internal::storage::Get, Db, EndQueryFutureInner, GetFutureInner, Id, Query, Tracked};

#[pin_project]
pub struct EndQueryFuture<'a, 'b, T: Query, F: Future<Output = T::Output> + 'b>(
	#[pin] pub EndQueryFutureInner<'a, 'b, T, F>,
);
unsafe impl<T: Query, F: Future<Output = T::Output>> Send for EndQueryFuture<'_, '_, T, F> {}
impl<'a, 'b, T: Query, F: Future<Output = T::Output> + 'a> Future for EndQueryFuture<'a, 'b, T, F> {
	type Output = Id<T::Output>;

	fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> { self.project().0.poll(cx) }
}

#[pin_project]
pub struct GetFuture<'a, T: Tracked>(#[pin] pub GetFutureInner<'a, T>);
unsafe impl<'a, T: Tracked> Send for GetFuture<'a, T> {}
impl<'a, T: Tracked> Future for GetFuture<'a, T> {
	type Output = Get<'a, T>;

	fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> { self.project().0.poll(cx) }
}

pub struct DbWrapper(pub *const dyn Db);
impl DbWrapper {
	pub unsafe fn to_ref(&self) -> &dyn Db { unsafe { &*self.0 } }
}
unsafe impl Send for DbWrapper {}
