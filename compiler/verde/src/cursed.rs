use std::{
	future::Future,
	pin::Pin,
	task::{Context, Poll},
};

use pin_project::pin_project;

use crate::{
	internal::{storage::Get, DbForQuery},
	Db,
	EndQueryFutureInner,
	GetFutureInner,
	Id,
	PushFutureInner,
	Pushable,
	Query,
	StartQueryFutureInner,
	Tracked,
};

#[must_use = "futures do nothing unless polled"]
#[pin_project]
pub struct StartQueryFuture<'a, T: Query>(#[pin] pub StartQueryFutureInner<'a, T>);
unsafe impl<T: Query> Send for StartQueryFuture<'_, T> {}
impl<'a, T: Query> Future for StartQueryFuture<'a, T> {
	type Output = DbForQuery<'a>;

	fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> { self.project().0.poll(cx) }
}

#[must_use = "futures do nothing unless polled"]
#[pin_project]
pub struct EndQueryFuture<'a, 'b, T: Query, F: Future<Output = T::Output> + 'b>(
	#[pin] pub EndQueryFutureInner<'a, 'b, T, F>,
);
unsafe impl<T: Query, F: Future<Output = T::Output>> Send for EndQueryFuture<'_, '_, T, F> {}
impl<'a, 'b, T: Query, F: Future<Output = T::Output> + 'a> Future for EndQueryFuture<'a, 'b, T, F> {
	type Output = Id<T::Output>;

	fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> { self.project().0.poll(cx) }
}

#[must_use = "futures do nothing unless polled"]
#[pin_project]
pub struct GetFuture<'a, T: Tracked>(#[pin] pub GetFutureInner<'a, T>);
unsafe impl<'a, T: Tracked> Send for GetFuture<'a, T> {}
impl<'a, T: Tracked> Future for GetFuture<'a, T> {
	type Output = Get<'a, T>;

	fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> { self.project().0.poll(cx) }
}

#[must_use = "futures do nothing unless polled"]
#[pin_project]
pub struct PushFuture<'a, T: Pushable>(#[pin] pub PushFutureInner<'a, T>);
unsafe impl<'a, T: Pushable> Send for PushFuture<'a, T> {}
impl<'a, T: Pushable> Future for PushFuture<'a, T> {
	type Output = ();

	fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> { self.project().0.poll(cx) }
}

pub struct DbWrapper(pub *const dyn Db);
impl DbWrapper {
	pub unsafe fn to_ref(&self) -> &dyn Db { unsafe { &*self.0 } }
}
unsafe impl Send for DbWrapper {}
