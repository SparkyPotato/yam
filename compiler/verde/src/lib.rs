#[cfg(feature = "serde")]
pub use serde;
pub use verde_derive::{db, query, storage, Pushable, Tracked};

pub use crate::internal::{storage::Id, Ctx, Db, Pushable, Tracked};

pub mod internal;
#[cfg(feature = "test")]
pub mod test;

#[cfg(feature = "tracing")]
macro_rules! span {
	(enter $($x:tt)*) => {
		let _e = crate::span!($($x)*).entered();
	};

	(trace, $($x:tt)*) => {
		tracing::span!(tracing::Level::TRACE, $($x)*)
	};
    (debug, $($x:tt)*) => {
		tracing::span!(tracing::Level::DEBUG, $($x)*)
	};
	(info, $($x:tt)*) => {
		tracing::span!(tracing::Level::INFO, $($x)*)
	};
	(warn, $($x:tt)*) => {
		tracing::span!(tracing::Level::WARN, $($x)*)
	};
	(error, $($x:tt)*) => {
		tracing::span!(tracing::Level::ERROR, $($x)*)
	};
}

#[cfg(not(feature = "tracing"))]
macro_rules! span {
	($($x:tt)*) => {{
		let x = crate::Span;
		x
	}};
}

use span;

#[cfg(feature = "tracing")]
macro_rules! event {
	(trace, $($x:tt)*) => {
		tracing::event!(tracing::Level::TRACE, $($x)*)
	};
    (debug, $($x:tt)*) => {
		tracing::event!(tracing::Level::DEBUG, $($x)*)
	};
	(info, $($x:tt)*) => {
		tracing::event!(tracing::Level::INFO, $($x)*)
	};
	(warn, $($x:tt)*) => {
		tracing::event!(tracing::Level::WARN, $($x)*)
	};
	(error, $($x:tt)*) => {
		tracing::event!(tracing::Level::ERROR, $($x)*)
	};
}

#[cfg(not(feature = "tracing"))]
macro_rules! event {
	($($x:tt)*) => {
		()
	};
}

use event;

#[cfg(not(feature = "tracing"))]
struct Span;

#[cfg(not(feature = "tracing"))]
impl Span {
	fn record<Q: ?Sized, V>(&self, _: &Q, _: V) -> &Self { self }

	fn enter(&self) {}
}
