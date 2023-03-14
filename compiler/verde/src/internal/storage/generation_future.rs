use std::{
	future::Future,
	marker::PhantomData,
	mem::MaybeUninit,
	pin::Pin,
	task::{Context, Poll},
};

const SIZE: usize = 256;

#[repr(align(256))]
pub struct GenerationFuture<'a> {
	buf: [MaybeUninit<u8>; SIZE],
	poll: unsafe fn(*mut (), &mut Context<'_>) -> Poll<u64>,
	_phantom: PhantomData<&'a ()>,
}

impl<'a> GenerationFuture<'a> {
	pub fn new<F: Future<Output = u64>>(value: F) -> Self {
		if std::mem::size_of::<F>() > SIZE {
			panic!("Future is too large to fit in GenerationFuture");
		}

		if std::mem::align_of::<F>() > 256 {
			panic!("Future is aligned too high to fit in GenerationFuture");
		}

		Self {
			buf: unsafe {
				let mut buf = [MaybeUninit::uninit(); SIZE];
				let ptr = buf.as_mut_ptr() as *mut F;
				std::ptr::write(ptr, value);
				buf
			},
			poll: poll::<F>,
			_phantom: PhantomData,
		}
	}
}

impl Future for GenerationFuture<'_> {
	type Output = u64;

	fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
		unsafe {
			let this = self.get_unchecked_mut();
			(this.poll)(this.buf.as_mut_ptr() as _, cx)
		}
	}
}

unsafe fn poll<F: Future<Output = u64>>(value: *mut (), cx: &mut Context<'_>) -> Poll<u64> {
	unsafe {
		let ptr = value as *mut F;
		let future = Pin::new_unchecked(&mut *ptr);
		future.poll(cx)
	}
}
