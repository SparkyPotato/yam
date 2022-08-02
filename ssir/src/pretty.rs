use std::fmt::{Result, Write};

use crate::ssir::*;

pub struct SsirWriter<'a, T> {
	ssir: &'a Ssir,
	w: &'a mut T,
}

impl<'a, T: Write> SsirWriter<'a, T> {
	pub fn new(ssir: &'a Ssir, w: &'a mut W) -> Self { Self { ssir, w } }
}

impl<T: Write> SsirWriter<'_, T> {
	pub fn write(&mut self) -> Result {
        for global in self.ssir.
    }
}
