use std::{fmt, hash};

mod indexed_vec;
mod macros;

pub use crate::indexed_vec::*;

pub trait Idx: fmt::Debug + fmt::Display + Copy + Eq + hash::Hash {
    fn new(index: usize) -> Self;
    fn index(self) -> usize;

    fn increment_mut(&mut self, n: usize) {
        *self = self.increment(n)
    }

    fn increment(self, n: usize) -> Self {
        Self::new(self.index() + n)
    }
}

#[derive(Debug, Default)]
pub struct Generation<I> {
    current: I,
}

impl<I: Idx> Generation<I> {
    pub fn new() -> Generation<I> {
        Generation { current: I::new(0) }
    }

    pub fn next(&mut self) -> I {
        let id = self.current;
        self.current = I::new(self.current.index() + 1);
        id
    }
}

impl<I: Idx> fmt::Display for Generation<I> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Generation({})", self.current)
    }
}
