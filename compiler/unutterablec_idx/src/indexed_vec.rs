use std::marker::PhantomData;
use std::ops;

use crate::Generation;
use crate::Idx;

#[derive(Debug, Clone)]
pub struct IndexedVec<I, T> {
    inner: Vec<T>,
    marker: PhantomData<fn(I)>,
}

pub type Values<'a, T> = std::slice::Iter<'a, T>;

pub struct Iter<'a, I, T> {
    inner: &'a IndexedVec<I, T>,
    gen: Generation<I>,
}

impl<I: Idx, T> IndexedVec<I, T> {
    pub fn new() -> IndexedVec<I, T> {
        IndexedVec {
            inner: Vec::new(),
            marker: PhantomData,
        }
    }

    pub fn with_capacity(capacity: usize) -> IndexedVec<I, T> {
        IndexedVec {
            inner: Vec::with_capacity(capacity),
            marker: PhantomData,
        }
    }

    pub fn allocate_id<F>(&mut self, f: F) -> I
    where
        F: FnOnce(I) -> T,
    {
        let id = I::new(self.inner.len());
        self.inner.push(f(id));
        id
    }

    pub fn allocate<F>(&mut self, f: F) -> &T
    where
        F: FnOnce(I) -> T,
    {
        let id = self.allocate_id(f);
        &self[id]
    }

    pub fn push(&mut self, value: T) -> I {
        self.allocate_id(|_| value)
    }

    pub fn get(&self, id: I) -> Option<&T> {
        self.inner.get(id.index())
    }

    pub fn get_mut(&mut self, id: I) -> Option<&mut T> {
        self.inner.get_mut(id.index())
    }

    pub fn iter<'a>(&'a self) -> Iter<'a, I, T> {
        Iter {
            inner: self,
            gen: Generation::new(),
        }
    }

    pub fn values<'a>(&'a self) -> Values<'a, T> {
        self.inner.iter()
    }

    pub fn last_id(&self) -> Option<I> {
        if self.is_empty() {
            None
        } else {
            Some(I::new(self.len()))
        }
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    pub fn last(&self) -> Option<&T> {
        self.inner.last()
    }
}

impl<I: Idx, T> Default for IndexedVec<I, T> {
    fn default() -> Self {
        IndexedVec::new()
    }
}

impl<I: Idx, T> ops::Index<I> for IndexedVec<I, T> {
    type Output = T;

    fn index(&self, index: I) -> &Self::Output {
        &self.inner[index.index()]
    }
}

impl<I: Idx, T> ops::IndexMut<I> for IndexedVec<I, T> {
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        &mut self.inner[index.index()]
    }
}

impl<'a, I: Idx, T> Iterator for Iter<'a, I, T> {
    type Item = (I, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        let id = self.gen.next();
        Some((id, self.inner.get(id)?))
    }
}

impl<'a, I: Idx, T> IntoIterator for &'a IndexedVec<I, T> {
    type Item = (I, &'a T);

    type IntoIter = Iter<'a, I, T>;

    fn into_iter(self) -> Self::IntoIter {
        Iter {
            inner: &self,
            gen: Generation::new(),
        }
    }
}

impl<I: Idx, T> FromIterator<T> for IndexedVec<I, T> {
    fn from_iter<Iter: IntoIterator<Item = T>>(iter: Iter) -> Self {
        let mut ivec = IndexedVec::new();

        for item in iter {
            ivec.push(item);
        }

        ivec
    }
}

#[cfg(test)]
mod tests {
    use crate::*;

    newindex!(TestId);

    #[derive(Debug, PartialEq, Eq)]
    struct Test {
        id: TestId,
        value: u32,
    }

    impl Test {
        fn new(id: TestId, value: u32) -> Test {
            Test { id, value }
        }
    }

    #[test]
    fn get() {
        let mut vec = IndexedVec::new();

        let id = vec.allocate_id(|id| Test::new(id, 5));
        assert_eq!(vec.get(id), Some(&Test::new(id, 5)));
    }

    #[test]
    fn get_mut() {
        let mut vec = IndexedVec::new();

        let id = vec.allocate_id(|id| Test::new(id, 5));
        vec.get_mut(id).map(|t| t.value = 7);
        assert_eq!(vec.get(id), Some(&Test::new(id, 7)));
    }

    #[test]
    fn overwriting() {
        let mut vec = IndexedVec::new();

        let id = vec.allocate_id(|id| Test::new(id, 5));
        assert_eq!(vec.get(id), Some(&Test::new(id, 5)));

        vec[id].value = 7;
        assert_eq!(vec.get(id), Some(&Test::new(id, 7)));
    }

    #[test]
    fn you_can_have_a_bunch() {
        let mut vec = IndexedVec::new();

        let mut last_id = TestId::new(usize::MAX);
        for i in 0..200 {
            last_id = vec.allocate_id(|id| Test::new(id, i));
        }

        assert_eq!(vec.inner.len(), vec.len());
        assert_eq!(vec.inner.len(), 200);
        assert_eq!(vec.inner.last(), Some(&Test::new(last_id, 199)));
    }

    #[test]
    fn you_can_have_some_things() {
        let mut vec = IndexedVec::new();

        let mut last_id = TestId::new(usize::MAX);
        for i in 0..200 {
            if i % 5 == 0 {
                last_id = vec.allocate_id(|id| Test::new(id, i));
            }
        }

        assert_eq!(vec.inner.len(), vec.len());
        assert_eq!(vec.inner.len(), 40);
        assert_eq!(vec.inner.last(), Some(&Test::new(last_id, 195)));
    }

    #[test]
    fn index() {
        let mut vec = IndexedVec::new();

        let id = vec.allocate_id(|id| Test::new(id, 0));
        assert_eq!(&vec[id], &Test::new(id, 0));
    }

    #[test]
    fn index_mut() {
        let mut vec = IndexedVec::new();

        let id = vec.allocate_id(|id| Test::new(id, 0));
        assert_eq!(&mut vec[id], &mut Test::new(id, 0));
    }
}
