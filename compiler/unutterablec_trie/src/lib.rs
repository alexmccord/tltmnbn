use std::borrow::Borrow;
use std::collections::{HashMap, VecDeque};
use std::hash::Hash;
use std::marker::PhantomData;

mod tests;

#[derive(Debug, Clone)]
pub struct TrieMap<K, A, V> {
    map: HashMap<A, V>,
    children: HashMap<A, TrieMap<K, A, V>>,
    marker: PhantomData<K>,
}

impl<K, A, V> TrieMap<K, A, V> {
    pub fn new() -> TrieMap<K, A, V> {
        TrieMap::default()
    }

    /// Counts how many keys are bound to a value.
    pub fn len(&self) -> usize {
        // Extremely inefficient since we don't memoize. /shrug. Computers are fast enough.
        let mut sum = 0;

        let mut queue = VecDeque::new();
        queue.push_front(self);

        while let Some(node) = queue.pop_back() {
            sum += node.map.len();

            for (_, descendant) in &node.children {
                queue.push_front(&descendant);
            }
        }

        sum
    }
}

impl<K, A, V> TrieMap<K, A, V>
where
    A: Eq + Hash,
{
    fn traverse<'a, Q: ?Sized, B: ?Sized>(&self, keys: &'a Q) -> Option<(&Self, &'a B)>
    where
        A: Borrow<B>,
        B: Eq + Hash,
        &'a Q: IntoIterator<Item = &'a B>,
    {
        let mut current = self;
        let mut last_k = None;

        for k in keys {
            if let Some(k) = last_k.take() {
                current = current.children.get(k)?;
            }

            last_k = Some(k);
        }

        Some((current, last_k?))
    }

    fn traverse_mut<'a, Q: ?Sized, B: ?Sized>(&mut self, keys: &'a Q) -> Option<(&mut Self, &'a B)>
    where
        A: Borrow<B>,
        B: Eq + Hash,
        &'a Q: IntoIterator<Item = &'a B>,
    {
        let mut current = self;
        let mut last_k = None;

        for k in keys {
            if let Some(k) = last_k.take() {
                current = current.children.get_mut(k)?;
            }

            last_k = Some(k);
        }

        Some((current, last_k?))
    }

    pub fn get<Q: ?Sized, B: ?Sized>(&self, keys: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        A: Borrow<B>,
        B: Eq + Hash,
        for<'a> &'a Q: IntoIterator<Item = &'a B>,
    {
        let (trie, k) = self.traverse(keys)?;
        trie.map.get(&k)
    }

    pub fn get_mut<Q: ?Sized, B: ?Sized>(&mut self, keys: &Q) -> Option<&mut V>
    where
        K: Borrow<Q>,
        A: Borrow<B>,
        B: Eq + Hash,
        for<'a> &'a Q: IntoIterator<Item = &'a B>,
    {
        let (trie, k) = self.traverse_mut(keys)?;
        trie.map.get_mut(&k)
    }

    pub fn contains_key<Q: ?Sized, B: ?Sized>(&self, keys: &Q) -> bool
    where
        A: Borrow<B>,
        B: Eq + Hash,
        for<'a> &'a Q: IntoIterator<Item = &'a B>,
    {
        self.traverse(keys).is_some()
    }

    pub fn contains_prefix_key<Q: ?Sized, B: ?Sized>(&self, keys: &Q) -> bool
    where
        A: Borrow<B>,
        B: Eq + Hash,
        for<'a> &'a Q: IntoIterator<Item = &'a B>,
    {
        self.traverse(keys)
            .and_then(|(trie, k)| trie.children.get(&k))
            .is_some()
    }
}

impl<K, A: ?Sized, V> TrieMap<K, A::Owned, V>
where
    for<'a> &'a K: IntoIterator<Item = &'a A>,
    A: ToOwned<Owned: Eq + Hash>,
{
    fn traverse_and_insert_mut(&mut self, keys: K) -> Option<(&mut Self, A::Owned)> {
        let mut current = self;
        let mut last_k = None;

        for k in &keys {
            if let Some(k) = last_k.take() {
                current = current.children.entry(k).or_default();
            }

            last_k = Some(k.to_owned());
        }

        Some((current, last_k?))
    }

    pub fn insert(&mut self, keys: K, v: V) -> Option<V> {
        let (trie, k) = self.traverse_and_insert_mut(keys)?;
        trie.map.insert(k, v)
    }
}

impl<K, A, V> Default for TrieMap<K, A, V> {
    fn default() -> Self {
        Self {
            map: Default::default(),
            children: Default::default(),
            marker: Default::default(),
        }
    }
}

impl<K, A, V> FromIterator<(K, V)> for TrieMap<K, A::Owned, V>
where
    for<'a> &'a K: IntoIterator<Item = &'a A>,
    A: ToOwned<Owned: Eq + Hash>,
{
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        let mut trie = TrieMap::new();

        for (keys, v) in iter {
            trie.insert(keys, v);
        }

        trie
    }
}
