#![cfg(test)]

use std::collections::HashMap;

use crate::TrieMap;

#[test]
fn empty_trie() {
    let trie: TrieMap<Vec<String>, String, ()> = TrieMap::new();
    let val = trie.get(&Vec::new());
    assert_eq!(val, None)
}

#[test]
fn empty_insert() {
    let mut trie: TrieMap<Vec<()>, (), i32> = TrieMap::new();
    trie.insert(vec![], 0);

    assert_eq!(trie.map.len(), 0);
    assert_eq!(trie.children.len(), 0);
}

#[test]
fn populate_from_vec() {
    let trie = TrieMap::from_iter(vec![
        (vec!["abc"], 0),
        (vec!["abc", "abc"], 1),
        (vec!["abc", "def"], 2),
        (vec!["def", "ghi"], 3),
    ]);

    assert_eq!(trie.get(&vec!["abc"]), Some(&0));
    assert_eq!(trie.get(&vec!["abc", "abc"]), Some(&1));
    assert_eq!(trie.get(&vec!["abc", "def"]), Some(&2));
    assert_eq!(trie.get(&vec!["def", "ghi"]), Some(&3));
    assert_eq!(trie.get(&vec![]), None);
    assert_eq!(trie.get(&vec!["abc", "abc", "abc"]), None);
}

#[test]
fn data_structure_sanity() {
    let trie = TrieMap::from_iter(vec![
        (vec!["abc"], 0),
        (vec!["abc", "abc"], 1),
        (vec!["abc", "def"], 2),
        (vec!["def", "ghi"], 3),
    ]);

    assert_eq!(trie.map.len(), 1);
    assert_eq!(trie.children.len(), 2);

    let abc_trie = &trie.children[&"abc"];
    assert_eq!(abc_trie.map.len(), 2);
    assert_eq!(abc_trie.children.len(), 0);

    let def_trie = &trie.children[&"def"];
    assert_eq!(def_trie.map.len(), 1);
    assert_eq!(def_trie.children.len(), 0);
}

#[test]
fn more_of_prefix_sharing() {
    let trie = TrieMap::from_iter(vec![
        (vec!["a", "b", "c"], 0),
        (vec!["a", "b", "d"], 1),
        (vec!["a", "x", "y"], 2),
    ]);

    assert_eq!(trie.children.len(), 1);

    let a_trie = &trie.children[&"a"];
    assert_eq!(a_trie.map.len(), 0);
    assert_eq!(a_trie.children.len(), 2);
}

#[test]
fn insertions() {
    let mut trie = TrieMap::new();

    trie.insert(vec!["a", "b"], 0);
    assert_eq!(trie.get(&vec!["a", "b"]), Some(&0));

    trie.insert(vec!["a", "b"], 5);
    assert_eq!(trie.get(&vec!["a", "b"]), Some(&5));
}

#[test]
fn hash_map_to_trie_map() {
    let map: HashMap<Vec<&str>, i32> = HashMap::from_iter(vec![
        (vec!["a", "b", "c"], 0),
        (vec!["a", "b", "d"], 1),
        (vec!["a", "x", "y"], 2),
    ]);

    let trie = TrieMap::from_iter(map);

    assert_eq!(trie.children.len(), 1);

    let a_trie = &trie.children[&"a"];
    assert_eq!(a_trie.map.len(), 0);
    assert_eq!(a_trie.children.len(), 2);
}

#[test]
fn len_of_trie() {
    let map: HashMap<Vec<&str>, i32> = HashMap::from_iter(vec![
        (vec!["a", "b", "c"], 0),
        (vec!["a", "b", "d"], 1),
        (vec!["a", "x", "y"], 2),
    ]);

    let trie = TrieMap::from_iter(map);

    assert_eq!(trie.len(), 3);
}
