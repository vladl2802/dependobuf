use super::identifiers::Tag;
use std::{
    collections::{HashMap, hash_map},
    fmt::Debug,
    hash::Hash,
};

/// HashMap wrapper that supports natural for the scopes nesting
#[derive(Debug)]
pub struct Scope<'a, Key, Value>
where
    Key: Hash + Eq + 'a,
    Value: 'a,
{
    parent: Option<&'a Scope<'a, Key, Value>>,
    map: HashMap<Key, Value>,
}

impl<'a, Key, Value> Scope<'a, Key, Value>
where
    Key: Hash + Eq,
{
    pub fn empty() -> Self {
        Scope {
            parent: None,
            map: HashMap::new(),
        }
    }

    pub fn nested_in(parent: &'a Scope<'a, Key, Value>) -> Self {
        Scope {
            parent: Some(parent),
            map: HashMap::new(),
        }
    }

    pub fn contains(&self, key: &Key) -> bool {
        self.get(key).is_some()
    }

    pub fn get(&self, key: &Key) -> Option<&Value> {
        self.map
            .get(key)
            .or(self.parent.and_then(|parent| parent.get(key)))
    }

    pub fn try_insert(&mut self, key: Key, value: Value) -> bool {
        if self.contains(&key) {
            false
        } else {
            self.map.insert(key, value);
            true
        }
    }

    pub fn flat_iter(&'a self) -> FlatIter<'a, Key, Value> {
        FlatIter {
            iter: self.map.iter(),
        }
    }

    pub fn flat_into_iter(self) -> FlatIntoIter<Key, Value> {
        FlatIntoIter {
            iter: self.map.into_iter(),
        }
    }

    pub fn recurse_iter(&'a self) -> RecurseIter<'a, Key, Value> {
        RecurseIter {
            current_scope: self,
            iter: self.map.iter(),
        }
    }
}

pub struct FlatIter<'a, Key, Value>
where
    Key: Hash + Eq,
{
    iter: hash_map::Iter<'a, Key, Value>,
}

impl<'a, Key, Value> Iterator for FlatIter<'a, Key, Value>
where
    Key: Hash + Eq,
{
    type Item = (&'a Key, &'a Value);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
}

pub struct FlatIntoIter<Key, Value>
where
    Key: Hash + Eq,
{
    iter: hash_map::IntoIter<Key, Value>,
}

impl<Key, Value> Iterator for FlatIntoIter<Key, Value>
where
    Key: Hash + Eq,
{
    type Item = (Key, Value);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
}

pub struct RecurseIter<'a, Key, Value>
where
    Key: Hash + Eq,
{
    current_scope: &'a Scope<'a, Key, Value>,
    iter: hash_map::Iter<'a, Key, Value>,
}

impl<'a, Key, Value> Iterator for RecurseIter<'a, Key, Value>
where
    Key: Hash + Eq,
{
    type Item = (&'a Key, &'a Value);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(value) = self.iter.next() {
                return Some(value);
            }
            if let Some(parent) = self.current_scope.parent {
                self.current_scope = parent;
                self.iter = self.current_scope.map.iter();
            } else {
                return None;
            }
        }
    }
}

// TODO: For now TaggerScope stores actual String representation of Value in order to track values with same name
// but that's unreasonable copy, because we actually store Value already. If it's string representation can be cheaply
// computed, then it's unreasonable to store copy.
// Potential solution could be to store some kind of StringRepr<Value> which just stores reference to value and
// can be cheaply check for the equality and provide its hash

/// This scope is used to track variables with the same name and give them additional unique tag
/// which can be used later by formatter in order to generate unique name.
///
/// So, NamingScope responsible for given identifier formatter unique tag.
#[derive(Debug)]
pub struct TaggerScope<'a, Value>
where
    Value: Hash + Eq,
{
    tagger: Scope<'a, Value, Tag>,
}

impl<'a, Value> TaggerScope<'a, Value>
where
    Value: Hash + Eq + Debug,
{
    pub fn empty() -> Self {
        TaggerScope {
            tagger: Scope::empty(),
        }
    }

    pub fn nested_in(parent: &'a TaggerScope<'a, Value>) -> Self {
        TaggerScope {
            tagger: Scope::nested_in(&parent.tagger),
        }
    }

    pub fn contains(&self, value: &Value) -> bool {
        self.tagger.contains(value)
    }

    pub fn insert(&mut self, value: Value) -> Tag {
        match self.tagger.map.entry(value) {
            hash_map::Entry::Occupied(mut occupied) => {
                let tag = occupied.get().inc();
                occupied.insert(tag.clone());
                tag
            }
            hash_map::Entry::Vacant(vacant) => {
                let tag = self
                    .tagger
                    .parent
                    .and_then(|parent| parent.get(vacant.key()))
                    .cloned()
                    .map(|tag| tag.inc())
                    .unwrap_or_default();
                vacant.insert(tag.clone());
                tag
            }
        }
    }
}
