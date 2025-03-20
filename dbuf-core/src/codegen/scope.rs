use super::identifiers::{Formattable, Tag};
use std::{
    collections::{
        hash_map::{self, Entry},
        HashMap,
    },
    hash::Hash,
};

/// HashMap wrapper that supports natural for the scopes nesting
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

struct FlatIter<'a, Key, Value>
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

struct FlatIntoIter<Key, Value>
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

struct RecurseIter<'a, Key, Value>
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
pub struct TaggerScope<'a, Value>
where
    Value: Formattable + Hash + Eq + 'a,
{
    tagger: Scope<'a, String, Tag>,
    scope: Scope<'a, Value, Tag>,
}

impl<'a, Value> TaggerScope<'a, Value>
where
    Value: Formattable + Hash + Eq,
{
    pub fn empty() -> Self {
        TaggerScope {
            tagger: Scope::empty(),
            scope: Scope::empty(),
        }
    }

    pub fn nested_in(parent: &'a TaggerScope<'a, Value>) -> Self {
        TaggerScope {
            tagger: Scope::nested_in(&parent.tagger),
            scope: Scope::nested_in(&parent.scope),
        }
    }

    pub fn contains(&self, value: &Value) -> bool {
        self.scope.contains(value)
    }

    pub fn get(&self, value: &Value) -> Option<Tag> {
        self.scope.get(value).copied()
    }

    pub fn try_insert(&mut self, value: Value) -> Option<Tag> {
        let string = value.format();
        if self.tagger.contains(&string) {
            None
        } else {
            let tag = Tag::default();
            assert!(
                self.tagger.try_insert(string, tag),
                "tagger contain and try_insert contradict"
            );
            assert!(
                self.scope.try_insert(value, tag),
                "tagger try_insert succeed but scope try_insert failed"
            );
            Some(tag)
        }
    }

    /// If value was already inserted does nothing and returns stored for it tag
    pub fn insert(&mut self, value: Value) -> Tag {
        let string = value.format();
        let vacant = match self.scope.map.entry(value) {
            Entry::Occupied(occupied) => return occupied.get().clone(),
            Entry::Vacant(vacant) => vacant,
        };
        let tag = match self.tagger.map.entry(string) {
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
        };
        vacant.insert(tag);
        tag
    }
}
