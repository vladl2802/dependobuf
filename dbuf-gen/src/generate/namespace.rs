use std::hash::Hash;

use super::{
    lookup::{Accessor, CursorBase, Walker},
    node::Node,
};

#[allow(clippy::struct_field_names, reason = "??? (edge field in Edge struct)")]
#[derive(Debug)]
struct Edge<'parent, Key, Value> {
    associated_with: Key,
    generated: &'parent mut Node<Key, Value>,
    edge: Option<&'parent Edge<'parent, Key, Value>>,
}

#[derive(Debug)]
pub struct NamespaceTree<'parent, Key, Value> {
    generated: Node<Key, Value>,
    edge: Option<Edge<'parent, Key, Value>>,
}

#[derive(Debug, Clone, Copy)]
enum TreeCursorImpl<'me, Key, Value> {
    InTree {
        object: &'me Node<Key, Value>,
        associated_with: &'me Key,
    },
    OnStack {
        object: &'me Node<Key, Value>,
        edge: Option<&'me Edge<'me, Key, Value>>,
    },
}

#[derive(Debug, Clone, Copy)]
pub struct NamespaceCursor<'me, Key, Value>(TreeCursorImpl<'me, Key, Value>);

#[derive(Debug, Clone, Copy)]
pub struct TreeCursorState<'me, Key, Value> {
    pub node: &'me Node<Key, Value>,
    pub key: Option<&'me Key>,
}

#[allow(dead_code, reason = "??? (some methods are never used)")]
impl<Key: Eq + Hash + Clone, Value: Clone> NamespaceTree<'_, Key, Value> {
    pub fn root(detail: Value) -> Self {
        NamespaceTree {
            generated: Node::new(detail),
            edge: None,
        }
    }

    pub fn try_insert_tree(&mut self, key: Key, node: Node<Key, Value>) -> bool {
        self.generated.try_insert(key, node)
    }

    pub fn insert_tree(&mut self, key: Key, node: Node<Key, Value>) {
        self.generated.insert(key, node);
    }

    pub fn remove_tree(&mut self, key: &Key) -> Option<Node<Key, Value>> {
        self.generated.remove(key)
    }

    pub fn try_insert(&mut self, key: Key, detail: Value) -> Option<NamespaceTree<'_, Key, Value>> {
        let parent = &mut self.generated;
        if parent.nested.contains_key(&key) {
            None
        } else {
            Some(NamespaceTree {
                generated: Node::new(detail),
                edge: Some(Edge {
                    associated_with: key,
                    generated: parent,
                    edge: self.edge.as_ref(),
                }),
            })
        }
    }

    pub fn insert(&mut self, key: Key, detail: Value) -> NamespaceTree<'_, Key, Value> {
        self.try_insert(key, detail).expect("could not insert")
    }

    pub fn finish(self) {
        if let Some(edge) = self.edge {
            assert!(
                edge.generated
                    .try_insert(edge.associated_with, self.generated),
                "could not finish"
            );
        }
    }

    pub fn get(&self) -> &Value {
        &self.generated.detail
    }

    pub fn get_mut(&mut self) -> &mut Value {
        &mut self.generated.detail
    }

    pub fn cursor(&self) -> NamespaceCursor<'_, Key, Value> {
        NamespaceCursor(TreeCursorImpl::OnStack {
            object: &self.generated,
            edge: self.edge.as_ref(),
        })
    }
}

impl<'me, Key, Value> TreeCursorImpl<'me, Key, Value> {
    fn node(&self) -> &'me Node<Key, Value> {
        match self {
            TreeCursorImpl::InTree {
                object,
                associated_with: _,
            } => object,
            TreeCursorImpl::OnStack { object, edge: _ } => object,
        }
    }

    fn key(&self) -> Option<&'me Key> {
        match self {
            TreeCursorImpl::InTree {
                object: _,
                associated_with,
            } => Some(associated_with),
            TreeCursorImpl::OnStack { object: _, edge } => edge.map(|edge| &edge.associated_with),
        }
    }

    fn back(&self) -> Option<Self> {
        if let TreeCursorImpl::OnStack { object: _, edge } = self {
            edge.map(
                |Edge {
                     associated_with: _,
                     generated,
                     edge,
                 }| {
                    TreeCursorImpl::OnStack {
                        object: generated,
                        edge: *edge,
                    }
                },
            )
        } else {
            panic!("go_back while not on stack is not supported")
        }
    }
}

impl<'me, Key, Value> Accessor<TreeCursorState<'me, Key, Value>>
    for NamespaceCursor<'me, Key, Value>
{
    fn state(&self) -> TreeCursorState<'me, Key, Value> {
        TreeCursorState {
            node: self.0.node(),
            key: self.0.key(),
        }
    }
}

impl<'me, Key: Eq + Hash, Value> Walker<Key> for NamespaceCursor<'me, Key, Value> {
    fn walk_next(&mut self, key: &Key) -> bool {
        match self.0.node().next_with_key(key) {
            Some((associated_with, node)) => {
                *self = NamespaceCursor(TreeCursorImpl::InTree {
                    object: node,
                    associated_with,
                });
                true
            }
            None => false,
        }
    }

    fn walk_back(&mut self) -> bool {
        match self.0.back() {
            Some(cursor) => {
                *self = NamespaceCursor(cursor);
                true
            }
            None => false,
        }
    }
}

impl<'me, Key: Eq + Hash + Clone, Value: Clone> CursorBase<Key, TreeCursorState<'me, Key, Value>>
    for NamespaceCursor<'me, Key, Value>
{
    fn clone_boxed(&self) -> Box<dyn CursorBase<Key, TreeCursorState<'me, Key, Value>> + '_> {
        Box::new(self.clone())
    }
}
