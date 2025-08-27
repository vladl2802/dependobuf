use std::{collections::HashMap, mem::ManuallyDrop};

use crate::{
    ast::NodeId,
    generate::{
        GlobalContext,
        lookup::{Cursor, NavigateResult, Navigator},
        namespace::{self, NamespaceTree},
        node,
    },
};

use super::objects::{self, GeneratedRustObject, Kind, Object, ObjectId, RustObject};

// Lifetimes naming:
// 'a - global lifetime for everything that has lifetime spanning whole codegen step.
// Currently there is only BoxAllocator that uses this lifetime.
// 'id - global lifetime for everything related to ast::NodeId and therefore to ObjectId. It is ast lifetime.
// Which from the view of codegen must be the same as 'a.
// 'me - often used in the context of Cursors with the idea, that Cursor borrows Tree for some lifetime
// and therefore anything, that is borrowed during this, has this lifetime. Maybe should be renamed to 'c (short for 'cursor)

// This file currenly has an awful structure. It defines many aliases for the commonly used types and traits and also defines NamingContext.
// So it should be split into two parts.

pub type MutContext<'a, 'parent, 'me> = (GlobalContext<'a>, &'me mut NamingContext<'a, 'parent>);

pub type Context<'a, 'me, C: GeneratedCursor<'a, 'me>> = (GlobalContext<'a>, C);

pub type Node<'id> = node::Node<ObjectId<'id>, NodeInfo>;

pub trait GeneratedCursor<'me, 'id: 'me>: Cursor<ObjectId<'id>, CursorState<'me, 'id>> {}

impl<'me, 'id: 'me, C> GeneratedCursor<'me, 'id> for C where
    C: Cursor<ObjectId<'id>, CursorState<'me, 'id>>
{
}

pub trait GeneratedNavigator<'me, 'id: 'me, C: GeneratedCursor<'me, 'id>, Result>:
    Navigator<ObjectId<'id>, C, Result>
{
}

impl<'me, 'id: 'me, C, Result, N> GeneratedNavigator<'me, 'id, C, Result> for N
where
    C: GeneratedCursor<'me, 'id>,
    N: Navigator<ObjectId<'id>, C, Result>,
{
}

#[derive(Debug, Clone)]
pub struct CursorState<'me, 'id>(namespace::TreeCursorState<'me, ObjectId<'id>, NodeInfo>);

#[derive(Debug, Clone)]
pub struct NodeInfo {
    object: GeneratedRustObject,
    tags: HashMap<RustObject, u64>,
}

#[derive(Debug)]
pub struct NamingContext<'id, 'parent>(
    ManuallyDrop<NamespaceTree<'parent, ObjectId<'id>, NodeInfo>>,
);

#[derive(Clone)]
pub struct Name {
    object: RustObject,
    tag: u64,
}

impl<'id> NamingContext<'id, '_> {
    fn wrap_namespace_tree<'a>(
        namespace_tree: NamespaceTree<'a, ObjectId<'id>, NodeInfo>,
    ) -> NamingContext<'id, 'a> {
        NamingContext(ManuallyDrop::new(namespace_tree))
    }

    fn insert_object_with_tag<'child, O: Object<'id>>(
        &'child mut self,
        object: O,
        tag: u64,
    ) -> (O::Generated, NamingContext<'id, 'child>) {
        self.0.get_mut().tags.insert(object.rust_object(), tag);

        let id = object.object_id();
        let generated = object.generate_tagged(tag);
        (
            generated.clone(),
            Self::wrap_namespace_tree(self.0.insert(
                id,
                NodeInfo {
                    object: generated.into(),
                    tags: HashMap::new(),
                },
            )),
        )
    }

    pub fn root() -> Self {
        Self::wrap_namespace_tree(NamespaceTree::root(NodeInfo {
            object: objects::Scope::new(ObjectId(
                NodeId::owned("root".to_owned()),
                objects::Tag::None,
            ))
            .generate_tagged(0)
            .into(),
            tags: HashMap::new(),
        }))
    }

    pub fn insert_tree<'cursor>(
        &'cursor mut self,
        id: &ObjectId<'id>,
        tree: Node<'id>,
    ) -> impl GeneratedCursor<'cursor, 'id>
    where
        'id: 'cursor,
    {
        self.0.insert_tree(id.clone(), tree);

        self.cursor()
            .next(id)
            .expect("newly inserted tree couldn't be accessed")
    }

    pub fn remove_tree(&mut self, id: &ObjectId<'id>) -> Option<Node<'id>> {
        self.0.remove_tree(id)
    }

    pub fn name_object<O: Object<'id>>(&mut self, object: &O) -> Name {
        let tag = match O::lookup_tag(self.cursor(), &object.rust_object()) {
            Some(tag) => tag + 1,
            None => 0,
        };
        self.0.get_mut().tags.insert(object.rust_object(), tag);
        Name {
            object: object.rust_object(),
            tag,
        }
    }

    pub fn insert_object<'child, O: Object<'id>>(
        &'child mut self,
        object: O,
        name: &Name,
    ) -> (O::Generated, NamingContext<'id, 'child>) {
        let id = object.object_id();
        let generated = object.generate_tagged(name.tag);
        (
            generated.clone(),
            Self::wrap_namespace_tree(self.0.insert(
                id,
                NodeInfo {
                    object: generated.into(),
                    tags: HashMap::new(),
                },
            )),
        )
    }

    pub fn insert_object_preserve_name<'child, O: Object<'id>>(
        &'child mut self,
        object: O,
    ) -> Option<(O::Generated, NamingContext<'id, 'child>)> {
        let rust_object = object.rust_object();
        match O::lookup_tag(self.cursor(), &rust_object) {
            Some(_) => None,
            None => Some(self.insert_object(
                object,
                &Name {
                    object: rust_object,
                    tag: 0,
                },
            )),
        }
    }

    pub fn insert_object_auto_name<'child, O: Object<'id>>(
        &'child mut self,
        object: O,
    ) -> (O::Generated, NamingContext<'id, 'child>) {
        let name = self.name_object(&object);
        self.insert_object(object, &name)
    }

    pub fn cursor<'cursor>(&'cursor self) -> impl GeneratedCursor<'cursor, 'id> + Clone {
        self.0.cursor().map_state(|state| CursorState(state))
    }
}

impl<'me, 'id> CursorState<'me, 'id> {
    pub fn object(&self) -> &GeneratedRustObject {
        &self.0.node.detail().object
    }

    pub fn associated_with(&self) -> &Option<&ObjectId<'id>> {
        &self.0.key
    }

    pub fn tag_for(&self, object: &RustObject) -> Option<u64> {
        self.0.node.detail().tags.get(object).copied()
    }
}

impl Drop for NamingContext<'_, '_> {
    fn drop(&mut self) {
        // SAFETY: we are in drop so there will be no usage after
        unsafe { ManuallyDrop::take(&mut self.0) }.finish();
    }
}

struct ModuleRootNavigator {}

impl<'me, 'id: 'me, C: GeneratedCursor<'me, 'id> + Clone> Navigator<ObjectId<'id>, C, ()>
    for ModuleRootNavigator
{
    fn decide(&mut self, cursor: &C) -> NavigateResult<ObjectId<'id>, ()> {
        if cursor.state().object().kind() == Kind::Module || cursor.clone().back().is_none() {
            NavigateResult::Stop(())
        } else {
            NavigateResult::GoBack
        }
    }
}
