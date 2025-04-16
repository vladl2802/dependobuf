use super::{
    BoxDoc, DocAllocator,
    codegen::CodegenContext,
    object_id::ObjectId,
    scope::{Scope, TaggerScope},
};

use std::fmt::Display;

// TODO: This is struct not accurate representation of the name scopes
// for example in following code snippet
// mod b {}
// mod a {
//     mod b {}
// }
// module a::b do not conflict with module b, but current implementation does not aware
// I'm not sure how to implement such behavior, so it's huge TODO

#[derive(Debug)]
pub struct NamingScope<'a, 'b> {
    pub generated: Scope<'a, ObjectId<'a>, BoxDoc<'b>>,
    pub tagger: TaggerScope<'a, String>,
}

impl<'a, 'b> NamingScope<'a, 'b> {
    pub fn empty() -> Self {
        NamingScope {
            generated: Scope::empty(),
            tagger: TaggerScope::empty(),
        }
    }

    pub fn nested_in(parent: &'a NamingScope<'a, 'b>) -> Self {
        NamingScope {
            generated: Scope::nested_in(&parent.generated),
            tagger: TaggerScope::nested_in(&parent.tagger),
        }
    }
}

pub fn get_generated<'a, 'b>(
    _: CodegenContext<'a>,
    scope: &NamingScope<'b, 'a>,
    object_id: ObjectId<'b>,
) -> Option<BoxDoc<'a>> {
    scope.generated.get(&object_id).cloned()
}

pub fn try_generate_bind<'a, 'b>(
    _: CodegenContext<'a>,
    scope: &mut NamingScope<'b, 'a>,
    object_id: ObjectId<'b>,
    doc: BoxDoc<'a>,
) -> Option<BoxDoc<'a>> {
    if scope.generated.try_insert(object_id, doc.clone()) {
        Some(doc)
    } else {
        None
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Tag(u32);

impl Tag {
    pub fn inc(&self) -> Self {
        Tag(self.0 + 1)
    }
}

impl Display for Tag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

pub trait Identifier<'a, 'b> {
    // TODO: replace string to more appropriate type that is close to pretty::Doc and supports Eq and Hash
    fn format(&self) -> String;

    fn tag_format(&self, tag: Tag) -> String;

    fn object_id(&self) -> ObjectId<'b>;

    /// Tries to generate Identifier without adding tag.
    fn try_generate(
        &self,
        ctx: CodegenContext<'a>,
        scope: &mut NamingScope<'b, 'a>,
    ) -> Option<BoxDoc<'a>> {
        let formatted = self.format();
        if scope.tagger.contains(&formatted) {
            None
        } else {
            let tag = scope.tagger.insert(formatted.clone());
            assert!(tag == Tag::default(), "expect tag zero value");
            let generated = ctx.alloc.text(formatted).into_doc();
            assert!(
                scope
                    .generated
                    .try_insert(self.object_id(), generated.clone()),
                "identifier with provided ObjectId already exists even so it was not tagged before"
            );
            Some(generated)
        }
    }

    fn generate(&self, ctx: CodegenContext<'a>, scope: &mut NamingScope<'b, 'a>) -> BoxDoc<'a> {
        let tag = scope.tagger.insert(self.format());
        let generated = ctx.alloc.text(self.tag_format(tag)).into_doc();
        assert!(
            scope
                .generated
                .try_insert(self.object_id(), generated.clone()),
            "object with provided ObjectId was already generated"
        );
        generated
    }
}

// TODO: make format apply naming conventions depending on identifier kind

macro_rules! declare_identifier {
    ($name:ident) => {
        pub struct $name<'a> {
            object_id: ObjectId<'a>,
            name: String,
        }

        impl<'a> $name<'a> {
            pub fn from_object(object_id: ObjectId<'a>, name: String) -> Self {
                Self { object_id, name }
            }

            pub fn from_name(name: String) -> Self {
                Self {
                    object_id: ObjectId::Owned { name: name.clone() },
                    name,
                }
            }
        }

        impl<'a, 'b> Identifier<'a, 'b> for $name<'b> {
            fn format(&self) -> String {
                // TODO: add r# or mandatory tag if ident is keyword
                self.name.clone()
            }

            fn tag_format(&self, tag: Tag) -> String {
                if tag.0 > 0 {
                    // here naming conventions must be applied
                    format!("{}_{}", self.format(), tag)
                } else {
                    self.format()
                }
            }

            fn object_id(&self) -> ObjectId<'b> {
                self.object_id.clone()
            }
        }
    };
}

declare_identifier! {Variable}
declare_identifier! {Function}
declare_identifier! {Module}
declare_identifier! {Type}
