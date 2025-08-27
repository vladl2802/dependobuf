use crate::{
    format::{BoxDoc, DocAllocator},
    generate::{
        GlobalContext,
        lookup::{Accessor, NavigateResult},
    },
};

use super::{
    GeneratedCursor, GeneratedObject, GeneratedRustObject, Kind, Object, ObjectId, RustObject,
};

#[derive(Clone)]
pub struct Variable<'id> {
    id: ObjectId<'id>,
    name: String,
}

#[derive(Debug, Clone)]
pub struct GeneratedVariable {
    name: String,
    tag: u64,
}

fn format(name: String) -> String {
    // TODO: add r# or mandatory tag if ident is keyword
    name
}

fn tag_format(name: String, tag: u64) -> String {
    if tag > 0 {
        // here naming conventions must be applied
        format!("{}_{}", format(name), tag)
    } else {
        format(name)
    }
}

impl<'id> Variable<'id> {
    pub fn from_object(id: ObjectId<'id>, name: String) -> Self {
        Self { id, name }
    }

    pub fn from_name(name: String) -> Self {
        Self {
            id: ObjectId::from_name(name.clone()),
            name,
        }
    }
}

impl<'id> Object<'id> for Variable<'id> {
    type Generated = GeneratedVariable;

    fn object_id(&self) -> ObjectId<'id> {
        self.id.clone()
    }

    fn kind() -> Kind {
        Kind::Variable
    }

    fn backwards_lookup_limit(generated: &GeneratedRustObject) -> bool {
        match generated.kind() {
            Kind::Scope => false,
            Kind::Module => true,
            Kind::Type => true,
            Kind::Function => false,
            Kind::Variable => false,
        }
    }

    fn lookup_tag<'cursor, C>(cursor: C, object: &RustObject) -> Option<u64>
    where
        'id: 'cursor,
        C: GeneratedCursor<'cursor, 'id> + Clone,
    {
        cursor
            .navigate(|cursor: &C| match cursor.state().tag_for(object) {
                Some(tag) => NavigateResult::Stop(Some(tag)),
                None => {
                    if Self::backwards_lookup_limit(cursor.state().object()) {
                        NavigateResult::Stop(None)
                    } else {
                        NavigateResult::GoBack
                    }
                }
            })
            .map(|cursor| cursor.state())
            .flatten()
    }

    fn navigate_visible<'cursor, C>(
        cursor: C,
        id: ObjectId<'id>,
    ) -> Option<impl GeneratedCursor<'cursor, 'id>>
    where
        'id: 'cursor,
        C: GeneratedCursor<'cursor, 'id> + Clone,
    {
        cursor
            .navigate(|cursor: &C| {
                if *cursor.state().associated_with() == Some(&id) {
                    NavigateResult::Stop(Some(cursor.clone()))
                } else {
                    match cursor.clone().next(&id) {
                        Some(cursor) => NavigateResult::Stop(Some(cursor)),
                        None => {
                            if Self::backwards_lookup_limit(cursor.state().object()) {
                                NavigateResult::Stop(None)
                            } else {
                                NavigateResult::GoBack
                            }
                        }
                    }
                }
            })
            .map(|cursor| cursor.state())
            .flatten()
    }

    fn rust_object(&self) -> RustObject {
        RustObject::Variable {
            name: format(self.name.clone()),
        }
    }

    fn generate_tagged(self, tag: u64) -> Self::Generated {
        GeneratedVariable {
            name: self.name,
            tag,
        }
    }
}

impl GeneratedVariable {
    pub fn to_doc<'a>(&self, ctx: GlobalContext<'a>) -> BoxDoc<'a> {
        ctx.alloc
            .text(tag_format(self.name.clone(), self.tag))
            .into_doc()
    }
}

impl From<GeneratedVariable> for GeneratedRustObject {
    fn from(generated: GeneratedVariable) -> Self {
        GeneratedRustObject {
            object: RustObject::Variable {
                name: generated.name,
            },
            tag: generated.tag,
        }
    }
}

impl TryFrom<GeneratedRustObject> for GeneratedVariable {
    type Error = ();

    fn try_from(generated: GeneratedRustObject) -> Result<Self, Self::Error> {
        if let RustObject::Variable { name } = generated.object {
            Ok(GeneratedVariable {
                name,
                tag: generated.tag,
            })
        } else {
            Err(())
        }
    }
}

impl TryFrom<&GeneratedRustObject> for GeneratedVariable {
    type Error = ();

    fn try_from(value: &GeneratedRustObject) -> Result<Self, Self::Error> {
        Self::try_from(value.clone())
    }
}

impl GeneratedObject<'_> for GeneratedVariable {}
