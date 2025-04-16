use std::{
    any::Any,
    marker::PhantomData,
    ptr,
    rc::{Rc, Weak},
};

/// This enum is expected to used in order to find semantically equal object comparing its ObjectIds.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum ObjectId<'a> {
    Pointer {
        pointer: usize,
        _m: PhantomData<&'a dyn Any>,
    },
    // TODO: because of that ObjectId could not be cheaply copied, maybe change this to Cow
    Owned {
        name: String,
    },
}

impl<'a> ObjectId<'a> {
    pub fn owned(name: String) -> ObjectId<'static> {
        ObjectId::Owned { name }
    }

    pub fn id<T>(this: &'a T) -> ObjectId<'a> {
        ObjectId::Pointer {
            pointer: ptr::from_ref(this) as usize,
            _m: PhantomData,
        }
    }

    pub fn id_rc<T: 'static>(this: &Rc<T>) -> ObjectId<'static> {
        ObjectId::Pointer {
            pointer: Rc::as_ptr(this) as usize,
            _m: PhantomData,
        }
    }

    pub fn id_weak<T: 'static>(this: &Weak<T>) -> ObjectId<'static> {
        let _ = this.upgrade().expect("dandling weak does not have id");
        ObjectId::Pointer {
            pointer: this.as_ptr() as usize,
            _m: PhantomData,
        }
    }
}
