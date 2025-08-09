use dbuf_rust_runtime::{Box, ConstructorError, DeserializeError};
use std::io::{Write, Read, Error};
use std::slice;
pub mod nat {
    use super::serde::{self, Serialize, Deserialize};
    
    mod deps {
        // pub(super) use super::super::{};
    }
    
    mod descriptor {
        pub(super) const Suc: u8 = 0;
        pub(super) const Zero: u8 = 1;
    }
    #[derive(Clone, Debug, PartialEq, Eq)]
    pub enum Body {
        Suc {
            pred: super::Box<Nat>
        },
        Zero {
        
        }
    }
    
    #[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
    #[serde(crate = "self::serde")]
    pub struct Dependencies {
    
    }
    
    #[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
    #[serde(crate = "self::serde")]
    pub struct Nat {
        pub body: Body,
        pub dependencies: Dependencies
    }
    
    impl Nat {
        pub fn Suc(pred: super::Box<Nat>) -> Result<Self, super::ConstructorError> {
            let body = if (()) == (()) {
                Ok(Body::Suc {
                    pred: pred
                })
            } else {
                Err(super::ConstructorError::MismatchedDependencies)
            }?;
            let dependencies = Dependencies {
            
            };
            Ok(Self { body: body, dependencies: dependencies })
        }
        pub fn Zero() -> Result<Self, super::ConstructorError> {
            let body = if () == () {
                Ok(Body::Zero {
                
                })
            } else {
                Err(super::ConstructorError::MismatchedDependencies)
            }?;
            let dependencies = Dependencies {
            
            };
            Ok(Self { body: body, dependencies: dependencies })
        }
        pub fn serialize<W: super::Write>(self, writer: &mut W) -> Result<(), super::Error> {
            match self.body {
                Body::Suc { pred } => {
                    writer.write_all(&[descriptor::Suc])?;
                    pred.serialize(writer)?;
                },
                Body::Zero {  } => {
                    writer.write_all(&[descriptor::Zero])?;
                },
            }
            Ok(())
        }
        pub fn deserialize<R: super::Read>(reader: &mut R) -> Result<Self, super::DeserializeError> {
            let mut descriptor = 0;
            reader.read(super::slice::from_mut(&mut descriptor)).map_err(|e| super::DeserializeError::IoError(e))?;
            match descriptor {
                descriptor::Suc => {
                    let pred = Self::deserialize(reader)?;
                    Self::Suc(super::Box::new(pred)).map_err(|e| super::DeserializeError::ConstructorError(e))
                },
                descriptor::Zero => {
                    Self::Zero().map_err(|e| super::DeserializeError::ConstructorError(e))
                },
                _ => Err(super::DeserializeError::UnknownDescriptor),
            }
        }
    }
}

pub use nat::Nat as Nat;

pub mod vec {
    use super::serde::{self, Serialize, Deserialize};
    
    mod deps {
        pub(super) use super::super::{{nat, Nat}};
    }
    
    mod descriptor {
        pub(super) const Cons: u8 = 0;
        pub(super) const Nil: u8 = 1;
    }
    #[derive(Clone, Debug, PartialEq, Eq)]
    pub enum Body {
        Cons {
            val: super::Box<deps::nat::Nat>,
            tail: super::Box<Vec>
        },
        Nil {
        
        }
    }
    
    #[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
    #[serde(crate = "self::serde")]
    pub struct Dependencies {
        pub n: super::Box<deps::nat::Nat>
    }
    
    #[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
    #[serde(crate = "self::serde")]
    pub struct Vec {
        pub body: Body,
        pub dependencies: Dependencies
    }
    
    impl Vec {
        pub fn Cons(p: super::Box<deps::nat::Nat>, val: super::Box<deps::nat::Nat>, tail: super::Box<Vec>) -> Result<Self, super::ConstructorError> {
            let body = if ((),
            (&p)) == ((),
            (&tail.dependencies.n)) {
                Ok(Body::Cons {
                    value: value,
                    tail: tail
                })
            } else {
                Err(super::ConstructorError::MismatchedDependencies)
            }?;
            let dependencies = Dependencies {
                n: Box::new(deps::nat::Nat::suc(p).expect("constructor 'Nat::Suc' failed"))
            };
            Ok(Self { body: body, dependencies: dependencies })
        }
        pub fn Nil() -> Result<Self, super::ConstructorError> {
            let body = if () == () {
                Ok(Body::Nil {
                
                })
            } else {
                Err(super::ConstructorError::MismatchedDependencies)
            }?;
            let dependencies = Dependencies {
                n: Box::new(deps::nat::Nat::zero().expect("constructor 'Nat::Zero' failed"))
            };
            Ok(Self { body: body, dependencies: dependencies })
        }
        pub fn serialize<W: super::Write>(self, writer: &mut W) -> Result<(), super::Error> {
            match self.body {
                Body::Cons { val, tail } => {
                    writer.write_all(&[descriptor::Cons])?;
                    val.serialize(writer)?;
                    tail.serialize(writer)?;
                },
                Body::Nil {  } => {
                    writer.write_all(&[descriptor::Nil])?;
                },
            }
            Ok(())
        }
        pub fn deserialize<R: super::Read>(dependencies: Dependencies, reader: &mut R) -> Result<Self, super::DeserializeError> {
            let mut descriptor = 0;
            reader.read(super::slice::from_mut(&mut descriptor)).map_err(|e| super::DeserializeError::IoError(e))?;
            match descriptor {
                descriptor::Cons => {
                    if let ( deps::nat::Body::Suc { pred } ) = ( dependencies.n.body ) {
                        let val = deps::Nat::deserialize(reader)?;
                        let tail = Self::deserialize(Dependencies { n: pred.clone() }, reader)?;
                        Self::Cons(pred, super::Box::new(val), super::Box::new(tail)).map_err(|e| super::DeserializeError::ConstructorError(e))
                    } else {
                        Err(super::DeserializeError::DependenciesDescriptorMismatch)
                    }
                },
                descriptor::Nil => {
                    if let deps::nat::Body::Zero {} = dependencies.n.body {
                        Self::Nil().map_err(|e| super::DeserializeError::ConstructorError(e))
                    } else {
                        Err(super::DeserializeError::DependenciesDescriptorMismatch)
                    }
                },
                _ => Err(super::DeserializeError::UnknownDescriptor),
            }
        }
    }
}

pub use vec::Vec as Vec;