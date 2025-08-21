use std::io;

#[derive(Debug)]
pub enum ConstructorError {
    MismatchedDependencies,
}

#[derive(Debug)]
pub enum DeserializeError {
    IoError(io::Error),
    ConstructorError(ConstructorError),
    UnknownDescriptor,
    DependenciesDescriptorMismatch,
}

pub type Box<T> = std::boxed::Box<T>;
