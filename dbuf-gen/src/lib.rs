mod ast;
pub mod codegen;
mod identifiers;
mod object_id;
mod scope;

mod samples;


use pretty::{BoxAllocator, BoxDoc, DocAllocator};

const NEST_UNIT: isize = 4;
