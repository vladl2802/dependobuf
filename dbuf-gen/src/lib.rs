mod ast;
pub mod codegen;
mod identifiers;
mod object_id;
mod scope;

use pretty::{BoxAllocator, BoxDoc, DocAllocator};

const NEST_UNIT: isize = 4;
