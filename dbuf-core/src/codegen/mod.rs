mod ast;
mod codegen;
mod identifiers;
mod object_id;
mod scope;

mod sample;

use pretty::{BoxAllocator, BoxDoc, DocAllocator};

const NEST_UNIT: isize = 4;
