mod ast;
mod codegen;
mod scope;
mod identifiers;

mod sample;

use scope::Scope;
use pretty::{BoxDoc, BoxAllocator, DocAllocator};

const NEST_UNIT: isize = 4;
