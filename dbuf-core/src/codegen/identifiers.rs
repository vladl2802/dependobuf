use super::{ast, codegen::CodegenContext, scope::TaggerScope, BoxDoc, DocAllocator};
use std::{fmt::Display, hash::Hash, rc::Rc};

// TODO: make format apply naming conventions depending on identifier kind

// TODO: This is struct not accurate representation of the name scopes
// for example in following code snippet
// mod b {}
// mod a {
//     mod b {}
// }
// module a::b do not conflict with module b, but current implementation does not aware
// I'm not sure how to implement such behavior, so it's huge TODO

// TODO: for now identifiers here are strongly connected with format in which I generate code
// for example identifier::Type can be constructed from ast::Type or just from name
// But in future I want to also be able to for example generate identifier::Module from just the ast::Type.
// More precisely, I want to have connection between generated identifiers and what I generate. 
// At current stand point I see this connection as HashMap<ObjectId, Box<dyn Identifier>> where ObjectId is broadly 
// speaking unique id that differentiates different ast objects (such as types, constructors, even fields maybe).

pub struct NamingScope<'a> {
    pub variables: TaggerScope<'a, Variable>,
    pub functions: TaggerScope<'a, Function>,
    pub modules: TaggerScope<'a, Module>,
    pub types: TaggerScope<'a, Type>,
}

impl<'a> NamingScope<'a> {
    pub fn nested_in(parent: &'a NamingScope<'a>) -> Self {
        NamingScope {
            variables: TaggerScope::nested_in(&parent.variables),
            functions: TaggerScope::nested_in(&parent.functions),
            modules: TaggerScope::nested_in(&parent.modules),
            types: TaggerScope::nested_in(&parent.types),
        }
    }
}

#[derive(Clone, Copy, Default)]
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

// maybe make result type generic
pub trait Formattable {
    fn format(&self) -> String;
}

pub trait Identifier<'a, 'c>
where
    Self: Formattable + Clone + Hash + PartialEq + Eq + 'c,
{
    fn tag_format(&self, tag: Tag) -> String;

    fn my_scope<'b>(scope: &'b mut NamingScope<'c>) -> &'b mut TaggerScope<'c, Self>;

    /// Tries to generate Identifier without adding tag.
    fn try_generate(
        &self,
        ctx: CodegenContext<'a>,
        scope: &mut NamingScope<'c>,
    ) -> Option<BoxDoc<'a>> {
        if let Some(tag) = Self::my_scope(scope).try_insert(self.clone()) {
            Some(ctx.alloc.text(self.tag_format(tag)).into_doc())
        } else {
            None
        }
    }

    fn generate(&self, ctx: CodegenContext<'a>, scope: &mut NamingScope<'c>) -> BoxDoc<'a> {
        let tag = Self::my_scope(scope).insert(self.clone());
        ctx.alloc.text(self.tag_format(tag)).into_doc()
    }

    fn try_generate_substituted(&self, ctx: CodegenContext<'a>, scope: &mut NamingScope<'c>, with: BoxDoc<'a>) -> BoxDoc<'a> {

    }

    fn get_generated(
        &self,
        ctx: CodegenContext<'a>,
        scope: &mut NamingScope<'c>,
    ) -> Option<BoxDoc<'a>> {
        Self::my_scope(scope)
            .get(&self)
            .map(|tag| ctx.alloc.text(self.tag_format(tag)).into_doc())
    }
}

#[derive(Clone)]
enum VariableBase {
    Symbol(Rc<ast::Symbol>),
    String(String),
}

impl Hash for VariableBase {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            VariableBase::Symbol(symbol) => Rc::as_ptr(symbol).hash(state),
            VariableBase::String(string) => string.hash(state),
        }
    }
}

impl PartialEq for VariableBase {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Symbol(lhs), Self::Symbol(rhs)) => Rc::ptr_eq(lhs, rhs),
            (Self::String(lhs), Self::String(rhs)) => lhs == rhs,
            _ => false,
        }
    }
}

impl Eq for VariableBase {}

// needed because enum variants are public to the user
#[derive(Clone, Hash, PartialEq, Eq)]
pub struct Variable(VariableBase);

impl Variable {
    pub fn from_symbol(symbol: Rc<ast::Symbol>) -> Self {
        Variable(VariableBase::Symbol(symbol))
    }

    pub fn from_name(name: String) -> Self {
        Variable(VariableBase::String(name))
    }
}

impl Formattable for Variable {
    fn format(&self) -> String {
        match &self.0 {
            VariableBase::Symbol(symbol) => symbol.name.clone(),
            VariableBase::String(string) => string.clone(),
        }
    }
}

impl<'a, 'c> Identifier<'a, 'c> for Variable {
    fn tag_format(&self, tag: Tag) -> String {
        if tag.0 > 0 {
            format!("{}_{}", self.format(), tag)
        } else {
            self.format()
        }
    }

    fn my_scope<'b>(scope: &'b mut NamingScope<'c>) -> &'b mut TaggerScope<'c, Self> {
        &mut scope.variables
    }
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct Function {
    name: String,
}

impl Function {
    pub fn from_name(name: String) -> Self {
        Function { name }
    }
}

impl Formattable for Function {
    fn format(&self) -> String {
        self.name.clone()
    }
}

impl<'a, 'c> Identifier<'a, 'c> for Function {
    fn tag_format(&self, tag: Tag) -> String {
        if tag.0 > 0 {
            format!("{}_{}", self.format(), tag)
        } else {
            self.format()
        }
    }

    fn my_scope<'b>(scope: &'b mut NamingScope<'c>) -> &'b mut TaggerScope<'c, Self> {
        &mut scope.functions
    }
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct Module {
    name: String,
}

impl Module {
    pub fn from_name(name: String) -> Self {
        Module { name }
    }
}

impl Formattable for Module {
    fn format(&self) -> String {
        self.name.clone()
    }
}

impl<'a, 'c> Identifier<'a, 'c> for Module {
    fn tag_format(&self, tag: Tag) -> String {
        if tag.0 > 0 {
            format!("{}_{}", self.format(), tag)
        } else {
            self.format()
        }
    }

    fn my_scope<'b>(scope: &'b mut NamingScope<'c>) -> &'b mut TaggerScope<'c, Self> {
        &mut scope.modules
    }
}

#[derive(Clone)]
enum TypeBase {
    Type(Rc<ast::Type>),
    String(String),
}

impl Hash for TypeBase {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            TypeBase::Type(ty) => Rc::as_ptr(ty).hash(state),
            TypeBase::String(string) => string.hash(state),
        }
    }
}

impl PartialEq for TypeBase {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Type(lhs), Self::Type(rhs)) => Rc::ptr_eq(lhs, rhs),
            (Self::String(lhs), Self::String(rhs)) => lhs == rhs,
            _ => false,
        }
    }
}

impl Eq for TypeBase {}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct Type(TypeBase);

impl Type {
    pub fn from_type(ty: Rc<ast::Type>) -> Self {
        Type(TypeBase::Type(ty))
    }

    pub fn from_name(name: String) -> Self {
        Type(TypeBase::String(name))
    }
}

impl Formattable for Type {
    fn format(&self) -> String {
        match &self.0 {
            TypeBase::Type(ty) => ty.name.clone(),
            TypeBase::String(string) => string.clone(),
        }
    }
}

impl<'a, 'c> Identifier<'a, 'c> for Type {
    fn tag_format(&self, tag: Tag) -> String {
        if tag.0 > 0 {
            format!("{}{}", self.format(), tag)
        } else {
            self.format()
        }
    }

    fn my_scope<'b>(scope: &'b mut NamingScope<'c>) -> &'b mut TaggerScope<'c, Self> {
        &mut scope.types
    }
}
