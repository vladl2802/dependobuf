use std::collections::{BTreeMap, BTreeSet};

use super::operators::OpCall;

/// An elaborated DependoBuf module.
#[derive(Debug)]
pub struct Module<Str> {
    /// List of elaborated types in topologically sorted order.
    pub types: Vec<(Str, Type<Str>)>,
    /// Collection of elaborated constructors for types.
    pub constructors: BTreeMap<Str, Constructor<Str>>,
}

/// Elaborated DependoBuf type.
#[derive(Debug)]
pub struct Type<Str> {
    /// List of elaborated dependencies.
    pub dependencies: Context<Str>,
    /// List of elaborated constructors' names.
    pub constructor_names: ConstructorNames<Str>,
}

/// Constructor names of a type.
#[derive(Debug)]
pub enum ConstructorNames<Str> {
    /// Message has a single constructor.
    OfMessage(Str),
    /// Enum has several constructors.
    OfEnum(BTreeSet<Str>),
}

/// Elaborated DependoBuf constructor.
#[derive(Debug)]
pub struct Constructor<Str> {
    /// List of elaborated implicit arguments' types.
    pub implicits: Context<Str>,
    /// List of elaborated explicit fields' types.
    pub fields: Context<Str>,
    /// Elaborated result type.
    pub result_type: TypeExpression<Str>,
}

/// Context is a list of typed variables.
pub type Context<Str> = Vec<(Str, TypeExpression<Str>)>;

/// Elaborated DependoBuf expression returning value.
#[derive(Clone, Debug)]
pub enum ValueExpression<Str> {
    /// An operator call.
    OpCall(OpCall<Str, Rec<Expression<Str>>>),
    /// Call to constructor.
    Constructor {
        name: Str,
        implicits: Exprs<Str>,
        arguments: Exprs<Str>,
    },
    /// Just a variable.
    Variable { name: Str },
}

/// Elaborated DependoBuf expression returning type.
#[derive(Clone, Debug)]
pub enum TypeExpression<Str> {
    /// Call to dependent type.
    TypeExpression {
        name: Str,
        dependencies: Exprs<ValueExpression<Str>>,
    },
}

/// Elaborated DependoBuf general expression.
#[derive(Clone, Debug)]
pub enum Expression<Str> {
    /// Value expression
    ValueExpression {
        expression: ValueExpression<Str>,
        its_type: TypeExpression<Str>,
    },
    TypeExpression(TypeExpression<Str>),
}

/// Shared list of subexpressions.
pub type Exprs<Expr> = Rec<[Expr]>;

/// Expression uses Rc for recursion.
/// Consider migrating to Arc when going multicore.
pub type Rec<T> = std::rc::Rc<T>;
