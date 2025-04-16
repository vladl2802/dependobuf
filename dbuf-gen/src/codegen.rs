use std::{io, rc::Rc};

use crate::{
    BoxAllocator, BoxDoc, DocAllocator, NEST_UNIT,
    ast::{self, Constructor, Expression, Module, Symbol, Type},
    identifiers::{self, Identifier, NamingScope},
    object_id::ObjectId,
};

#[derive(Clone, Copy)]
pub struct CodegenContext<'a> {
    pub alloc: &'a BoxAllocator,
}

pub fn generate_module<Writer: io::Write>(
    module: ast::elaborated::Module<String>,
    w: &mut Writer,
) -> io::Result<()> {
    let allocator = BoxAllocator;
    let ctx = CodegenContext { alloc: &allocator };
    let mut scope = NamingScope::empty();
    let module = ast::Module::from_elaborated(module);
    let doc = module.generate(ctx, &mut scope);
    doc.render(40, w)
}

// TODO: make string here and in ast stored in arena and remove cloning

// TODO: introduce Codegen trait in order to generate helper functions. For example:
// suppose that we have implemented
// TypeDefinitionCodegen::generate(ty: &Type, context: CodegenContext<'a>, scope: &NamingScope<'_>) -> BoxDoc<'a>
// then it can automatically generate
// TypeDefinitionCodegen::generate(iter: impl Iterator<Item = &Type>, context: CodegenContext<'a>, scope: &NamingScope<'_>) -> impl Iterator<Item = BoxDoc<'a>
// saving tedious maps that I must write for now

impl<'a> Module {
    pub fn generate(&self, ctx: CodegenContext<'a>, scope: &mut NamingScope<'_, 'a>) -> BoxDoc<'a> {
        let alloc = ctx.alloc;

        let types = self
            .types
            .iter()
            .map(|ty| (*ty).clone().generate(ctx, scope))
            .collect::<Vec<_>>();

        alloc
            .intersperse(types, alloc.hardline())
            .append(alloc.hardline())
            .into_doc()
    }
}

impl<'a> Type {
    pub fn generate(&self, ctx: CodegenContext<'a>, scope: &mut NamingScope<'_, 'a>) -> BoxDoc<'a> {
        let mut type_scope = NamingScope::nested_in(scope);

        let alloc = ctx.alloc;

        let module = alloc.intersperse(
            [
                self.generate_dependencies_import(ctx, &mut type_scope),
                self.generate_declaration(ctx, &mut type_scope),
                self.generate_inherent_impl(ctx, &mut type_scope),
            ],
            alloc.hardline(),
        );

        alloc
            .text("pub mod")
            .append(alloc.space())
            // TODO: allocate module here
            .append(self.name.clone().to_lowercase())
            .append(alloc.space())
            .append("{")
            .append(
                alloc
                    .hardline()
                    .append(module)
                    .nest(NEST_UNIT)
                    .append(alloc.hardline()),
            )
            .append("}")
            .append(alloc.hardline())
            .append(alloc.hardline())
            .append("pub use")
            .append(alloc.space())
            // TODO: use allocated module
            .append(self.name.clone().to_lowercase())
            .append("::")
            // TODO: get generate type name
            .append(self.name.clone())
            .append(";")
            .append(alloc.hardline())
            .into_doc()
    }
}

mod type_dependencies_import {
    use super::ast::{Constructor, Expression, Symbol, Type, operators};
    use super::identifiers::{self, Identifier};
    use super::{BoxDoc, CodegenContext, DocAllocator, NEST_UNIT, NamingScope};
    use std::{collections::hash_set::HashSet, iter, rc::Rc};

    impl<'a> Type {
        pub(super) fn generate_dependencies_import(
            &self,
            ctx: CodegenContext<'a>,
            scope: &mut NamingScope<'_, 'a>,
        ) -> BoxDoc<'a> {
            let deps = identifiers::Module::from_name("deps".to_owned())
                .try_generate(ctx, scope)
                .expect("couldn't create 'deps' module");
            let mut deps_scope = NamingScope::nested_in(&scope);
            let scope = &mut deps_scope;
            let alloc = ctx.alloc;

            let mut dependencies: HashSet<_> = self
                .constructors
                .iter()
                .map(|constructor| {
                    let Constructor {
                        name: _,
                        implicits,
                        fields,
                        result_type,
                    } = constructor.as_ref();
                    let implicits = implicits.iter().map(Self::symbol_dependencies);
                    let fields = fields.iter().map(Self::symbol_dependencies);
                    iter::once(Self::expression_dependencies(&result_type))
                        .chain(implicits)
                        .chain(fields)
                        .into_iter()
                        .flatten()
                        .collect()
                })
                .chain(self.dependencies.iter().map(Self::symbol_dependencies))
                .into_iter()
                .flatten()
                .collect();

            dependencies.remove(&*self.name); // don't need to return self name

            let helpers_deps =
                alloc.text("pub use super::super::{Box, ConstructorError, Message};");

            let other_type_deps = if dependencies.is_empty() {
                alloc.text("// ")
            } else {
                alloc.nil()
            };

            let other_type_deps = other_type_deps
                .append(alloc.text("pub use super::super::{"))
                .append(alloc.intersperse(
                    dependencies.into_iter().map(|name| {
                        let ty = identifiers::Type::from_name(name.clone())
                            .try_generate(ctx, scope)
                            .expect("couldn't import dependence");
                        let module = identifiers::Module::from_name(name.clone().to_lowercase())
                            .try_generate(ctx, scope)
                            .expect("couldn't import dependence");
                        alloc
                            .text("{")
                            .append(ty)
                            .append(", ")
                            .append(module)
                            .append("}")
                    }),
                    alloc.text(", "),
                ))
                .append("};");

            alloc
                .text("mod ")
                .append(deps)
                .append(" {")
                .append(
                    alloc
                        .hardline()
                        .append(
                            alloc.intersperse([helpers_deps, other_type_deps], alloc.hardline()),
                        )
                        .nest(NEST_UNIT)
                        .append(alloc.hardline()),
                )
                .append("}")
                .append(alloc.hardline())
                .into_doc()
        }

        fn expression_dependencies(expr: &Expression) -> HashSet<String> {
            let results = match expr {
                Expression::OpCall(op_call) => {
                    use operators::OpCall;
                    match op_call {
                        OpCall::Literal(_) => vec![], // no deps in literal
                        OpCall::Unary(_, expr) => vec![Self::expression_dependencies(expr)],
                        OpCall::Binary(_, lhs, rhs) => {
                            vec![
                                Self::expression_dependencies(lhs),
                                Self::expression_dependencies(rhs),
                            ]
                        }
                    }
                }
                Expression::Type { call, dependencies } => {
                    let ty = (*call.upgrade().expect("missing type in expression")).clone();
                    let name_deps = if Self::is_primitive_type(&ty.name) {
                        iter::empty().collect()
                    } else {
                        iter::once(ty.name).collect()
                    };
                    iter::once(name_deps)
                        .chain(dependencies.into_iter().map(Self::expression_dependencies))
                        .collect()
                }
                Expression::Constructor {
                    call: _, // dependencies in called constructor should not matter
                    implicits,
                    arguments,
                } => implicits
                    .into_iter()
                    .map(Self::expression_dependencies)
                    .chain(arguments.into_iter().map(Self::expression_dependencies))
                    .collect(),
                Expression::Variable(symbol) => {
                    vec![Self::symbol_dependencies(
                        &symbol.upgrade().expect("missing type in symbol"),
                    )] // this is maybe unnecessary
                }
            };
            results.into_iter().flatten().collect()
        }

        fn symbol_dependencies(symbol: &Rc<Symbol>) -> HashSet<String> {
            Self::expression_dependencies(&(**symbol).clone().ty)
        }

        // TODO: move this to more appropriate place
        fn is_primitive_type(name: &str) -> bool {
            match name {
                "Bool" => true,
                "Double" => true,
                "Int" => true,
                "UInt" => true,
                "String" => true,
                _ => false,
            }
        }
    }
}

mod type_declaration {
    use super::ast::{self, Symbol, Type};
    use super::identifiers::{self, Identifier};
    use super::{BoxDoc, CodegenContext, DocAllocator, NEST_UNIT, NamingScope, ObjectId};
    use std::rc::Rc;

    impl<'a, 'b> Type {
        pub(super) fn generate_declaration(
            &'b self,
            ctx: CodegenContext<'a>,
            scope: &mut NamingScope<'b, 'a>,
        ) -> BoxDoc<'a> {
            let alloc = ctx.alloc;

            let body = self.generate_body(ctx, scope);
            let dependencies = self.generate_dependencies(ctx, scope);

            let name_alias = alloc
                .text("pub type ")
                .append(self.generate_comptime_type(ctx, scope))
                // deps module provided by type_dependencies_import
                .append(" = deps::Message<Body, Dependencies>;")
                .append(alloc.hardline())
                .into_doc();

            alloc
                .intersperse([body, dependencies, name_alias], alloc.hardline())
                .into_doc()
        }

        fn generate_body(
            &self,
            ctx: CodegenContext<'a>,
            scope: &mut NamingScope<'_, 'a>,
        ) -> BoxDoc<'a> {
            let alloc = ctx.alloc;

            let body = identifiers::Type::from_name("Body".to_owned())
                .try_generate(ctx, scope)
                .expect("failed to declare Body");

            // if I implement Codegen::generate as trait, those helper function can be generated automatically
            let generate_fields = |fields: &Vec<Rc<Symbol>>, scope: &mut NamingScope<'_, 'a>| {
                fields
                    .iter()
                    .map(|symbol| symbol.clone().generate_as_field_declaration(ctx, scope))
                    .collect::<Vec<_>>()
            };

            let fields = match self.kind {
                ast::TypeKind::Message => {
                    debug_assert!(
                        self.constructors.len() == 1,
                        "Message expected to have only one constructor"
                    );
                    let constructor = self.constructors[0].as_ref();
                    alloc.intersperse(
                        generate_fields(&constructor.fields, scope),
                        alloc.hardline(),
                    )
                }
                ast::TypeKind::Enum => {
                    let mut branch_scope = NamingScope::nested_in(&scope);
                    let scope = &mut branch_scope;
                    alloc.intersperse(
                        self.constructors
                            .iter()
                            .map(|constructor| {
                                let constructor = constructor.as_ref();
                                let fields = generate_fields(&constructor.fields, scope);
                                // TODO: that's not really accurate, enum variant do not match with type behavior in
                                // scopes, because it will not shadow names in the scope where enum is declared
                                let name = identifiers::Type::from_name(constructor.name.clone())
                                    .try_generate(ctx, scope)
                                    .expect("failed to declare constructor (enum variant)");
                                name.append(" {")
                                    .append(
                                        alloc
                                            .hardline()
                                            .append(alloc.intersperse(
                                                fields,
                                                alloc.text(",").append(alloc.hardline()),
                                            ))
                                            .nest(NEST_UNIT)
                                            .append(alloc.hardline()),
                                    )
                                    .append("}")
                            })
                            .collect::<Vec<_>>(),
                        alloc.text(",").append(alloc.hardline()),
                    )
                }
            };

            let holder = match self.kind {
                ast::TypeKind::Message => "struct",
                ast::TypeKind::Enum => "enum",
            };
            alloc
                .text("#[derive(PartialEq, Eq)]")
                .append(alloc.hardline())
                .append(format!("pub {} ", holder))
                .append(body)
                .append(" {")
                .append(
                    alloc
                        .hardline()
                        .append(fields)
                        .nest(NEST_UNIT)
                        .append(alloc.hardline()),
                )
                .append("}")
                .append(alloc.hardline())
                .into_doc()
        }

        fn generate_dependencies(
            &self,
            context: CodegenContext<'a>,
            scope: &mut NamingScope<'_, 'a>,
        ) -> BoxDoc<'a> {
            let alloc = context.alloc;

            let dependencies = identifiers::Type::from_name("Dependencies".to_owned())
                .try_generate(context, scope)
                .expect("failed to define Dependencies");

            alloc
                .text("#[derive(PartialEq, Eq)]")
                .append(alloc.hardline())
                .append("struct ")
                .append(dependencies)
                .append(" {")
                .append(
                    alloc
                        .hardline()
                        .append(
                            alloc.intersperse(
                                self.dependencies
                                    .iter()
                                    .map(|symbol| {
                                        symbol.clone().generate_as_field_declaration(context, scope)
                                    })
                                    .collect::<Vec<_>>(),
                                alloc.hardline(),
                            ),
                        )
                        .nest(NEST_UNIT)
                        .append(alloc.hardline()),
                )
                .append("}")
                .append(alloc.hardline())
                .into_doc()
        }
    }
}

impl<'a> Type {
    pub fn generate_type_dependencies_struct(
        &self,
        ctx: CodegenContext<'a>,
        scope: &mut NamingScope<'_, 'a>,
        values: Vec<BoxDoc<'a>>,
    ) -> BoxDoc<'a> {
        assert!(
            values.len() == self.dependencies.len(),
            "not enough/to much values to initialize Dependencies"
        );
        let alloc = ctx.alloc;

        let deps_struct = identifiers::Type::from_name("Dependencies".to_owned())
            .try_generate(ctx, scope)
            .expect("could not generate Dependencies struct");

        let mut fields_scope = NamingScope::empty();

        deps_struct
            .append(" {")
            .append(
                alloc
                    .hardline()
                    .append(
                        alloc.intersperse(
                            self.dependencies
                                .iter()
                                .zip(values.iter())
                                .map(|(symbol, value)| {
                                    symbol
                                        .clone()
                                        .generate_as_field_name(ctx, &mut fields_scope)
                                        .append(": ")
                                        .append(value.clone())
                                }),
                            alloc.text(",").append(alloc.hardline()),
                        ),
                    )
                    .nest(NEST_UNIT)
                    .append(alloc.hardline()),
            )
            .append("}")
    }
}

impl<'a> Type {
    pub fn generate_comptime_type(
        &self,
        ctx: CodegenContext<'a>,
        scope: &mut NamingScope<'_, 'a>,
    ) -> BoxDoc<'a> {
        // how it should be
        // identifiers::get_generated(ctx, scope, ObjectId::id(self))
        //     .expect("could not generate comptime type of Type")

        // how it is
        identifiers::Type::from_name(self.name.clone())
            .try_generate(ctx, scope)
            .or_else(|| identifiers::get_generated(ctx, scope, ObjectId::owned(self.name.clone())))
            .expect("impossible")
    }
}

mod type_inherent_impl {
    use super::ast::{Constructor, Expression, Type};
    use super::identifiers::{self, Identifier};
    use super::{BoxDoc, CodegenContext, DocAllocator, NEST_UNIT, NamingScope, ObjectId};

    impl<'a> Type {
        pub(super) fn generate_inherent_impl(
            &self,
            ctx: CodegenContext<'a>,
            scope: &mut NamingScope<'_, 'a>,
        ) -> BoxDoc<'a> {
            let alloc = ctx.alloc;

            let constructors = self
                .constructors
                .iter()
                .map(|constructor| {
                    let constructor = constructor.as_ref();
                    constructor.generate_constructor_declaration(ctx, scope)
                })
                .collect::<Vec<_>>();

            alloc
                .text("impl")
                .append(alloc.space())
                .append(self.generate_comptime_type(ctx, scope))
                .append(alloc.space())
                .append("{")
                .append(
                    alloc
                        .hardline()
                        .append(alloc.intersperse(constructors, alloc.hardline()))
                        .nest(NEST_UNIT)
                        .append(alloc.hardline()),
                )
                .append("}")
                .into_doc()
        }
    }

    impl<'a> Constructor {
        pub fn generate_constructor_declaration(
            &self,
            ctx: CodegenContext<'a>,
            scope: &mut NamingScope<'_, 'a>,
        ) -> BoxDoc<'a> {
            let alloc = ctx.alloc;
            let constructor_name = self.generate_constructor_name(ctx, scope);

            let mut constructor_scope = NamingScope::nested_in(scope);
            let scope = &mut constructor_scope;

            let params = self
                .implicits
                .iter()
                .map(|symbol| symbol.clone().generate_as_field_declaration(ctx, scope))
                .collect::<Vec<_>>()
                .into_iter()
                .chain(
                    self.fields
                        .iter()
                        .map(|symbol| symbol.clone().generate_as_field_declaration(ctx, scope)),
                )
                .collect::<Vec<_>>();

            let (ty, dependencies) = self.split_return_type();

            let body_var = identifiers::Variable::from_name("body".to_owned()).generate(ctx, scope);
            let dependencies_var =
                identifiers::Variable::from_name("dependencies".to_owned()).generate(ctx, scope);

            let body_initialization = alloc
                .text("let ")
                .append(body_var.clone())
                .append(" = ")
                .append(
                    self.generate_type_checker_if(ctx, scope)
                        .append(" {")
                        .append(
                            alloc
                                .hardline()
                                .append(
                                    self.generate_as_body_construction(
                                        ctx,
                                        scope,
                                        self.fields
                                            .iter()
                                            .map(|field| {
                                                identifiers::get_generated(
                                                    ctx,
                                                    scope,
                                                    ObjectId::id_rc(field),
                                                )
                                                .expect("could not find passed field")
                                            })
                                            .collect(),
                                    ),
                                )
                                .nest(NEST_UNIT)
                                .append(alloc.hardline()),
                        )
                        .append("} else {")
                        .append(
                            alloc
                                .hardline()
                                .append(Self::generate_constructor_error(ctx, scope))
                                .nest(NEST_UNIT)
                                .append(alloc.hardline()),
                        )
                        .append("}"),
                )
                .append("?;");

            let dependencies = dependencies
                .iter()
                .map(|expr| expr.generate_as_value(ctx, scope))
                .collect();

            let mut deps_scope = NamingScope::empty();

            let dependencies_initialization = alloc
                .text("let ")
                .append(dependencies_var.clone())
                .append(" = ")
                .append(ty.generate_type_dependencies_struct(ctx, &mut deps_scope, dependencies))
                .append(";");

            let constructor_body = body_initialization
                .append(alloc.hardline())
                .append(dependencies_initialization)
                .append(alloc.hardline())
                .append("Ok(deps::Message { body: ")
                .append(body_var)
                .append(", dependencies: ")
                .append(dependencies_var)
                .append(" })");
            alloc
                .text(format!("pub fn",))
                .append(alloc.space())
                .append(constructor_name)
                .append("(")
                .append(alloc.intersperse(params, alloc.text(", ")))
                .append(") -> Result<Self, deps::ConstructorError> {")
                .append(
                    alloc
                        .hardline()
                        .append(constructor_body)
                        .nest(NEST_UNIT)
                        .append(alloc.hardline()),
                )
                .append("}")
                .into_doc()
        }

        fn generate_type_checker_if(
            &self,
            ctx: CodegenContext<'a>,
            scope: &mut NamingScope<'_, 'a>,
        ) -> BoxDoc<'a> {
            let alloc = ctx.alloc;

            alloc
                .text("if (")
                .append(alloc.intersperse(
                    self.fields.iter().map(|symbol| {
                        if let Expression::Type {
                            call: _,
                            dependencies,
                        } = &symbol.ty
                        {
                            alloc
                                .text("(")
                                .append(
                                    alloc.intersperse(
                                        dependencies
                                            .iter()
                                            .map(|expr| expr.generate_as_value(ctx, scope)),
                                        alloc.text(",").append(alloc.line()),
                                    ),
                                )
                                .append(")")
                        } else {
                            panic!("symbol ty is not type expression")
                        }
                    }),
                    alloc.text(",").append(alloc.line()),
                ))
                .append(") == (")
                .append(alloc.intersperse(
                    self.fields.iter().map(|symbol| {
                        let field = identifiers::get_generated(ctx, scope, ObjectId::id_rc(symbol))
                            .expect("constructor params must be already in the scope");

                        let field_dependencies = identifiers::get_generated(
                            ctx,
                            scope,
                            ObjectId::owned("dependencies".to_owned()),
                        )
                        .expect("failed to get 'dependencies' message field");

                        let mut field_dependencies_scope = NamingScope::empty();

                        if let Expression::Type {
                            call,
                            dependencies: _,
                        } = &symbol.ty
                        {
                            let ty = call
                                .upgrade()
                                .expect("type expression calls to unknown type");

                            alloc
                                .text("(")
                                .append(alloc.intersperse(
                                    ty.dependencies.iter().map(|dep_field| {
                                        field
                                            .clone()
                                            .append(".")
                                            .append(field_dependencies.clone())
                                            .append(".")
                                            .append(
                                                identifiers::Variable::from_object(
                                                    ObjectId::id_rc(dep_field),
                                                    dep_field.name.clone(),
                                                )
                                                .try_generate(ctx, &mut field_dependencies_scope)
                                                .expect("failed to access dependency"),
                                            )
                                    }),
                                    alloc.text(",").append(alloc.line()),
                                ))
                                .append(")")
                        } else {
                            panic!("symbol ty is not type expression")
                        }
                    }),
                    alloc.text(",").append(alloc.line()),
                ))
                .append(")")
                .into_doc()
        }

        fn generate_constructor_error(
            ctx: CodegenContext<'a>,
            _: &mut NamingScope<'_, 'a>,
        ) -> BoxDoc<'a> {
            // Dirty, because doesn't check that deps include this
            ctx.alloc
                .text("Err(deps::ConstructorError::MismatchedDependencies)")
                .into_doc()
        }
    }
}

mod value_from_expression {
    use super::ast::{Constructor, Expression, OpCall, operators};
    use super::identifiers::{self, Identifier};
    use super::{BoxDoc, CodegenContext, DocAllocator, NamingScope, ObjectId};
    use std::iter;

    impl<'a> Expression {
        pub(super) fn generate_as_value(
            &self,
            ctx: CodegenContext<'a>,
            scope: &mut NamingScope<'_, 'a>,
        ) -> BoxDoc<'a> {
            match self {
                Expression::OpCall(op_call) => generate_op_as_value(op_call, ctx, scope),
                Expression::Type {
                    call: _,
                    dependencies: _,
                } => panic!("type expression could not be generated as value"),
                Expression::Constructor {
                    call,
                    implicits,
                    arguments,
                } => call
                    .upgrade()
                    .expect("call to unknown constructor")
                    .generate_call_as_value(ctx, scope, implicits, arguments),
                Expression::Variable(weak) => {
                    identifiers::get_generated(ctx, scope, ObjectId::id_weak(weak))
                        .expect("unknown variable")
                }
            }
        }
    }

    fn generate_op_as_value<'a>(
        op_call: &OpCall,
        ctx: CodegenContext<'a>,
        scope: &mut NamingScope<'_, 'a>,
    ) -> BoxDoc<'a> {
        use operators::{BinaryOp, Literal, UnaryOp};
        let alloc = ctx.alloc;
        match op_call {
            OpCall::Literal(literal) => {
                let string = match literal {
                    Literal::Bool(val) => val.to_string(),
                    Literal::Double(val) => val.to_string(),
                    Literal::Int(val) => val.to_string(),
                    Literal::UInt(val) => val.to_string(),
                    Literal::Str(val) => val.clone(),
                };
                alloc.text(string).into_doc()
            }
            OpCall::Unary(unary_op, operand) => {
                let operand = operand.generate_as_value(ctx, scope);
                match unary_op {
                    UnaryOp::Access(name) => operand.append(
                        identifiers::Variable::from_object(
                            ObjectId::owned(name.clone()),
                            name.clone(),
                        )
                        .try_generate(ctx, scope)
                        .expect("failed to generate field declaration"),
                    ),
                    UnaryOp::Minus => ctx.alloc.text("-").append(operand).into_doc(),
                    UnaryOp::Bang => ctx.alloc.text("!").append(operand).into_doc(),
                }
            }
            OpCall::Binary(binary_op, lhs, rhs) => {
                let op = match binary_op {
                    BinaryOp::Plus => alloc.text("+"),
                    BinaryOp::Minus => alloc.text("-"),
                    BinaryOp::Star => alloc.text("*"),
                    BinaryOp::Slash => alloc.text("/"),
                    BinaryOp::And => alloc.text("&"),
                    BinaryOp::Or => alloc.text("|"),
                };
                alloc
                    .text("(")
                    .append(lhs.generate_as_value(ctx, scope))
                    .append(alloc.space())
                    .append(op)
                    .append(alloc.space())
                    .append(rhs.generate_as_value(ctx, scope))
                    .append(")")
                    .into_doc()
            }
        }
    }

    impl<'a> Constructor {
        fn generate_call_as_value(
            &self,
            ctx: CodegenContext<'a>,
            scope: &mut NamingScope<'_, 'a>,
            implicits: &Vec<Expression>,
            arguments: &Vec<Expression>,
        ) -> BoxDoc<'a> {
            assert!(arguments.len() == self.fields.len());

            let alloc = ctx.alloc;

            let (ty, _) = self.split_return_type();
            let deps_module =
                identifiers::get_generated(ctx, scope, ObjectId::owned("deps".to_owned()))
                    .expect("missing deps module");

            let mut deps_scope = NamingScope::empty();
            let constructor_func = deps_module
                .clone()
                .append("::")
                // TODO: this is bad, because it avoids identifier::Type get_generated. The reason why it avoids is following:
                // current NamingScope is stack-based, meaning that you have access only to the current stack of the names.
                // But deps is separate module, so it was already popped from NamingScope stack. So in current model desired
                // behavior can't be achieved.
                // Also, there is an additional argument against naming scope in its current form: such stack-based behavior is only
                // natural to the Variables. But it is completely unnatural to the Modules, Types and Functions. Meaning, there should
                // be another more suitable abstraction for them.
                .append(
                    identifiers::Type::from_name(ty.name.clone())
                        .try_generate(ctx, &mut deps_scope)
                        .expect("impossible"),
                )
                .append("::")
                // TODO: same to what written above
                .append(
                    identifiers::Function::from_name(self.name.clone())
                        .try_generate(ctx, &mut deps_scope)
                        .expect("impossible"),
                );

            constructor_func
                .append("(")
                .append(
                    alloc.intersperse(
                        implicits
                            .iter()
                            .map(|expr| expr.generate_as_value(ctx, scope))
                            .collect::<Vec<_>>()
                            .into_iter()
                            .chain(
                                arguments
                                    .iter()
                                    .map(|expr| expr.generate_as_value(ctx, scope)),
                            ),
                        alloc.text(",").append(alloc.line()),
                    ),
                )
                .append(")")
                .append(".expect(")
                .append(format!(
                    "\"constructor '{}::{}' failed\"",
                    ty.name, self.name
                ))
                .append(")")
        }
    }
}

impl<'a> Constructor {
    pub fn generate_constructor_name(
        &self,
        ctx: CodegenContext<'a>,
        scope: &mut NamingScope<'_, 'a>,
    ) -> BoxDoc<'a> {
        identifiers::Function::from_name(self.name.clone())
            .try_generate(ctx, scope)
            .expect("failed to generate constructor name")
    }
}

impl<'a> Constructor {
    pub fn generate_as_body_construction(
        &self,
        ctx: CodegenContext<'a>,
        scope: &mut NamingScope<'_, 'a>,
        fields: Vec<BoxDoc<'a>>,
    ) -> BoxDoc<'a> {
        assert!(
            self.fields.len() == fields.len(),
            "unexpected amount of fields passed in"
        );

        let alloc = ctx.alloc;

        let body_struct =
            identifiers::get_generated(ctx, scope, ObjectId::owned("Body".to_owned()).to_owned())
                .expect("could not find Body enum/struct");

        let mut type_module_scope = NamingScope::empty();

        let (ty, _) = self.split_return_type();
        let constructor = match ty.kind {
            ast::TypeKind::Message => body_struct,
            ast::TypeKind::Enum => body_struct.append("::").append(
                identifiers::Type::from_name(self.name.clone())
                    .try_generate(ctx, &mut type_module_scope)
                    .expect("could not generate enum branch constructor"),
            ),
        };

        let mut constructor_scope = NamingScope::nested_in(&type_module_scope);

        alloc
            .text("Ok(")
            .append(constructor)
            .append(alloc.space())
            .append("{")
            .append(
                alloc
                    .hardline()
                    .append(
                        alloc.intersperse(
                            self.fields
                                .iter()
                                .zip(fields.into_iter())
                                .map(|(field, value)| {
                                    identifiers::Variable::from_object(
                                        ObjectId::id_rc(field),
                                        field.name.clone(),
                                    )
                                    .try_generate(ctx, &mut constructor_scope)
                                    .expect("could not generate constructor field")
                                    .append(": ")
                                    .append(value)
                                }),
                            alloc.text(",").append(alloc.hardline()),
                        ),
                    )
                    .nest(NEST_UNIT)
                    .append(alloc.hardline()),
            )
            .append("}")
            .append(")")
            .into_doc()
    }
}

impl<'a, 'b> Symbol {
    pub fn generate_as_field_declaration(
        self: Rc<Self>,
        ctx: CodegenContext<'a>,
        scope: &mut NamingScope<'b, 'a>,
    ) -> BoxDoc<'a> {
        let ty = ctx
            .alloc
            .text("deps::Box<")
            .append(self.ty.generate_type_expression(ctx, scope))
            .append(">");
        self.generate_as_field_name(ctx, scope)
            .append(": ")
            .append(ty)
    }
}

impl<'a> Symbol {
    pub fn generate_as_field_name(
        self: Rc<Self>,
        ctx: CodegenContext<'a>,
        scope: &mut NamingScope<'_, 'a>,
    ) -> BoxDoc<'a> {
        identifiers::Variable::from_object(ObjectId::id_rc(&self), self.name.clone())
            .try_generate(ctx, scope)
            .expect("failed to generate field declaration")
    }
}

impl<'a> Expression {
    pub fn generate_type_expression(
        &self,
        ctx: CodegenContext<'a>,
        scope: &mut NamingScope<'_, 'a>,
    ) -> BoxDoc<'a> {
        match self {
            Expression::OpCall(_) => panic!("OpCall in type expression"),
            Expression::Type {
                call,
                dependencies: _,
            } => {
                // dependencies are runtime so they are not relevant in call
                call.upgrade()
                    .expect("call to unknown type")
                    .generate_comptime_type(ctx, scope)
            }
            Expression::Constructor {
                call: _,
                implicits: _,
                arguments: _,
            } => panic!("Constructor in type expression"),
            Expression::Variable(_) => panic!("Variable in type expression"),
        }
    }
}
