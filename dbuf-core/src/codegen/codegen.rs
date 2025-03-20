use std::rc::Rc;

use super::{
    ast::{self, Constructor, Expression, Module, Symbol, Type},
    identifiers::{self, Identifier, NamingScope},
    BoxAllocator, BoxDoc, DocAllocator, NEST_UNIT,
};

// TODO: make string here and in ast stored in arena and remove cloning

// TODO: introduce Codegen trait in order to generate helper functions. For example:
// suppose that we have implemented
// TypeDefinitionCodegen::generate(ty: &Type, context: CodegenContext<'a>, scope: &NamingScope<'_>) -> BoxDoc<'a>
// then it can automatically generate
// TypeDefinitionCodegen::generate(iter: impl Iterator<Item = &Type>, context: CodegenContext<'a>, scope: &NamingScope<'_>) -> impl Iterator<Item = BoxDoc<'a>
// saving tedious maps that I must write for now

#[derive(Clone, Copy)]
pub struct CodegenContext<'a> {
    pub alloc: &'a BoxAllocator,
}

impl<'a> Module {
    pub fn generate(&self, ctx: CodegenContext<'a>, scope: &mut NamingScope<'_>) -> BoxDoc<'a> {
        let alloc = ctx.alloc;
        let aliases = alloc
            .text("mod aliases {")
            .append(alloc.intersperse(
                [alloc.text("type Box<T> = std::boxed::Box<T>;")],
                alloc.line(),
            ))
            .append("}")
            .append(alloc.line())
            .into_doc();

        let types = self
            .types
            .iter()
            .map(|ty| (*ty).clone().generate(ctx, scope))
            .collect::<Vec<_>>();

        aliases.append(alloc.intersperse(types, alloc.line()))
    }
}

impl<'a> Type {
    pub fn generate(&self, ctx: CodegenContext<'a>, scope: &mut NamingScope<'_>) -> BoxDoc<'a> {
        let prelude =
            BoxDoc::text("use super::{Box, ConstructorError, Message}").append(BoxDoc::line());

        let module = BoxDoc::intersperse(
            [
                prelude,
                self.generate_dependencies_import(ctx, scope),
                self.generate_declaration(ctx, scope),
                self.generate_inherent_impl(ctx, scope),
            ],
            BoxDoc::line(),
        );

        BoxDoc::text(format!("mod {} {{", self.name))
            .append(BoxDoc::line())
            .append(module.nest(NEST_UNIT))
            .append(BoxDoc::text("}"))
    }
}

mod type_dependencies_import {
    use super::ast::{Constructor, Expression, Symbol, Type};
    use super::identifiers::{self, Identifier};
    use super::{BoxDoc, CodegenContext, DocAllocator, NamingScope, NEST_UNIT};
    use std::{collections::hash_set::HashSet, iter, rc::Rc};

    impl<'a> Type {
        pub(super) fn generate_dependencies_import(
            &self,
            ctx: CodegenContext<'a>,
            scope: &mut NamingScope<'_>,
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
                    } = (*constructor.upgrade().expect("missing type constructor")).clone();
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

            let helpers_deps = alloc
                .text("use super::{Box, ConstructorError, Message};")
                .into_doc();

            let other_type_deps = alloc
                .text("use super::super::{")
                .append(alloc.intersperse(
                    dependencies.into_iter().map(|name| {
                        let ty = identifiers::Type::from_name(name.clone())
                            .try_generate(ctx, scope)
                            .expect("couldn't import dependence");
                        let module = identifiers::Module::from_name(name.clone())
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
                .append("};")
                .append(alloc.line())
                .append(";")
                .into_doc();

            alloc
                .text("mod ")
                .append(deps)
                .append(" {")
                .append(
                    alloc
                        .intersperse([helpers_deps, other_type_deps], alloc.line())
                        .nest(NEST_UNIT),
                )
                .append("}")
                .into_doc()
        }

        fn expression_dependencies(expr: &Expression) -> HashSet<String> {
            let results = match expr {
                Expression::OpCall(op_call) => {
                    use crate::ast::operators::OpCall;
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
                    call: _, // dependencies of called constructor should not be matter
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
    use super::{BoxDoc, CodegenContext, DocAllocator, NamingScope, NEST_UNIT};
    use std::rc::Rc;

    impl<'a> Type {
        pub(super) fn generate_declaration(
            &self,
            ctx: CodegenContext<'a>,
            scope: &mut NamingScope<'_>,
        ) -> BoxDoc<'a> {
            let alloc = ctx.alloc;

            let body = self.generate_body(ctx, scope);
            let dependencies = self.generate_dependencies(ctx, scope);

            let name_alias = alloc
                .text("pub type ")
                .append(self.generate_comptime_type(ctx, scope))
                // deps module provided by type_dependencies_import
                .append(" = deps::Message<Body, Dependencies>;")
                .append(BoxDoc::line())
                .into_doc();

            alloc
                .intersperse([body, dependencies, name_alias], alloc.line())
                .into_doc()
        }

        fn generate_body(
            &self,
            ctx: CodegenContext<'a>,
            scope: &mut NamingScope<'_>,
        ) -> BoxDoc<'a> {
            let alloc = ctx.alloc;

            let body = identifiers::Type::from_name("body".to_owned())
                .try_generate(ctx, scope)
                .expect("failed to declare Body");

            // if I implement Codegen::generate as trait, those helper function can be generated automatically
            let generate_fields = |fields: &Vec<Rc<Symbol>>, scope: &mut NamingScope<'_>| {
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
                    let constructor = self.constructors[0]
                        .upgrade()
                        .expect("missing type constructor");
                    alloc.intersperse(generate_fields(&constructor.fields, scope), alloc.line())
                }
                ast::TypeKind::Enum => {
                    let mut branch_scope = NamingScope::nested_in(&scope);
                    let scope = &mut branch_scope;
                    alloc.intersperse(
                        self.constructors
                            .iter()
                            .map(|constructor| {
                                let constructor =
                                    &*constructor.upgrade().expect("missing type constructor");
                                let fields = generate_fields(&constructor.fields, scope);
                                // TODO: that's not really accurate, enum variant do not match with type behavior in
                                // scopes, because it will not shadow names in the scope where enum is declared
                                let name = identifiers::Type::from_name(constructor.name.clone())
                                    .try_generate(ctx, scope)
                                    .expect("failed to declare constructor (enum variant)");
                                name.append(" {")
                                    .append(
                                        alloc.intersperse(
                                            fields,
                                            alloc.text(",").append(alloc.line()),
                                        ),
                                    )
                                    .append("}")
                            })
                            .collect::<Vec<_>>(),
                        alloc.text(",").append(alloc.line()),
                    )
                }
            };

            let holder = match self.kind {
                ast::TypeKind::Message => "enum",
                ast::TypeKind::Enum => "struct",
            };
            alloc
                .text("#[derive(PartialEq, Eq)]")
                .append(alloc.line())
                .append(format!("pub {} ", holder))
                .append(body)
                .append(" {")
                .append(alloc.line())
                .append(fields.nest(NEST_UNIT))
                .append(alloc.text("}"))
                .append(alloc.line())
                .into_doc()
        }

        fn generate_dependencies(
            &self,
            context: CodegenContext<'a>,
            scope: &mut NamingScope<'_>,
        ) -> BoxDoc<'a> {
            let alloc = context.alloc;

            let dependencies = identifiers::Type::from_name("Dependencies".to_owned())
                .try_generate(context, scope)
                .expect("failed to define Dependencies");

            alloc
                .text("#[derive(PartialEq, Eq)]")
                .append(alloc.line())
                .append("struct ")
                .append(dependencies)
                .append(" {")
                .append(
                    alloc.intersperse(
                        self.dependencies
                            .iter()
                            .map(|symbol| {
                                symbol.clone().generate_as_field_declaration(context, scope)
                            })
                            .collect::<Vec<_>>(),
                        alloc.line(),
                    ),
                )
                .nest(NEST_UNIT)
                .append("}")
                .append(alloc.line())
                .into_doc()
        }
    }
}

impl<'a> Type {
    pub fn generate_type_dependencies_struct(
        &self,
        ctx: CodegenContext<'a>,
        scope: &mut NamingScope<'_>,
        values: Vec<BoxDoc<'a>>,
    ) -> BoxDoc<'a> {
        assert!(
            values.len() == self.dependencies.len(),
            "not enough/to much values to initialize Dependencies"
        );
        let alloc = ctx.alloc;
        identifiers::Type::from_name("Dependencies".to_owned())
            .get_generated(ctx, scope)
            .expect("Dependencies struct must be already generated")
            .append(" {")
            .append(alloc.line())
            .append(
                alloc.intersperse(
                    self.dependencies
                        .iter()
                        .zip(values.iter())
                        .map(|(symbol, value)| {
                            symbol
                                .clone()
                                .generate_as_field_name(ctx, scope)
                                .append(": ")
                                .append(value.clone())
                        }),
                    alloc.text(",").append(alloc.line()),
                ),
            )
            .append(alloc.line())
            .append("}")
    }
}

impl<'a> Type {
    pub fn generate_comptime_type(
        &self,
        ctx: CodegenContext<'a>,
        scope: &mut NamingScope<'_>,
    ) -> BoxDoc<'a> {
        identifiers::Type::from_name(self.name.clone())
            .try_generate(ctx, scope)
            .expect("failed to generate Type compile time type")
    }
}

mod type_inherent_impl {
    use super::ast::{self, Constructor, Expression, Symbol, Type};
    use super::identifiers::{self, Identifier};
    use super::{BoxDoc, CodegenContext, DocAllocator, NamingScope, NEST_UNIT};
    use std::{iter, rc::Rc};

    impl<'a> Type {
        pub(super) fn generate_inherent_impl(
            &self,
            context: CodegenContext<'a>,
            scope: &mut NamingScope<'_>,
        ) -> BoxDoc<'a> {
            let constructors = self.constructors.iter().map(|constructor| {
                let constructor = &*constructor.upgrade().expect("missing type constructor");
                constructor.generate_constructor_declaration(context, scope)
            });

            let setters = iter::once(BoxDoc::nil());

            let getters = iter::once(BoxDoc::nil());

            BoxDoc::intersperse(constructors.chain(setters).chain(getters), BoxDoc::line())
        }
    }

    impl<'a> Constructor {
        // this function is kind of mess.. TODO: when something like NodeId will be implemented, revisit this
        pub fn generate_constructor_declaration(
            &self,
            ctx: CodegenContext<'a>,
            scope: &mut NamingScope<'_>,
        ) -> BoxDoc<'a> {
            let alloc = ctx.alloc;
            let constructor_name = self.generate_constructor_name(ctx, scope);

            let mut constructor_scope = NamingScope::nested_in(scope);
            let scope = &mut constructor_scope;

            // order matters because there can be field name "dependencies" and I want to preserve params names
            let params = self
                .fields
                .iter()
                .map(|symbol| {
                    identifiers::Variable::from_symbol(symbol.clone())
                        .try_generate(ctx, scope)
                        .expect("failed to name constructor param")
                })
                .collect::<Vec<_>>();
            let dependencies_param =
                identifiers::Variable::from_name("dependencies".to_owned()).generate(ctx, scope);
            let params = iter::once(dependencies_param.clone())
                .chain(params)
                .collect::<Vec<_>>();

            let (ty, dependencies) = self.split_return_type();

            let ifelse2 = alloc.text("if (").append(alloc.intersperse(
                self.fields.iter().map(|symbol| {
                    let field = identifiers::Variable::from_symbol(symbol.clone())
                        .get_generated(ctx, scope)
                        .expect("constructor params must be already in the scope");
                    // field.
                }),
                separator,
            ));

            // I'm doing if let on tuples even if I need to match only one parameter, maybe TODO: change that in future
            let ifelse1 = alloc
                .text("if let (")
                .append(
                    alloc.intersperse(
                        dependencies
                            .iter()
                            .map(|expr| expr.generate_as_pattern(ctx, scope)),
                        alloc.text(",").append(alloc.line()),
                    ),
                )
                .append(") = (")
                .append(alloc.intersperse(
                    ty.dependencies.iter().map(|symbol| {
                        alloc
                            .text("&")
                            .append(dependencies_param.clone())
                            .append(".")
                            .append(symbol.clone().generate_as_field_name(ctx, scope))
                            .append(".")
                            .append("body")
                    }),
                    alloc.text(",").append(alloc.line()),
                ))
                .append(" {")
                .append(ifelse2.append(alloc.hardline()).nest(NEST_UNIT))
                .append("} else {")
                .append(
                    Self::generate_constructor_error(ctx, scope)
                        .append(alloc.hardline())
                        .nest(NEST_UNIT),
                )
                .append("}");

            let body_var = identifiers::Variable::from_name("body".to_owned()).generate(ctx, scope);

            let constructor_body = alloc
                .text("let ")
                .append(body_var.clone())
                .append(" = ")
                .append(ifelse1)
                .append("?;")
                .append(alloc.hardline())
                .append("Ok(deps::Message { body: ")
                .append(body_var)
                .append(", dependencies: ")
                .append(dependencies_param)
                .append(" })")
                .into_doc();

            alloc
                .text(format!("pub fn",))
                .append(constructor_name)
                .append("(")
                .append(alloc.intersperse(params, alloc.text(", ")))
                .append(") -> Result<Self, deps::ConstructorError> {")
                .append(constructor_body.nest(NEST_UNIT))
                .into_doc()
        }

        fn generate_constructor_error(
            ctx: CodegenContext<'a>,
            _: &mut NamingScope<'_>,
        ) -> BoxDoc<'a> {
            ctx.alloc
                .text("Err(deps::ConstructorError::MismatchedDependencies)")
                .into_doc()
        }
    }

    impl<'a> Expression {
        pub fn generate_as_pattern(
            &self,
            ctx: CodegenContext<'a>,
            scope: &mut NamingScope<'_>,
        ) -> BoxDoc<'a> {
        }
    }
}

mod value_from_expression {
    use std::iter;

    use super::ast::{Constructor, Expression};
    use super::identifiers::{self, Identifier};
    use super::{BoxDoc, CodegenContext, DocAllocator, NamingScope, NEST_UNIT};

    impl<'a> Expression {
        pub(super) fn generate_as_value(
            &self,
            ctx: CodegenContext<'a>,
            scope: &mut NamingScope<'a>,
        ) -> BoxDoc<'a> {
            let alloc = ctx.alloc;
            match self {
                Expression::OpCall(op_call) => todo!(),
                Expression::Type { call, dependencies } => todo!(),
                Expression::Constructor {
                    call,
                    implicits,
                    arguments,
                } => {
                    let (constructor, provided_deps, arguments) = if let Expression::Constructor {
                        call,
                        implicits,
                        arguments,
                    } = self
                    {
                        (
                            call.upgrade().expect("call to unknown constructor"),
                            implicits,
                            arguments,
                        )
                    } else {
                        panic!("expected constructor call expression")
                    };
                    let dependencies = alloc.text();
                    alloc.text(ty.name.clone()).into_doc()
                }
                Expression::Variable(weak) => todo!(),
            }
        }
    }

    impl<'a> Constructor {
        fn generate_call_as_value(
            &self,
            ctx: CodegenContext<'a>,
            scope: &mut NamingScope<'a>,
            implicits: Vec<Expression>,
            arguments: Vec<Expression>,
        ) -> BoxDoc<'a> {
            assert!(arguments.len() == self.fields.len());

            let alloc = ctx.alloc;
            
            let (ty, type_deps) = self.split_return_type();
            let dependencies = type_deps
                .iter()
                .map(|expr| expr.generate_as_value(ctx, scope))
                .collect();
            let deps_module = identifiers::Module::from_name("deps".to_owned())
                .get_generated(ctx, scope)
                .expect("missing deps module");

            let mut deps_scope = NamingScope::nested_in(scope);
            let constructor_call = deps_module
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
            let dependencies_struct = deps_module
                .append("::")
                // TODO: same to what written above.
                .append(
                    identifiers::Module::from_name(ty.name.clone())
                        .try_generate(ctx, &mut deps_scope)
                        .expect("impossible"),
                )
                .append("::")
                .append(ty.generate_type_dependencies_struct(ctx, scope, dependencies));

            constructor_call
                .append("(")
                .append(alloc.intersperse(
                    iter::once(dependencies_struct).chain(
                        arguments.
                    ),
                    alloc.text(",").append(alloc.line()),
                ))
                .append(")")
        }
    }
}

impl<'a> Constructor {
    pub fn generate_constructor_name(
        &self,
        ctx: CodegenContext<'a>,
        scope: &mut NamingScope<'_>,
    ) -> BoxDoc<'a> {
        identifiers::Function::from_name(self.name.clone())
            .try_generate(ctx, scope)
            .expect("failed to generate constructor name")
    }
}

impl<'a> Symbol {
    pub fn generate_as_field_declaration(
        self: Rc<Self>,
        ctx: CodegenContext<'a>,
        scope: &mut NamingScope<'_>,
    ) -> BoxDoc<'a> {
        let name = identifiers::Variable::from_symbol(self.clone()).generate(ctx, scope);
        let ty = self.ty.generate_type_expression(ctx, scope);
        name.append(": ").append(ty)
    }
}

impl<'a> Symbol {
    pub fn generate_as_field_name(
        self: Rc<Self>,
        ctx: CodegenContext<'a>,
        scope: &mut NamingScope<'_>,
    ) -> BoxDoc<'a> {
        identifiers::Variable::from_symbol(self.clone()).generate(ctx, scope)
    }
}

impl<'a> Expression {
    pub fn generate_type_expression(
        &self,
        ctx: CodegenContext<'a>,
        scope: &mut NamingScope<'_>,
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
