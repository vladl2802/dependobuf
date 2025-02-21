use std::collections::HashSet;
use std::iter;

use super::{helpers, BoxDoc, Scope, NEST_UNIT};
use super::{Constructor, Context, ElaboratedConstructor, Expression, ElaboratedModule, Str, TypeExpression};

impl<'a> Type<'a> {
    // generates type definition
    pub fn generate_definition(&'a self) -> BoxDoc<'a> {
        let body = self.generate_body();
        let dependencies = BoxDoc::text("#[derive(PartialEq, Eq)]")
            .append(BoxDoc::line())
            .append(BoxDoc::text("struct Dependencies {"))
            .append(helpers::generate_fields(self.dependencies).nest(NEST_UNIT))
            .append(BoxDoc::text("}"))
            .append(BoxDoc::line());
        let name_alias = BoxDoc::text(format!(
            "pub type {} = Message<Body, Dependencies>;",
            self.name
        ))
        .append(BoxDoc::line());

        BoxDoc::intersperse([body, dependencies, name_alias], BoxDoc::line())
    }

    fn generate_body(&'a self) -> BoxDoc<'a> {
        let fields = match self.kind {
            TypeKind::Message => {
                debug_assert!(
                    self.constructors.len() == 1,
                    "Message expected to have only one constructor"
                );
                let constructor = &self.constructors[0];
                helpers::generate_fields(&constructor.fields)
            }
            TypeKind::Enum => BoxDoc::intersperse(
                self.constructors.iter().map(
                    |Constructor {
                         name,
                         implicits: _,
                         fields,
                         result_type: _,
                     }| {
                        if fields.is_empty() {
                            BoxDoc::nil()
                        } else {
                            BoxDoc::text(format!("{} {{", name))
                                .append(helpers::generate_fields(fields).nest(NEST_UNIT))
                                .append(BoxDoc::text("}"))
                        }
                    },
                ),
                BoxDoc::text(",").append(BoxDoc::line()),
            ),
        };

        let holder = match self.kind {
            TypeKind::Message => "enum",
            TypeKind::Enum => "struct",
        };
        BoxDoc::text("#[derive(PartialEq, Eq)]")
            .append(BoxDoc::line())
            .append(BoxDoc::text(format!("pub {} {} {{", holder, self.name)))
            .append(BoxDoc::line())
            .append(fields.nest(NEST_UNIT))
            .append(BoxDoc::text("}"))
    }
}

impl<'a> Type<'a> {
    // generates inherent impl that implements constructors for main type, setters and getters
    pub fn generate_inherent_impl(&'a self) -> BoxDoc<'a> {
        let constructors = self
            .constructors
            .iter()
            .map(|constructor| self.generate_constructor(constructor));

        let setters = iter::once(BoxDoc::nil());

        let getters = iter::once(BoxDoc::nil());

        BoxDoc::intersperse(constructors.chain(setters).chain(getters), BoxDoc::line())
    }

    fn generate_constructor(&'a self, constructor: &'a Constructor) -> BoxDoc<'a> {
        // type could have field named "dependencies"
        // so in such case here we need to name this parameter differently (for example add suffix _#)
        let params = iter::once(("dependencies".to_owned(), BoxDoc::text("Dependencies"))).chain(
            constructor
                .fields
                .iter()
                .map(|(name, ty)| (name.clone(), helpers::generate_type_expression(ty))),
        );

        let body = BoxDoc::text("let body = ");

        BoxDoc::text(format!("pub fn {}(", constructor.name)).append(BoxDoc::intersperse(
            params.map(|(name, ty)| BoxDoc::text(name).append(BoxDoc::text(": ")).append(ty)),
            BoxDoc::text(", "),
        )).append(BoxDoc::text(") -> Result<Self, ConstructorError> {"))
        .append(BoxDoc::line())
        .append(body)
        .append(BoxDoc::text("}"))
    }

    fn generate_constructor_body(&'a self, constructor: &'a Constructor) -> BoxDoc<'a> {

    }

    // fn generate_setter()
}
