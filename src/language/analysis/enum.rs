use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use crate::language::{ast, HalfHashed, Ident, im, Span};
use crate::language::analysis::error::{AnalysisError, QueryResult};
use crate::language::analysis::interner::{Interner, IntoInternId};
use crate::language::im::{EnumConstant, EnumConstantId};

#[salsa::query_group(EnumDatabase)]
pub trait EnumQuery: EnumDeclQuery + Interner {
    fn query_enum(&self, r#enum: ast::Enum) -> QueryResult<im::Enum, AnalysisError>;

    fn find_enum_def(&self, enum_decl: im::EnumDeclarationId) -> im::Enum;
}

fn find_enum_def(db: &dyn EnumQuery, enum_decl: im::EnumDeclarationId) -> im::Enum {
    todo!()
}

fn no_duplicates(constants: Vec<EnumConstant>) -> QueryResult<Vec<EnumConstant>, AnalysisError> {
    let mut constants_map = HashMap::new();
    let mut duplicates = Vec::new();


    for enum_constant in constants {
        let value = enum_constant.name.value.clone();

        let result = constants_map.try_insert(value, enum_constant.clone());
        if let Err(error) = result {
            let first = Ident {
                value: Rc::clone(error.entry.key()),
                span: error.entry.get().name.span.clone(),
            };

            duplicates.push(AnalysisError::DuplicateIdent { first, second: enum_constant.name.clone() })
        }
    }
    println!("{:#?}", duplicates);

    let unique_constants = constants_map.into_values().collect::<Vec<EnumConstant>>();

    QueryResult::from((unique_constants, duplicates))
}

fn query_enum(db: &dyn EnumQuery, r#enum: ast::Enum) -> QueryResult<im::Enum, AnalysisError> {
    let declaration = db.query_enum_decl(r#enum.declaration);

    let constants: Vec<_> = r#enum.definition.constants.into_iter()
        .map(|name| im::EnumConstant { name, r#enum: declaration })
        .collect();

    no_duplicates(constants)
        .map_inner(|it| db.intern_enum_constant(it))
        .map(|constants| im::Enum {
            declaration,
            definition: im::EnumDefinition { constants },
            span: r#enum.span,
        })
}

#[salsa::query_group(EnumDeclarationDatabase)]
pub trait EnumDeclQuery: Interner {
    fn query_enum_decl(&self, r#enum: ast::EnumDeclaration) -> im::EnumDeclarationId;
}

fn query_enum_decl(db: &dyn EnumDeclQuery, r#enum: ast::EnumDeclaration) -> im::EnumDeclarationId {
    im::EnumDeclaration {
        name: r#enum.name,
        is_workshop: r#enum.is_workshop,
    }.intern(db)
}
