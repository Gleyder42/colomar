use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use crate::language::{ast, HalfHashed, Ident, im, Span};
use crate::language::analysis::error::{AnalysisError, QueryResult};
use crate::language::analysis::file::RootFileQuery;
use crate::language::analysis::interner::{Interner, IntoInternId};
use crate::language::ast::{EnumDeclaration, EnumDefinition};
use crate::language::im::{EnumConstant, EnumConstantId, EnumDeclarationId};

#[salsa::query_group(EnumDatabase)]
pub trait EnumQuery: EnumDeclQuery + Interner + RootFileQuery {
    fn query_enum(&self, r#enum: ast::Enum) -> QueryResult<im::Enum, AnalysisError>;

    fn query_enum_ast_map(&self) -> HashMap<EnumDeclarationId, ast::Enum>;

    fn query_enum_ast(&self, enum_decl: EnumDeclarationId) -> Result<ast::Enum, AnalysisError>;

    fn query_enum_def(&self, enum_decl: EnumDeclarationId) -> QueryResult<im::Enum, AnalysisError>;
}

fn query_enum_ast(db: &dyn EnumQuery, enum_decl_id: EnumDeclarationId) -> Result<ast::Enum, AnalysisError> {
    db.query_enum_ast_map().get(&enum_decl_id)
        .map(|it| it.clone())
        .ok_or_else(|| {
            let enum_decl: im::EnumDeclaration = db.lookup_intern_enum_decl(enum_decl_id);
            AnalysisError::CannotFindIdent(enum_decl.name)
        })
}

fn query_enum_ast_map(db: &dyn EnumQuery) -> HashMap<EnumDeclarationId, ast::Enum> {
    db.input_content().into_iter()
        .filter_map(|it| {
            if let ast::Root::Enum(r#enum) = it {
                Some((db.query_enum_decl(r#enum.declaration.clone()), r#enum))
            } else {
                None
            }
        })
        .collect::<HashMap<_, _>>()
}

fn query_enum_def(db: &dyn EnumQuery, enum_decl_id: EnumDeclarationId) -> QueryResult<im::Enum, AnalysisError> {
    db.query_enum_ast(enum_decl_id)
        .map(|enum_ast| db.query_enum(enum_ast))
        .into()
}

fn no_duplicates(constants: Vec<EnumConstant>) -> QueryResult<Vec<EnumConstant>, AnalysisError> {
    let mut constants_map = HashMap::new();
    let mut duplicates = Vec::new();

    for enum_constant in constants {
        let constant_name = enum_constant.name.value.clone();

        let result = constants_map.try_insert(constant_name, enum_constant.clone());
        // Returns Err if the constants is already present in the constants_map
        if let Err(error) = result {
            let first = Ident {
                value: Rc::clone(error.entry.key()),
                span: error.entry.get().name.span.clone(),
            };

            duplicates.push(AnalysisError::DuplicateIdent { first, second: enum_constant.name.clone() })
        }
    }
    let unique_constants = constants_map.into_values().collect::<Vec<EnumConstant>>();

    QueryResult::from((unique_constants, duplicates))
}

fn query_enum(db: &dyn EnumQuery, r#enum: ast::Enum) -> QueryResult<im::Enum, AnalysisError> {
    let declaration = db.query_enum_decl(r#enum.declaration);

    let constants: Vec<_> = r#enum.definition.constants.into_iter()
        .map(|name| EnumConstant { name, r#enum: declaration })
        .collect();

    no_duplicates(constants)
        .intern_inner(db)
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
