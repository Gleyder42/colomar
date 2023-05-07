use crate::language::{ast, im};
use crate::language::analysis::error::{AnalysisError, QueryResult};
use crate::language::analysis::interner::IntoInternId;
use crate::language::analysis::r#type::TypeQuery;
use crate::language::im::{CalledType, CalledTypes};

#[salsa::query_group(ArgDatabase)]
pub trait ArgQuery: TypeQuery {

    fn query_declared_arg(&self, decl_arg: ast::DeclaredArgument) -> QueryResult<im::DeclaredArgumentId, AnalysisError>;
}

fn query_declared_arg(db: &dyn ArgQuery, decl_arg: ast::DeclaredArgument) -> QueryResult<im::DeclaredArgumentId, AnalysisError> {
    decl_arg.types.clone().into_iter()
        .map(|ident| db.query_type(ident.clone()).map(|r#type| CalledType { r#type, span: ident.span.clone() }))
        .collect::<QueryResult<Vec<CalledType>, AnalysisError>>()
        .map(|types| im::DeclaredArgument {
            name: decl_arg.name,
            types: CalledTypes {
                types,
                span: decl_arg.types.span,
            },
            default_value: None, // TODO Implement when call chains can be resolved
        })
        .intern(db)
}