use crate::language::analysis::error::{AnalysisError, QueryResult};
use crate::language::{ast, im};
use crate::language::analysis::arg::ArgQuery;
use crate::language::im::Type;

#[salsa::query_group(FunctionDatabase)]
pub trait FunctionDeclQuery: ArgQuery {

    fn query_function_decl(&self, function: ast::FunctionDeclaration) -> QueryResult<im::FunctionDecl, AnalysisError>;
}

fn query_function_decl(
    db: &dyn FunctionDeclQuery,
    function: ast::FunctionDeclaration
) -> QueryResult<im::FunctionDecl, AnalysisError> {
    db.query_declared_args(function.arguments.value)
        .map(|decl_args| {
            im::FunctionDecl {
                name: function.name,
                is_workshop: function.is_workshop,
                return_type: Type::Unit, // TODO Add return types
                arguments: decl_args
            }
        })
}
