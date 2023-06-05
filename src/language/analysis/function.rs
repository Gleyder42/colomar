use crate::language::analysis::error::{AnalysisError, Trisult};
use crate::language::{ast, im};
use crate::language::analysis::decl::DeclQuery;
use crate::language::im::Type;

pub(in super) fn query_function_decl(
    db: &dyn DeclQuery,
    function: ast::FunctionDeclaration
) -> Trisult<im::FunctionDecl, AnalysisError> {
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
