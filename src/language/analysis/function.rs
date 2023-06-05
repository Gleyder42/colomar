use crate::language::analysis::decl::DeclQuery;
use crate::language::analysis::QueryTrisult;
use crate::language::im::Type;
use crate::language::{ast, im};

pub(super) fn query_function_decl(
    db: &dyn DeclQuery,
    function: ast::FunctionDeclaration,
) -> QueryTrisult<im::FunctionDecl> {
    db.query_declared_args(function.arguments.value)
        .map(|decl_args| {
            im::FunctionDecl {
                name: function.name,
                is_workshop: function.is_workshop,
                return_type: Type::Unit, // TODO Add return types
                arguments: decl_args,
            }
        })
}
