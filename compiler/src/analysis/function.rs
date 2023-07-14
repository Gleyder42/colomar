use crate::analysis::decl::DeclQuery;
use crate::cir::Type;
use crate::QueryTrisult;
use crate::{cir, cst};

pub(super) fn query_function_decl(
    db: &dyn DeclQuery,
    instance: Option<Type>,
    function: cst::FunctionDeclaration,
) -> QueryTrisult<cir::FunctionDecl> {
    db.query_declared_args(function.arguments.value)
        .map(|decl_args| {
            cir::FunctionDecl {
                instance,
                name: function.name,
                is_native: function.is_native,
                return_type: Type::Unit, // TODO Add return types
                arguments: decl_args,
            }
        })
}
