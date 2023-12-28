use super::super::analysis::decl::DeclQuery;
use super::super::cir::Type;
use super::super::QueryTrisult;
use super::super::{cir, cst};

pub(super) fn query_function_decl(
    db: &dyn DeclQuery,
    instance: Option<Type>,
    function: cst::FunctionDecl,
) -> QueryTrisult<cir::FunctionDecl> {
    db.query_declared_args(function.args.value)
        .map(|decl_args| {
            cir::FunctionDecl {
                instance,
                name: function.name,
                is_native: function.is_native,
                return_type: Type::Unit.into(), // TODO Add return types
                args: decl_args,
            }
        })
}
