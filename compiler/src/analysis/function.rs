use super::super::analysis::decl::DeclQuery;
use super::super::cir::Type;
use super::super::QueryTrisult;
use super::super::{cir, cst};
use crate::analysis::namespace::Nameholder;
use crate::cir::VirtualType;
use crate::trisult::Errors;
use crate::{tri, TextId};
use hashlink::LinkedHashSet;
use smallvec::smallvec;

pub(super) fn query_function_decl(
    db: &dyn DeclQuery,
    instance: Option<Type>,
    function: cst::FunctionDecl,
    generic_names: LinkedHashSet<TextId>,
) -> QueryTrisult<cir::FunctionDecl> {
    let mut errors = Errors::new();

    let decl_args = tri!(
        db.query_declared_args(function.args.value, generic_names),
        errors
    );

    let return_type = match function.return_type {
        Some(return_type) => {
            let root_namespace = smallvec![Nameholder::Root];
            let return_type = tri!(
                db.query_namespaced_type2(root_namespace, return_type),
                errors
            );
            return_type
        }
        None => VirtualType::from(Type::Unit),
    };

    let function = cir::FunctionDecl {
        instance,
        name: function.name,
        is_native: function.is_native,
        return_type,
        args: decl_args,
    };
    errors.value(function)
}
