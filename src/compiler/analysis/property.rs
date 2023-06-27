use crate::compiler::analysis::decl::DeclQuery;
use crate::compiler::analysis::namespace::Nameholder;
use crate::compiler::analysis::QueryTrisult;
use crate::compiler::cir::{PropertyDecl, Type};
use crate::compiler::cst;

use smallvec::smallvec;

pub(super) fn query_property(
    db: &dyn DeclQuery,
    instance: Option<Type>,
    property_decl: cst::PropertyDeclaration,
) -> QueryTrisult<PropertyDecl> {
    db.query_namespaced_type(smallvec![Nameholder::Root], property_decl.r#type)
        .map(|r#type| PropertyDecl {
            instance,
            is_native: property_decl.is_native,
            name: property_decl.name,
            desc: property_decl.use_restriction,
            r#type,
        })
}
