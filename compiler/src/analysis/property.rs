use super::super::analysis::decl::DeclQuery;
use super::super::analysis::namespace::Nameholder;
use super::super::cir::{PropertyDecl, TypeDesc};
use super::super::cst;
use super::super::QueryTrisult;

use smallvec::smallvec;

pub(super) fn query_property(
    db: &dyn DeclQuery,
    instance: Option<TypeDesc>,
    property_decl: cst::PropertyDecl,
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
