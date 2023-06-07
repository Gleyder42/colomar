use smallvec::smallvec;
use crate::language::analysis::decl::DeclQuery;
use crate::language::analysis::namespace::Nameholder;
use crate::language::analysis::QueryTrisult;
use crate::language::ast;
use crate::language::im::PropertyDecl;

pub(super) fn query_property(
    db: &dyn DeclQuery,
    property_decl: ast::PropertyDeclaration,
) -> QueryTrisult<PropertyDecl> {
    db.query_namespaced_type(smallvec![Nameholder::Root], property_decl.r#type)
        .map(|r#type| PropertyDecl {
            is_workshop: property_decl.is_workshop,
            name: property_decl.name,
            desc: property_decl.use_restriction,
            r#type,
        })
}
