use crate::language::analysis::error::{AnalysisError, QueryResult};
use crate::language::analysis::namespace::{Nameholder, NamespaceQuery};
use crate::language::ast;
use crate::language::im::PropertyDecl;

#[salsa::query_group(PropertyDatabase)]
pub trait PropertyDeclQuery: NamespaceQuery {

    fn query_property(&self, property_decl: ast::PropertyDeclaration) -> QueryResult<PropertyDecl, AnalysisError>;
}

fn query_property(
    db: &dyn PropertyDeclQuery,
    property_decl: ast::PropertyDeclaration,
) -> QueryResult<PropertyDecl, AnalysisError> {
    db.query_namespaced_type(vec![Nameholder::Root], property_decl.r#type)
        .map(|r#type| {
            PropertyDecl {
                is_workshop: property_decl.is_workshop,
                name: property_decl.name,
                desc: property_decl.use_restriction,
                r#type,
            }
        })
}