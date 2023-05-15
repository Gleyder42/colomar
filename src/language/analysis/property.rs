use crate::language::analysis::error::{AnalysisError, QueryResult};
use crate::language::analysis::namespace::{NamespacePlaceholder, NamespaceQuery};
use crate::language::analysis::r#type::TypeQuery;
use crate::language::ast;
use crate::language::im::Property;

#[salsa::query_group(PropertyDatabase)]
pub trait PropertyQuery: NamespaceQuery {
    fn query_property(&self, property_decl: ast::PropertyDeclaration) -> QueryResult<Property, AnalysisError>;
}

fn query_property(
    db: &dyn PropertyQuery,
    property_decl: ast::PropertyDeclaration,
) -> QueryResult<Property, AnalysisError> {
    db.query_namespaced_type(NamespacePlaceholder::Root, property_decl.r#type)
        .map(|r#type| {
            Property {
                is_workshop: property_decl.is_workshop,
                name: property_decl.name,
                desc: property_decl.use_restriction,
                r#type,
            }
        }).into()
}