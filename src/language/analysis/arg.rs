use crate::language::{ast, im};
use crate::language::analysis::call::CallQuery;
use crate::language::analysis::error::{AnalysisError, QueryResult};
use crate::language::analysis::namespace::{NamespacePlaceholder, NamespaceQuery};
use crate::language::im::{CalledType, CalledTypes};

#[salsa::query_group(ArgDatabase)]
pub trait ArgQuery: NamespaceQuery + CallQuery {
    fn query_declared_arg(&self, decl_arg: ast::DeclaredArgument) -> QueryResult<im::DeclaredArgumentId, AnalysisError>;
}

fn query_declared_arg(db: &dyn ArgQuery, decl_arg: ast::DeclaredArgument) -> QueryResult<im::DeclaredArgumentId, AnalysisError> {
    let default_value_option = decl_arg.default_value
        .map(|call_chain| db.query_call_chain(NamespacePlaceholder::Root, call_chain));

    decl_arg.types.clone().into_iter()
        .map(|ident|
            db.query_namespaced_type(
                NamespacePlaceholder::Root,
                ident.clone(),
            ).map(|r#type| CalledType { r#type, span: ident.span.clone() })
        )
        .collect::<QueryResult<Vec<CalledType>, AnalysisError>>()
        .and_maybe(default_value_option)
        .map(|(types, default_value)| {
            im::DeclaredArgument {
                name: decl_arg.name,
                types: CalledTypes { types, span: decl_arg.types.span, },
                default_value,
            }
        })
        .intern(db)
}