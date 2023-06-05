use crate::language::analysis::decl::DeclQuery;
use crate::language::analysis::namespace::Nameholder;
use crate::language::analysis::QueryTrisult;
use crate::language::im::{CalledType, CalledTypes};
use crate::language::{ast, im};

pub(super) fn query_declared_args(
    db: &dyn DeclQuery,
    decl_args: Vec<ast::DeclaredArgument>,
) -> QueryTrisult<Vec<im::DeclaredArgumentId>> {
    decl_args
        .into_iter()
        .map(|decl_arg| db.query_declared_arg(decl_arg))
        .collect::<QueryTrisult<Vec<_>>>()
}

pub(super) fn query_declared_arg(
    db: &dyn DeclQuery,
    decl_arg: ast::DeclaredArgument,
) -> QueryTrisult<im::DeclaredArgumentId> {
    let default_value_option = decl_arg
        .default_value
        .map(|call_chain| db.query_call_chain(vec![Nameholder::Root], call_chain));

    decl_arg
        .types
        .clone()
        .into_iter()
        .map(|ident| {
            db.query_namespaced_type(vec![Nameholder::Root], ident.clone())
                .map(|r#type| CalledType {
                    r#type,
                    span: ident.span.clone(),
                })
        })
        .collect::<QueryTrisult<Vec<CalledType>>>()
        .and_maybe(default_value_option)
        .map(|(types, default_value)| im::DeclaredArgument {
            name: decl_arg.name,
            types: CalledTypes {
                types,
                span: decl_arg.types.span,
            },
            default_value,
        })
        .intern(db)
}
