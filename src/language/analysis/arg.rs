use crate::language::{ast, im};
use crate::language::analysis::decl::DeclQuery;
use crate::language::analysis::error::{AnalysisError, Trisult};
use crate::language::analysis::namespace::{Nameholder};
use crate::language::im::{CalledType, CalledTypes};

pub (in super) fn query_declared_args(db: &dyn DeclQuery, decl_args: Vec<ast::DeclaredArgument>) -> Trisult<Vec<im::DeclaredArgumentId>, AnalysisError> {
    decl_args.into_iter()
        .map(|decl_arg| db.query_declared_arg(decl_arg))
        .collect::<Trisult<Vec<_>, _>>()
}

pub (in super) fn query_declared_arg(db: &dyn DeclQuery, decl_arg: ast::DeclaredArgument) -> Trisult<im::DeclaredArgumentId, AnalysisError> {
    let default_value_option = decl_arg.default_value
        .map(|call_chain| db.query_call_chain(vec![Nameholder::Root], call_chain));

    decl_arg.types.clone().into_iter()
        .map(|ident|
            db.query_namespaced_type(
                vec![Nameholder::Root],
                ident.clone(),
            ).map(|r#type| CalledType { r#type, span: ident.span.clone() })
        )
        .collect::<Trisult<Vec<CalledType>, AnalysisError>>()
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