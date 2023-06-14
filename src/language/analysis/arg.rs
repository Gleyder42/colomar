use crate::language::analysis::decl::DeclQuery;
use crate::language::analysis::interner::IntoInternId;
use crate::language::analysis::namespace::Nameholder;
use crate::language::analysis::{AnalysisError, QueryTrisult};
use crate::language::im::{
    AValue, CalledArgument, CalledArguments, CalledType, CalledTypes, DeclaredArgument,
    DeclaredArgumentIds,
};
use crate::language::{ast, im};
use either::Either;
use smallvec::smallvec;

pub(super) fn query_declared_args(
    db: &dyn DeclQuery,
    decl_args: ast::DeclaredArguments,
) -> QueryTrisult<DeclaredArgumentIds> {
    decl_args
        .into_iter()
        .map(|decl_arg| db.query_declared_arg(decl_arg))
        .collect::<QueryTrisult<_>>()
}

pub(super) fn query_called_args(
    db: &dyn DeclQuery,
    called_arg_avalues: Vec<AValue>,
    decl_arg_ids: DeclaredArgumentIds,
) -> QueryTrisult<CalledArguments> {
    decl_arg_ids
        .into_iter()
        .map::<DeclaredArgument, _>(|decl_arg_id| db.lookup_intern_decl_arg(decl_arg_id))
        .zip(called_arg_avalues)
        .map(|(decl_arg, avalue)| {
            let called_type = avalue.return_called_type(db);
            let valid_type = decl_arg.types.contains_type(&called_type.r#type);

            let called_argument = CalledArgument {
                value: avalue,
                declared: decl_arg.clone().intern(db),
            };

            if valid_type {
                QueryTrisult::Ok(called_argument)
            } else {
                let error = AnalysisError::WrongType {
                    actual: called_type,
                    expected: Either::Right(decl_arg.types),
                };
                QueryTrisult::Par(called_argument, vec![error])
            }
        })
        .collect::<QueryTrisult<_>>()
}

pub(super) fn query_declared_arg(
    db: &dyn DeclQuery,
    decl_arg: ast::DeclaredArgument,
) -> QueryTrisult<im::DeclaredArgumentId> {
    let default_value_option = decl_arg
        .default_value
        .map(|call_chain| db.query_call_chain(smallvec![Nameholder::Root], call_chain));

    decl_arg
        .types
        .clone()
        .into_iter()
        .map(|ident| {
            db.query_namespaced_type(smallvec![Nameholder::Root], ident.clone())
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
