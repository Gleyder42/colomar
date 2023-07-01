use crate::compiler::analysis::decl::DeclQuery;
use crate::compiler::analysis::interner::IntoInternId;
use crate::compiler::analysis::namespace::Nameholder;
use crate::compiler::cir::{
    AValue, AValueChain, CalledArgument, CalledArguments, CalledType, CalledTypes,
    DeclaredArgumentIds,
};
use crate::compiler::error::CompilerError;
use crate::compiler::{cir, cst, QueryTrisult};
use either::Either;
use smallvec::smallvec;

pub(super) fn query_declared_args(
    db: &dyn DeclQuery,
    decl_args: cst::DeclaredArguments,
) -> QueryTrisult<DeclaredArgumentIds> {
    decl_args
        .into_iter()
        .map(|decl_arg| db.query_declared_arg(decl_arg))
        .collect::<QueryTrisult<_>>()
}

pub(super) fn query_called_args(
    db: &dyn DeclQuery,
    called_arg_avalues: Vec<AValueChain>,
    decl_arg_ids: DeclaredArgumentIds,
) -> QueryTrisult<CalledArguments> {
    decl_arg_ids
        .into_iter()
        .map::<cir::DeclaredArgument, _>(|decl_arg_id| db.lookup_intern_decl_arg(decl_arg_id))
        .zip(called_arg_avalues)
        .map(|(decl_arg, avalue_chain)| {
            let called_type = avalue_chain.returning_avalue().return_called_type(db);
            let valid_type = decl_arg.types.contains_type(&called_type.r#type);

            let called_argument = CalledArgument {
                value: avalue_chain,
                declared: decl_arg.clone().intern(db),
            };

            if valid_type {
                QueryTrisult::Ok(called_argument)
            } else {
                let error = CompilerError::WrongType {
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
    decl_arg: cst::DeclaredArgument,
) -> QueryTrisult<cir::DeclaredArgumentId> {
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
        .map(|(types, default_value)| cir::DeclaredArgument {
            name: decl_arg.name,
            types: CalledTypes {
                types,
                span: decl_arg.types.span,
            },
            default_value,
        })
        .intern(db)
}
