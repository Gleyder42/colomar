use crate::compiler::analysis::decl::DeclQuery;
use crate::compiler::analysis::namespace::Nameholder;
use crate::compiler::cir::{
    AValueChain, CalledArguments, CalledType, CalledTypes, DeclaredArgumentIds,
};
use crate::compiler::error::CompilerError;

use crate::compiler::{cir, cst, Ident, QueryTrisult, Spanned, Text};

use either::Either;
use smallvec::smallvec;
use std::collections::{HashMap, HashSet};

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
    called_arg_avalues: Spanned<Vec<(Option<Ident>, AValueChain)>>,
    decl_arg_ids: DeclaredArgumentIds,
) -> QueryTrisult<CalledArguments> {
    let mut decl_args_map: HashMap<Text, (usize, cir::DeclaredArgumentId)> = decl_arg_ids
        .iter()
        .enumerate()
        .map(|(index, id)| {
            let decl_arg = db.lookup_intern_decl_arg(*id);
            let arg_name = decl_arg.name.value;
            (arg_name, (index, *id))
        })
        .collect();

    let called_span = called_arg_avalues.span;

    let called_args: Vec<_> = called_arg_avalues.value.into_iter().enumerate().collect();

    let initial_accumulator: (Option<usize>, Vec<cir::CalledArgument>) = (None, Vec::new());
    QueryTrisult::Ok(called_args)
        .fold_flat_map(
            initial_accumulator,
            |(_, args)| args,
            |(last_index, mut decl_args), (index, (name, arg))| {
                use crate::compiler::Trisult::*;
                let is_index_valid = |index| last_index.is_none() || last_index.unwrap() < index;

                match name {
                    // If the called arguments is named
                    Some(name) => match decl_args_map.remove(&name.value) {
                        Some((index, decl_arg_id)) => {
                            if is_index_valid(index) {
                                let called_arg = cir::CalledArgument {
                                    declared: decl_arg_id,
                                    value: arg,
                                };
                                decl_args.push(called_arg);
                                Ok((Some(index), decl_args))
                            } else {
                                let error = CompilerError::CannotMixArguments(arg.span);
                                Par((Some(index), decl_args), vec![error])
                            }
                        }
                        None => {
                            let error = CompilerError::DuplicateNamedArgument(name);
                            Par((Some(index), decl_args), vec![error])
                        }
                    },
                    // If the called argument is not named
                    None => {
                        if is_index_valid(index) {
                            match decl_arg_ids.get(index) {
                                Some(decl_arg_id) => {
                                    let called = cir::CalledArgument {
                                        declared: *decl_arg_id,
                                        value: arg,
                                    };
                                    decl_args.push(called);
                                    Ok((Some(index), decl_args))
                                }
                                None => {
                                    let error = CompilerError::ArgumentOutOfRange(index, arg.span);
                                    Par((Some(index), decl_args), vec![error])
                                }
                            }
                        } else {
                            let error = CompilerError::CannotMixArguments(arg.span);
                            Par((Some(index), decl_args), vec![error])
                        }
                    }
                }
            },
        )
        .flat_map(|called_args| {
            called_args
                .into_iter()
                .map(|called_arg| {
                    let decl_arg: cir::DeclaredArgument =
                        db.lookup_intern_decl_arg(called_arg.declared);
                    let called_type = called_arg.value.returning_avalue().return_called_type(db);

                    if decl_arg.types.contains_type(&called_type.r#type) {
                        QueryTrisult::Ok(called_arg)
                    } else {
                        QueryTrisult::Par(
                            called_arg,
                            vec![CompilerError::WrongType {
                                expected: Either::Right(decl_arg.types),
                                actual: called_type,
                            }],
                        )
                    }
                })
                .collect::<QueryTrisult<Vec<cir::CalledArgument>>>()
        })
        .validate(|called_args| {
            let supplied_decl_args: HashSet<_> = called_args
                .iter()
                .map(|called_arg| called_arg.declared)
                .collect();
            let all_decl_args: HashSet<_> = decl_arg_ids.iter().cloned().collect();

            let errors: Vec<_> = all_decl_args
                .difference(&supplied_decl_args)
                .filter_map(|missing_decl_arg_id| {
                    let decl_arg: cir::DeclaredArgument =
                        db.lookup_intern_decl_arg(*missing_decl_arg_id);
                    if decl_arg.default_value.is_none() {
                        Some(CompilerError::MissingArgument {
                            missing_arg: *missing_decl_arg_id,
                            call_site: called_span,
                        })
                    } else {
                        None
                    }
                })
                .collect();
            errors
        })
        .map(|called_args| called_args.into())
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
                    span: ident.span,
                })
        })
        .collect::<QueryTrisult<Vec<CalledType>>>()
        .and_maybe(default_value_option)
        .map(|(types, default_value)| cir::DeclaredArgument {
            position: decl_arg.position,
            name: decl_arg.name,
            types: CalledTypes {
                types,
                span: decl_arg.types.span,
            },
            default_value,
        })
        .intern(db)
}
