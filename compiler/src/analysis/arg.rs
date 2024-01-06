use super::super::analysis::decl::DeclQuery;
use super::super::analysis::namespace::Nameholder;
use super::super::cir::{AValueChain, CalledArgs, CalledType, CalledTypes, DeclArgIds};
use super::super::error::CompilerError;

use super::super::{cir, cst, Ident, QueryTrisult, TextId};

use super::super::span::Spanned;
use crate::cir::{GenericTypeBoundMap, VirtualTypeKind};
use crate::tri;
use crate::trisult::Errors;
use either::Either;
use hashlink::{LinkedHashMap, LinkedHashSet};
use smallvec::smallvec;
use std::collections::HashSet;

pub(super) fn query_declared_args(
    db: &dyn DeclQuery,
    decl_args: cst::DeclArgs,
    generic_names: LinkedHashSet<TextId>,
) -> QueryTrisult<DeclArgIds> {
    decl_args
        .into_iter()
        .map(|decl_arg| db.query_declared_arg(decl_arg, generic_names.clone())) // TODO can we prevent cloning here?
        .collect::<QueryTrisult<_>>()
}

pub(super) fn query_called_args(
    db: &dyn DeclQuery,
    called_arg_avalues: Spanned<Vec<(Option<Ident>, AValueChain)>>,
    decl_arg_ids: DeclArgIds,
    type_hint: Option<CalledType>,
) -> QueryTrisult<CalledArgs> {
    /// Intermediate declared argument
    struct ImDeclArg {
        index: usize,
        id: cir::DeclArgId,
        is_vararg: bool,
    }

    let mut decl_args_map: LinkedHashMap<TextId, ImDeclArg> = decl_arg_ids
        .iter()
        .enumerate()
        .map(|(index, id)| {
            let decl_arg = db.lookup_intern_decl_arg(*id);
            let arg_name = decl_arg.name.value;
            (
                arg_name,
                ImDeclArg {
                    index,
                    id: *id,
                    is_vararg: decl_arg.is_vararg,
                },
            )
        })
        .collect();

    let called_span = called_arg_avalues.span;

    let vararg_id = decl_args_map
        .back()
        .filter(|(_, value)| value.is_vararg)
        .map(|(_, value)| value.id);

    let called_args: Vec<_> = called_arg_avalues.value.into_iter().enumerate().collect();

    let initial_accumulator: Option<usize> = None;
    QueryTrisult::Ok(called_args)
        .reduce(initial_accumulator, |last_index, (index, (name, arg))| {
            use super::super::Trisult::*;
            let is_index_valid = |index| last_index.is_none() || last_index.unwrap() < index;

            match name {
                // If the called arguments is named
                Some(name) => match decl_args_map.remove(&name.value) {
                    Some(ImDeclArg { index, id, .. }) => {
                        if is_index_valid(index) {
                            let called_arg = cir::CalledArg {
                                declared: id,
                                value: arg,
                            };
                            Ok((Some(index), Some(called_arg)))
                        } else {
                            let error = CompilerError::CannotMixArgs(arg.span);
                            Par((Some(index), None), vec![error])
                        }
                    }
                    None => {
                        let error = CompilerError::DuplicateNamedArg(name);
                        Par((Some(index), None), vec![error])
                    }
                },
                // If the called argument is not named
                None => {
                    if is_index_valid(index) {
                        match decl_arg_ids.get(index) {
                            Some(decl_arg_id) => {
                                let called = cir::CalledArg {
                                    declared: *decl_arg_id,
                                    value: arg,
                                };
                                Ok((Some(index), Some(called)))
                            }
                            None => match vararg_id {
                                Some(id) => {
                                    let called = cir::CalledArg {
                                        declared: id,
                                        value: arg,
                                    };
                                    Ok((Some(index), Some(called)))
                                }
                                None => {
                                    let error = CompilerError::ArgOutOfRange {
                                        index,
                                        span: arg.span,
                                        max_index: decl_arg_ids.len() - 1,
                                    };
                                    Par((Some(index), None), vec![error])
                                }
                            },
                        }
                    } else {
                        let error = CompilerError::CannotMixArgs(arg.span);
                        Par((Some(index), None), vec![error])
                    }
                }
            }
        })
        .flat_map(|called_args| {
            called_args
                .into_iter()
                .map(|called_arg| {
                    let decl_arg: cir::DeclArg = db.lookup_intern_decl_arg(called_arg.declared);
                    let called_type = called_arg.value.returning_avalue().return_called_type(db);

                    let bound_map = type_hint
                        .clone()
                        .map(|it| db.query_generic_type_bound_map(it))
                        .unwrap_or(QueryTrisult::Ok(GenericTypeBoundMap::new()));
                    let mut errors = Errors::new();
                    let bound_map = tri!(bound_map, errors);

                    let virtual_type = match &called_type.r#type {
                        VirtualTypeKind::Type(it) => it,
                        VirtualTypeKind::Generic(_) => {
                            let error = CompilerError::NotImplemented(
                                "Type must not be generic".into(),
                                called_type.span,
                            );
                            return errors.fail(error);
                        }
                    };

                    if decl_arg.types.contains_type(virtual_type, &bound_map) {
                        errors.value(called_arg)
                    } else {
                        let error = CompilerError::WrongType {
                            expected: Either::Right(decl_arg.types),
                            actual: called_type,
                        };
                        errors.par(called_arg, error)
                    }
                })
                .collect::<QueryTrisult<Vec<cir::CalledArg>>>()
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
                    let decl_arg: cir::DeclArg = db.lookup_intern_decl_arg(*missing_decl_arg_id);
                    if decl_arg.default_value.is_none() {
                        Some(CompilerError::MissingArg {
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
        .fail_when_par()
}

pub(super) fn query_declared_arg(
    db: &dyn DeclQuery,
    decl_arg: cst::DeclArg,
    generic_names: LinkedHashSet<TextId>,
) -> QueryTrisult<cir::DeclArgId> {
    let default_value_option = decl_arg
        .default_value
        .map(|call_chain| db.query_call_chain(smallvec![Nameholder::Root], call_chain, None));

    decl_arg
        .types
        .clone()
        .into_iter()
        .map(|r#type| {
            if generic_names.contains(&r#type.ident.value) {
                let called_type = CalledType {
                    r#type: VirtualTypeKind::Generic(r#type.ident),
                    span: r#type.ident.span,
                };
                QueryTrisult::Ok(called_type)
            } else {
                let span = r#type.ident.span;
                db.query_namespaced_type2(smallvec![Nameholder::Root], r#type)
                    .map(|r#type| CalledType {
                        r#type: r#type.into(),
                        span,
                    })
            }
        })
        .collect::<QueryTrisult<Vec<CalledType>>>()
        .and_maybe(default_value_option)
        .map(|(types, default_value)| cir::DeclArg {
            is_vararg: decl_arg.is_vararg.is_some(),
            position: decl_arg.position,
            name: decl_arg.name,
            types: CalledTypes {
                types: LinkedHashSet::from_iter(types),
                span: decl_arg.types.span,
            },
            default_value,
        })
        .intern(db)
}
