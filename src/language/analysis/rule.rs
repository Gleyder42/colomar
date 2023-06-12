use crate::language::analysis::def::DefQuery;
use crate::language::analysis::interner::IntoInternId;
use crate::language::analysis::namespace::Nameholder;
use crate::language::analysis::{AnalysisError, QueryTrisult};
use crate::language::ast::{Action, Actions, Conditions};
use crate::language::error::Trisult;
use crate::language::im::{
    CalledArgument, DeclaredArgument, EventDeclarationId, Predicate, RValue, Type,
};
use crate::language::{ast, im};
use crate::query_error;
use either::Either;
use smallvec::smallvec;

pub(super) fn query_rule_actions(
    db: &dyn DefQuery,
    event_decl_id: EventDeclarationId,
    actions: Actions,
) -> QueryTrisult<Vec<im::AValue>> {
    actions
        .into_iter()
        .map(|action| match action {
            Action::CallChain(call_chain) => db.query_call_chain(
                smallvec![Nameholder::Root, Nameholder::Event(event_decl_id)],
                call_chain,
            ),
            Action::Property(ast_property) => db.query_property(ast_property).map(|it| {
                let span = it.name.span.clone();
                im::AValue::RValue(RValue::Property(it), span)
            }),
        })
        .collect::<QueryTrisult<_>>()
}

pub(super) fn query_rule_cond(
    db: &dyn DefQuery,
    event_decl_id: EventDeclarationId,
    conditions: Conditions,
) -> QueryTrisult<Vec<im::AValue>> {
    conditions
        .into_iter()
        .map(|condition| {
            db.query_call_chain(
                smallvec![Nameholder::Root, Nameholder::Event(event_decl_id)],
                condition,
            )
        })
        .collect::<QueryTrisult<Vec<_>>>()
        .and_require(db.query_bool_type().map(Type::Struct))
        .flat_map(|(avalues, bool_id)| {
            avalues
                .into_iter()
                .map(|avalue| {
                    let called_type = avalue.return_called_type(db);
                    if called_type.r#type == bool_id {
                        Trisult::Ok(avalue)
                    } else {
                        query_error!(AnalysisError::WrongType {
                            actual: called_type,
                            expected: Either::Left(bool_id.clone())
                        })
                    }
                })
                .collect()
        })
}

pub(super) fn query_rule_decl(db: &dyn DefQuery, rule: ast::Rule) -> QueryTrisult<im::Rule> {
    let arguments = |event_decl_id: EventDeclarationId| {
        rule.arguments
            .into_iter()
            .map(|call_chain| db.query_call_chain(smallvec![Nameholder::Root], call_chain))
            .collect::<QueryTrisult<Vec<_>>>()
            .and_require(db.query_event_def_by_id(event_decl_id))
            .flat_map(|(arguments, event_def)| {
                event_def
                    .arguments
                    .into_iter()
                    .map::<DeclaredArgument, _>(|decl_arg_id| {
                        db.lookup_intern_decl_arg(decl_arg_id)
                    })
                    .zip(arguments.into_iter())
                    .map(|(decl_arg, avalue)| {
                        let called_type = avalue.return_called_type(db);
                        let valid_type = decl_arg.types.contains_type(&called_type.r#type);

                        let called_argument = CalledArgument {
                            value: avalue,
                            declared: decl_arg.clone().intern(db),
                        };

                        if valid_type {
                            Trisult::Ok(called_argument)
                        } else {
                            let error = AnalysisError::WrongType {
                                actual: called_type,
                                expected: Either::Right(decl_arg.types),
                            };
                            Trisult::Par(called_argument, vec![error])
                        }
                    })
                    .collect::<QueryTrisult<_>>()
            })
    };

    db.query_namespaced_event(smallvec![Nameholder::Root], rule.event)
        .map_and_require(arguments)
        .map_and_require(|(event_decl_id, _)| {
            db.query_rule_cond(event_decl_id, rule.conditions)
                .map_inner(|avalue| Predicate {
                    return_value: avalue,
                })
        })
        .map_and_require(|((event_decl_id, _), _)| {
            db.query_rule_actions(event_decl_id, rule.actions)
        })
        .map(
            |(((event_decl_id, arguments), conditions), actions)| im::Rule {
                title: rule.name.value,
                event: event_decl_id,
                arguments,
                conditions,
                actions,
            },
        )
}
