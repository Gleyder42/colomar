use crate::compiler::analysis::def::DefQuery;
use crate::compiler::analysis::namespace::Nameholder;
use crate::compiler::cir::{AValueChain, EventDeclarationId, Predicate, RValue, Type};
use crate::compiler::cst::{Action, Actions, CallArgument, Conditions};
use crate::compiler::error::CompilerError;
use crate::compiler::trisult::Trisult;
use crate::compiler::{cir, cst, QueryTrisult};
use crate::query_error;

use either::Either;
use smallvec::smallvec;

pub(super) fn query_rule_actions(
    db: &dyn DefQuery,
    event_decl_id: EventDeclarationId,
    actions: Actions,
) -> QueryTrisult<Vec<AValueChain>> {
    let event_type = cir::Type::Event(event_decl_id);

    actions
        .into_iter()
        .map(|action| match action {
            Action::CallChain(call_chain) => db.query_call_chain(
                smallvec![Nameholder::Root, Nameholder::Event(event_decl_id)],
                call_chain,
            ),
            Action::Property(ast_property) => db
                .query_property(Some(event_type), ast_property)
                .map(|property_decl| {
                    let span = property_decl.name.span;
                    let chain: AValueChain =
                        cir::AValue::RValue(RValue::Property(property_decl), span).into();
                    chain
                }),
        })
        .collect::<QueryTrisult<_>>()
}

pub(super) fn query_rule_cond(
    db: &dyn DefQuery,
    event_decl_id: EventDeclarationId,
    conditions: Conditions,
) -> QueryTrisult<Vec<AValueChain>> {
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
                .map(|avalue_chain: AValueChain| {
                    let avalue = avalue_chain.returning_avalue();

                    let called_type = avalue.return_called_type(db);
                    if called_type.r#type == bool_id {
                        Trisult::Ok(avalue_chain)
                    } else {
                        query_error!(CompilerError::WrongType {
                            actual: called_type,
                            expected: Either::Left(bool_id)
                        })
                    }
                })
                .collect()
        })
}

pub(super) fn query_rule_decl(db: &dyn DefQuery, rule: cst::Rule) -> QueryTrisult<cir::Rule> {
    let arguments = |event_decl_id: &EventDeclarationId| {
        let arguments_span = rule.arguments.span;

        rule.arguments
            .value
            .into_iter()
            .map(|call_argument| {
                db.query_call_chain(
                    smallvec![Nameholder::Root],
                    call_argument.clone().call_chain(),
                )
                .map(|it| match call_argument {
                    CallArgument::Named(name, _, _) => (Some(name), it),
                    CallArgument::Pos(_) => (None, it),
                })
            })
            .collect::<QueryTrisult<Vec<_>>>()
            .spanned(arguments_span)
            .and_require(db.query_event_def_by_id(*event_decl_id))
            .flat_map(|(arguments, event_def)| db.query_called_args(arguments, event_def.arguments))
    };

    db.query_namespaced_event(smallvec![Nameholder::Root], rule.event)
        .map_and_require(arguments)
        .map_and_require(|(event_decl_id, _)| {
            db.query_rule_cond(*event_decl_id, rule.conditions)
                .map_inner(|avalue| Predicate {
                    return_value: avalue,
                })
        })
        .map_and_require(|((event_decl_id, _), _)| {
            db.query_rule_actions(*event_decl_id, rule.actions)
        })
        .map(
            |(((event_decl_id, arguments), conditions), actions)| cir::Rule {
                title: rule.name.value,
                event: event_decl_id,
                arguments,
                conditions,
                actions,
            },
        )
}
