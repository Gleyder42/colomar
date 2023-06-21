use crate::language::analysis::def::DefQuery;
use crate::language::analysis::namespace::Nameholder;
use crate::language::analysis::{AnalysisError, QueryTrisult};
use crate::language::ast::{Action, Actions, Conditions};
use crate::language::error::Trisult;
use crate::language::im::{AValueChain, EventDeclarationId, Predicate, RValue, Type};
use crate::language::{ast, im};
use crate::query_error;
use either::Either;
use smallvec::smallvec;

pub(super) fn query_rule_actions(
    db: &dyn DefQuery,
    event_decl_id: EventDeclarationId,
    actions: Actions,
) -> QueryTrisult<Vec<AValueChain>> {
    let event_type = im::Type::Event(event_decl_id);

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
                    let span = property_decl.name.span.clone();
                    let chain: AValueChain =
                        im::AValue::RValue(RValue::Property(property_decl), span).into();
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
                db.query_called_args_by_chain(arguments, event_def.arguments)
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
