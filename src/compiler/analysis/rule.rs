use crate::compiler::analysis::def::DefQuery;
use crate::compiler::analysis::namespace::Nameholder;
use crate::compiler::cir::{AValueChain, EventDeclarationId, Predicate, RValue};
use crate::compiler::cst::{Action, Actions, CallArgument, Conditions};
use crate::compiler::{cir, cst, QueryTrisult};

use crate::compiler::analysis::interner::IntoInternId;
use smallvec::smallvec;

pub(super) fn query_rule_actions(
    db: &dyn DefQuery,
    event_decl_id: EventDeclarationId,
    actions: Actions,
) -> QueryTrisult<cir::Actions> {
    let event_type = cir::Type::Event(event_decl_id);

    actions
        .into_iter()
        .map(|action| match action {
            Action::CallChain(call_chain) => db
                .query_call_chain(
                    smallvec![Nameholder::Root, Nameholder::Event(event_decl_id)],
                    call_chain,
                )
                .map(cir::Action::AvalueChain),
            Action::Assignment(left, right, assign_mod) => {
                let nameholders = smallvec![Nameholder::Root, Nameholder::Event(event_decl_id)];

                let left = db.query_call_chain(nameholders.clone(), left);
                let right = db.query_call_chain(nameholders, right);
                left.and_require(right)
                    .map(|(left, right)| cir::Action::Assigment(left, right, assign_mod))
            }
            Action::Property(ast_property) => db
                .query_property(Some(event_type), ast_property)
                .map(|property_decl| {
                    let span = property_decl.name.span;
                    let chain: AValueChain =
                        cir::AValue::RValue(RValue::Property(property_decl.intern(db)), span)
                            .into();
                    cir::Action::AvalueChain(chain)
                }),
        })
        .collect::<QueryTrisult<_>>()
}

pub(super) fn query_rule_cond(
    db: &dyn DefQuery,
    event_decl_id: EventDeclarationId,
    conditions: Conditions,
) -> QueryTrisult<Vec<cir::Expr>> {
    conditions
        .into_iter()
        .map(|condition| {
            db.query_expr(
                smallvec![Nameholder::Root, Nameholder::Event(event_decl_id)],
                true,
                condition,
            )
        })
        .collect::<QueryTrisult<Vec<cir::Expr>>>()
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
                .map_inner(Predicate)
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
