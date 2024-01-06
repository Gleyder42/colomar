use super::super::analysis::def::DefQuery;
use super::super::analysis::namespace::Nameholder;
use super::super::cir::{AValueChain, EventDeclId, Predicate, RValue};
use super::super::cst::{Action, Actions, CallArg, Conditions};
use super::super::{cir, cst, QueryTrisult};

use super::super::analysis::interner::IntoInternId;
use smallvec::smallvec;

pub(super) fn query_rule_actions(
    db: &dyn DefQuery,
    event_decl_id: EventDeclId,
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
                    None,
                )
                .map(cir::Action::AValueChain),
            Action::Assignment(left, right, assign_mod) => {
                let nameholders = smallvec![Nameholder::Root, Nameholder::Event(event_decl_id)];

                let left = db.query_call_chain(nameholders.clone(), left, None);
                let right = left
                    .clone()
                    .map(|left| {
                        db.query_call_chain(
                            nameholders,
                            right,
                            Some(left.returning_avalue().return_called_type(db)),
                        )
                    })
                    .flatten();

                left.and(right)
                    .map(|(left, right)| cir::Action::Assigment(left, right, assign_mod))
            }
            Action::Property(ast_property) => db
                .query_property(Some(event_type), ast_property)
                .map(|property_decl| {
                    let span = property_decl.name.span;
                    let chain: AValueChain =
                        cir::AValue::RValue(RValue::Property(property_decl.intern(db)), span)
                            .into();
                    cir::Action::AValueChain(chain)
                }),
        })
        .collect::<QueryTrisult<_>>()
}

pub(super) fn query_rule_cond(
    db: &dyn DefQuery,
    event_decl_id: EventDeclId,
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
    let args = |event_decl_id: &EventDeclId| {
        let args_span = rule.args.span;

        rule.args
            .value
            .into_iter()
            .map(|call_arg| {
                db.query_call_chain(
                    smallvec![Nameholder::Root],
                    call_arg.clone().call_chain(),
                    None,
                )
                .map(|it| match call_arg {
                    CallArg::Named(name, _, _) => (Some(name), it),
                    CallArg::Pos(_) => (None, it),
                })
            })
            .collect::<QueryTrisult<Vec<_>>>()
            .spanned(args_span)
            .and(db.query_event_def_by_id(*event_decl_id))
            .flat_map(|(args, event_def)| db.query_called_args(args, event_def.args, None))
    };

    db.query_namespaced_event(smallvec![Nameholder::Root], rule.event)
        .and_with(args)
        .and_with(|(event_decl_id, _)| {
            db.query_rule_cond(*event_decl_id, rule.conditions)
                .map_inner(Predicate)
        })
        .and_with(|((event_decl_id, _), _)| db.query_rule_actions(*event_decl_id, rule.actions))
        .map(|(((event_decl_id, args), conditions), actions)| cir::Rule {
            title: rule.name.value,
            event: event_decl_id,
            args,
            conditions,
            actions,
        })
}
