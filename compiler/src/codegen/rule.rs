use super::super::codegen::{Caller, Codegen};
use super::super::error::CompilerError;
use super::super::trisult::IntoTrisult;
use super::super::{cir, wst, Op, QueryTrisult};

use crate::error::ErrorCause;
use std::collections::{HashMap, VecDeque};

pub(super) fn query_wst_rule(db: &dyn Codegen, rule: cir::Rule) -> QueryTrisult<wst::Rule> {
    let event_decl: cir::EventDecl = db.lookup_intern_event_decl(rule.event);
    let args = db.query_event_def_by_id(rule.event).flat_map(|event_def| {
        db.query_wst_call_from_args(event_def.args, rule.args)
            .into_iter()
            .map(|arg| {
                db.query_wst_call(None, cir::Action::from(arg.value))
                    .map(|call| (arg.name, call))
            })
            .collect::<QueryTrisult<Vec<_>>>()
            .flat_map(|args| {
                let arg_map: HashMap<_, _> = args
                    .into_iter()
                    // TODO Verify that .name(db) is correct here
                    .map(|(ident, call)| (ident.value.name(db), call))
                    .collect();

                db.query_wscript_event_impl(event_decl.name.value.name(db))
                    .complete_with_span(event_decl.span)
                    .flat_map(|event| {
                        event
                            .value
                            .args
                            .into_iter()
                            .map(|arg_name| {
                                arg_map.get(arg_name.as_str()).cloned().trisult_ok_or(
                                    CompilerError::CannotFindNativeDef(
                                        arg_name,
                                        ErrorCause::Span(event_decl.name.span),
                                    ),
                                )
                            })
                            .collect::<QueryTrisult<VecDeque<wst::Call>>>()
                    })
            })
    });

    let query_event_wst_call = |chain: Vec<cir::Action>| -> QueryTrisult<Vec<wst::Call>> {
        chain
            .into_iter()
            .map(|action| {
                let caller = Caller {
                    wst: None,
                    cir: cir::AValue::RValue(rule.event.into(), action.ghost_span()),
                };
                db.query_wst_call(Some(caller), action)
            })
            .collect::<QueryTrisult<Vec<wst::Call>>>()
    };

    let conditions = rule.conditions.into_iter().map(cir::Action::from).collect();

    let wscript_event_name = db
        .query_wscript_event_name_impl(event_decl.name.value.name(db))
        .complete_with_span(event_decl.span);

    args.and(query_event_wst_call(rule.actions).map_inner(|it| it.unwrap_function()))
        .and(
            query_event_wst_call(conditions).map_inner(|it| wst::Condition {
                left: Box::new(it),
                op: Op::Equals,
                right: Box::new(wst::Call::Ident("True".into())),
            }),
        )
        .and(wscript_event_name)
        .map(
            |(((mut args, actions), conditions), event_name)| wst::Rule {
                title: rule.title,
                event: wst::Event {
                    name: event_name.into(),
                    team: args
                        .pop_front()
                        .map(|it| db.query_const_eval(it).unwrap_ok()),
                    hero_slot: args
                        .pop_front()
                        .map(|it| db.query_const_eval(it).unwrap_ok()),
                },
                actions,
                conditions,
            },
        )
}
