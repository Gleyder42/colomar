use crate::codegen::{Caller, Codegen};
use crate::error::CompilerError;
use crate::trisult::IntoTrisult;
use crate::{cir, wst, Op, QueryTrisult};

use crate::cir::AValueChain;
use std::collections::{HashMap, VecDeque};

pub(super) fn query_wst_rule(db: &dyn Codegen, rule: cir::Rule) -> QueryTrisult<wst::Rule> {
    let event_decl: cir::EventDeclaration = db.lookup_intern_event_decl(rule.event);
    let args = db.query_event_def_by_id(rule.event).flat_map(|event_def| {
        db.query_wst_call_from_args(event_def.arguments, rule.arguments)
            .into_iter()
            .map(|arg| {
                db.query_wst_call(None, arg.value)
                    .map(|call| (arg.name, call))
            })
            .collect::<QueryTrisult<Vec<_>>>()
            .flat_map(|args| {
                let arg_map: HashMap<_, _> = args
                    .into_iter()
                    .map(|(ident, call)| (ident.value, call))
                    .collect();

                db.query_wscript_event_impl(event_decl.name.value.clone())
                    .flat_map(|event| {
                        event
                            .args
                            .into_iter()
                            .map(|arg_name| {
                                arg_map.get(arg_name.as_str()).cloned().trisult_ok_or(
                                    CompilerError::CannotFindNativeDefinition(arg_name.into()),
                                )
                            })
                            .collect::<QueryTrisult<Vec<wst::Call>>>()
                    })
            })
            .map(|it| it.into_iter().collect::<VecDeque<_>>()) // TODO Add trait converting into all containers
    });

    let query_event_wst_call = |chain: Vec<AValueChain>| -> QueryTrisult<Vec<wst::Call>> {
        chain
            .into_iter()
            .map(|actions| {
                let caller = Caller {
                    wst: None,
                    cir: cir::AValue::RValue(rule.event.into(), actions.ghost_span()),
                };
                db.query_wst_call(Some(caller), actions)
            })
            .collect::<QueryTrisult<Vec<wst::Call>>>()
    };

    let conditions = rule.conditions.into_iter().map(|it| it.0).collect();

    let wscript_event_name = db.query_wscript_event_name_impl(event_decl.name.value);

    args.and_require(query_event_wst_call(rule.actions).map_inner(|it| it.unwrap_function()))
        .and_require(
            query_event_wst_call(conditions).map_inner(|it| wst::Condition {
                left: Box::new(it),
                op: Op::Equals,
                right: Box::new(wst::Call::Ident("True".into())),
            }),
        )
        .and_require(wscript_event_name)
        .map(
            |(((mut args, actions), conditions), event_name)| wst::Rule {
                title: rule.title,
                event: wst::Event {
                    name: event_name.into(),
                    team: db.query_const_eval(args.pop_front().unwrap()).unwrap_ok(),
                    hero_slot: db.query_const_eval(args.pop_front().unwrap()).unwrap_ok(),
                },
                actions,
                conditions,
            },
        )
}
