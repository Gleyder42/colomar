use std::collections::{HashMap, HashSet, VecDeque};
use crate::compiler::cir::{AValue, AValueChain, RValue};
use crate::compiler::codegen::Codegen;
use crate::compiler::error::CompilerError;
use crate::compiler::trisult::{IntoTrisult, Trisult};
use crate::compiler::{cir, wst, QueryTrisult};
use crate::query_error;
use chumsky::Parser;

pub(super) fn query_wst_rule(db: &dyn Codegen, rule: cir::Rule) -> QueryTrisult<wst::Rule> {
    let event_decl: cir::EventDeclaration = db.lookup_intern_event_decl(rule.event);
    let all_decl_args = db.query_event_def_by_id(rule.event)
        .map(|event_def| event_def.arguments
            .into_iter()
            .collect::<HashSet<_>>());

    let supplied_decl_args: HashSet<_> = rule.arguments
        .iter()
        .map(|called_arg| called_arg.declared)
        .collect();

    let args = all_decl_args
        .map(|all_decl_args| {
            all_decl_args.difference(&supplied_decl_args)
                .map(|decl_arg_id| db.lookup_intern_decl_arg(*decl_arg_id))
                .collect::<Vec<cir::DeclaredArgument>>()
        })
        .and_require(Trisult::Ok(rule.arguments))
        .flat_map(|(defaulted_args, supplied_args)| {
            const ERROR: &str = "Compiler Bug:
            Not supplied arguments (e.g. arguments for a function)\
            should have default values when generating workshop code";

            let mut args: Vec<_> = defaulted_args.into_iter()
                .map(|decl_arg| (decl_arg.position, decl_arg.name, decl_arg.default_value.expect(ERROR)))
                .chain(supplied_args
                    .into_iter()
                    .map(|called_arg| {
                        let decl_arg: cir::DeclaredArgument = db.lookup_intern_decl_arg(called_arg.declared);
                        (decl_arg.position, decl_arg.name, called_arg.value)
                    })
                )
                .collect();


            args.sort_by_key(|(index, _, _)| *index);
            args.into_iter()
                .map(|(_, name, value)| db.query_wst_call(None, value).map(|call| (name, call)))
                .collect::<QueryTrisult<Vec<_>>>()
                .flat_map(|args| {
                    let arg_map: HashMap<_, _> = args
                        .into_iter()
                        .map(|(ident, call)| (ident.value, call))
                        .collect();

                    db.query_wscript_event_impl(event_decl.name.value.clone())
                        .flat_map(|event| {
                            event.args
                                .into_iter()
                                .map(|arg_name| {
                                    arg_map
                                        .get(arg_name.as_str())
                                        .cloned()
                                        .trisult_ok_or(CompilerError::CannotFindNativeDefinition(arg_name.into()))
                                })
                                .collect::<QueryTrisult<Vec<wst::Call>>>()
                        })
                        .map(|it| it.into_iter().collect::<VecDeque<_>>())
                })
        });

    args.map(|mut args| wst::Rule {
        title: rule.title,
        event: wst::Event {
            name: event_decl.name.into(),
            team: db.query_const_eval(args.pop_front().unwrap()).unwrap_ok(),
            hero_slot: db.query_const_eval(args.pop_front().unwrap()).unwrap_ok()
        },
        actions: Vec::new(),
        conditions: Vec::new(),
    })
}
