use crate::compiler::cir::AValueChain;
use crate::compiler::codegen::Codegen;
use crate::compiler::error::CompilerError;
use crate::compiler::trisult::Trisult;
use crate::compiler::{cir, wst, QueryTrisult};
use crate::query_error;
use chumsky::Parser;

pub(super) fn query_wst_rule(db: &dyn Codegen, rule: cir::Rule) -> QueryTrisult<wst::Rule> {
    let event_decl: cir::EventDeclaration = db.lookup_intern_event_decl(rule.event);
    db.query_event_def_by_id(rule.event).map(|event_def| {
        let supplied_avalue_chains: QueryTrisult<Vec<AValueChain>> = event_def
            .arguments
            .iter()
            .enumerate()
            .map(|(index, decl_arg_id)| {
                let decl_arg: cir::DeclaredArgument = db.lookup_intern_decl_arg(*decl_arg_id);
                let called_arg = rule.arguments.get(index).cloned();

                match (called_arg, decl_arg.default_value) {
                    (Some(called_arg), _) => Trisult::Ok(called_arg.value),
                    (None, Some(default_value)) => Trisult::Ok(default_value),
                    (None, None) => query_error!(CompilerError::MissingArgument {
                        missing_arg: *decl_arg_id,
                        call_site: event_decl.span.clone()
                    }),
                }
            })
            .collect();
    });

    todo!()
}
