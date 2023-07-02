use crate::compiler::analysis::def::DefQuery;
use crate::compiler::QueryTrisult;
use crate::compiler::{cir, cst};

pub(super) fn query_im(db: &dyn DefQuery) -> QueryTrisult<cir::Cir> {
    db.input_content()
        .into_iter()
        .map(|root| match root {
            cst::Root::Event(event) => db.query_event(event).map(cir::Root::Event),
            cst::Root::Rule(rule) => db.query_rule_decl(rule).map(cir::Root::Rule),
            cst::Root::Enum(r#enum) => db.query_enum(r#enum).map(cir::Root::Enum),
            cst::Root::Struct(r#struct) => db.query_struct(r#struct).map(cir::Root::Struct),
        })
        .collect::<QueryTrisult<_>>()
        // TODO Decide how to handle this
        // query_root_namespace() has to be called here to include the errors
        .and_only_errors(db.query_root_namespace())
        .map(cir::Cir)
}
