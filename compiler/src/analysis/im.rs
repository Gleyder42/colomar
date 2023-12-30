use super::super::analysis::def::DefQuery;
use super::super::QueryTrisult;
use super::super::{cir, cst};
use crate::tri;
use crate::trisult::Errors;

pub(super) fn query_im(db: &dyn DefQuery) -> QueryTrisult<cir::Cir> {
    let mut errors = Errors::new();

    let action_items = tri!(db.query_action_items(), errors);

    action_items
        .into_iter()
        .map(|root| match root {
            cst::Root::Event(event) => db.query_event(event).map(cir::Root::Event),
            cst::Root::Rule(rule) => db.query_rule_decl(rule).map(cir::Root::Rule),
            cst::Root::Enum(r#enum) => db.query_enum(r#enum).map(cir::Root::Enum),
            cst::Root::Struct(r#struct) => db.query_struct(r#struct).map(cir::Root::Struct),
            cst::Root::Import(_) => unreachable!(),
        })
        .collect::<QueryTrisult<_>>()
        .merge_errors(errors)
        // TODO Decide how to handle this
        // query_root_namespace() has to be called here to include the errors
        .and_only_errors(db.query_root_namespace())
        .map(cir::Cir)
}

pub(super) fn query_player_struct_def(db: &dyn DefQuery) -> QueryTrisult<cst::Struct> {
    db.query_struct_by_name(db.query_player_name())
        .map(|mut structs| structs.remove(0))
}
