use crate::compiler::analysis::def::DefQuery;
use crate::compiler::{cir, cst};
use crate::compiler::{QueryTrisult, Text};

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

pub(super) fn query_player_struct_def(db: &dyn DefQuery) -> cst::Struct {
    const PLAYER_STRUCT: Text = Text::new_inline("Player");

    let mut player_struct: Vec<_> = db
        .input_content()
        .into_iter()
        .flat_map(|root| {
            if let cst::Root::Struct(r#struct) = root {
                if r#struct.declaration.name.value == PLAYER_STRUCT {
                    Some(r#struct)
                } else {
                    None
                }
            } else {
                None
            }
        })
        .collect();
    let player_struct = player_struct.remove(0);
    player_struct
}
