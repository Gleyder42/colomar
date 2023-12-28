use super::super::cir::Type;
use super::super::codegen::Codegen;
use super::super::wst::Ident;
use super::super::{cir, cst, wst, QueryTrisult};
use crate::tri;
use crate::trisult::Errors;

pub(super) fn query_player_variables(db: &dyn Codegen) -> QueryTrisult<Vec<wst::Variable>> {
    let mut errors = Errors::new();

    let player_struct: cst::Struct = tri!(db.query_player_struct_def(), errors);
    let struct_decl_id = db.query_struct_decl(player_struct.decl);

    type PropertiesTrisult = QueryTrisult<Vec<cir::PropertyDecl>>;
    player_struct
        .def
        .properties
        .into_iter()
        .filter(|property| property.is_native.is_none())
        .map(|property| db.query_property(Some(Type::Struct(struct_decl_id)), property))
        .collect::<PropertiesTrisult>()
        .merge_errors(errors)
        .map(|properties| {
            properties
                .into_iter()
                .enumerate()
                .map(|(index, property)| wst::Variable {
                    name: Ident::from_ident(property.name, db),
                    index: index as u8,
                })
                .collect::<Vec<_>>()
        })
}
