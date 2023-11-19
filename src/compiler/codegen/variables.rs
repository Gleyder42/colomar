use crate::compiler::cir::Type;
use crate::compiler::codegen::Codegen;
use crate::compiler::wst::Ident;
use crate::compiler::{cir, cst, wst, QueryTrisult};

pub(super) fn query_player_variables(db: &dyn Codegen) -> QueryTrisult<Vec<wst::Variable>> {
    let player_struct: cst::Struct = db.query_player_struct_def();
    let struct_decl_id = db.query_struct_decl(player_struct.declaration);

    type PropertiesTrisult = QueryTrisult<Vec<cir::PropertyDecl>>;
    player_struct
        .definition
        .properties
        .into_iter()
        .filter(|property| property.is_native.is_none())
        .map(|property| db.query_property(Some(Type::Struct(struct_decl_id)), property))
        .collect::<PropertiesTrisult>()
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
