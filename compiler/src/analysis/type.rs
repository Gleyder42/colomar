use super::super::analysis::decl::DeclQuery;
use super::super::{cir, cst, Ident, QueryTrisult};

use super::super::analysis::namespace::Nameholders;
use super::super::trisult::Errors;
use crate::cir::{CalledType, GenericTypeBoundMap, Type, TypeDesc, VirtualTypeKind};
use crate::error::CompilerError;
use crate::tri;
use std::collections::HashMap;

pub(super) fn query_generic_type_bound_map(
    db: &dyn DeclQuery,
    called_type: CalledType,
) -> QueryTrisult<GenericTypeBoundMap> {
    let mut errors = Errors::new();
    let message = "Only first level generics are currently implemented";
    let mut bound_map = GenericTypeBoundMap::new();

    match called_type.r#type {
        VirtualTypeKind::Type(r#type) => {
            for bound_generic in r#type.generics {
                if !bound_generic.generics.is_empty() {
                    errors.push(CompilerError::NotImplemented(
                        message.into(),
                        called_type.span,
                    ));
                } else {
                    let mut generics = match r#type.desc {
                        TypeDesc::Struct(r#struct) => {
                            db.lookup_intern_struct_decl(r#struct).generics
                        }
                        _ => continue,
                    };

                    bound_map.insert(generics.remove(0).value, Type::from(bound_generic.r#type));
                }
            }
            errors.value(bound_map)
        }
        VirtualTypeKind::Generic(ident) => {
            let error = CompilerError::NotImplemented(message.into(), ident.span);
            errors.par(bound_map, error)
        }
    }
}

pub(super) fn resolve_generics(
    db: &dyn DeclQuery,
    nameholders: Nameholders,
    generics: Vec<cst::BoundGeneric>,
) -> QueryTrisult<Vec<cir::BoundGeneric>> {
    generics
        .into_iter()
        .map(|generic| {
            let mut errors = Errors::new();

            let r#type: TypeDesc = tri!(
                db.query_namespaced_type_desc(nameholders.clone(), generic.ident),
                errors
            );
            let generics = tri!(
                db.resolve_generics(nameholders.clone(), generic.bound_generics),
                errors
            );

            let bound_generic = cir::BoundGeneric { r#type, generics };
            errors.value(bound_generic)
        })
        .collect()
}

pub(super) fn query_type_map(db: &dyn DeclQuery) -> QueryTrisult<HashMap<Ident, TypeDesc>> {
    let mut errors = Errors::new();

    let type_items = tri!(db.query_type_items(), errors);

    let map: HashMap<_, _> = type_items
        .into_iter()
        .filter_map(|root| match root {
            cst::TypeRoot::Event(event) => Some((
                event.decl.name.clone(),
                TypeDesc::Event(db.query_event_decl(event.decl)),
            )),
            cst::TypeRoot::Enum(r#enum) => Some((
                r#enum.decl.name.clone(),
                TypeDesc::Enum(db.query_enum_decl(r#enum.decl)),
            )),
            cst::TypeRoot::Struct(r#struct) => Some((
                r#struct.decl.name.clone(),
                TypeDesc::Struct(db.query_struct_decl(r#struct.decl)),
            )),
        })
        .collect();

    errors.value(map)
}
