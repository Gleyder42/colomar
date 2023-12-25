use super::super::analysis::decl::DeclQuery;
use super::super::analysis::interner::{Interner, IntoInternId};
use super::super::cir::{
    EnumConstant, EnumConstantId, EnumDeclId, EnumDef, EventDeclId, FunctionDecl, PropertyDecl,
    RValue, StructDeclId, Type, VirtualType,
};
use super::super::error::CompilerError;
use super::super::trisult::Trisult;
use super::super::{flatten, HashableMap, Ident, QueryTrisult, StructId, TextId};
use crate::query_error;

use super::super::cst::{FunctionDecls, PropertyDecls};
use super::super::span::StringInterner;
use colomar_macros::Interned;
use smallvec::{smallvec, SmallVec};
use std::collections::HashMap;
use std::hash::Hash;
use std::rc::Rc;

pub(super) fn query_root_namespace(db: &dyn DeclQuery) -> QueryTrisult<NamespaceId> {
    let mut namespace = Namespace::new();

    db.query_type_map()
        .into_iter()
        .map(|(ident, r#type)| {
            let allow_duplicates = r#type.is_partial(db).unwrap_or(false);
            namespace.add(allow_duplicates, ident, RValue::Type(r#type), db)
        })
        .collect::<QueryTrisult<Vec<()>>>()
        .map(|_| Rc::new(namespace))
        .intern(db)
}

pub(super) fn query_enum_namespace(
    db: &dyn DeclQuery,
    r#enum: EnumDeclId,
) -> QueryTrisult<NamespaceId> {
    db.query_enum_def(r#enum)
        .map(|r#enum| Rc::new(Namespace::from_enum_def(db, r#enum.def)))
        .intern(db)
}

pub(super) fn query_event_namespace(
    db: &dyn DeclQuery,
    event_decl: EventDeclId,
) -> QueryTrisult<NamespaceId> {
    db.query_event_context_variables(event_decl)
        .flat_map(|properties| {
            let mut namespace = Namespace::new();
            properties
                .into_iter()
                .map(|property_decl| {
                    namespace.add(
                        false,
                        property_decl.name.clone(),
                        RValue::Property(property_decl.intern(db)),
                        db,
                    )
                })
                .collect::<QueryTrisult<Vec<()>>>()
                .map(|_| Rc::new(namespace).intern(db))
        })
}

pub(super) fn query_bool_name(db: &dyn DeclQuery) -> TextId {
    db.intern_string("bool".to_owned())
}

pub(super) fn query_string_name(db: &dyn DeclQuery) -> TextId {
    db.intern_string("string".to_owned())
}

pub(super) fn query_number_name(db: &dyn DeclQuery) -> TextId {
    db.intern_string("num".to_owned())
}

pub(super) fn query_primitives(db: &dyn DeclQuery) -> QueryTrisult<HashMap<TextId, Type>> {
    db.query_namespace(smallvec![Nameholder::Root])
        .map(|namespace| {
            let mut map = HashMap::new();
            let mut add = |name: TextId| {
                if let Some(RValue::Type(r#type)) = namespace.get(name) {
                    map.insert(name.clone(), r#type);
                }
            };

            add(db.query_string_name());
            add(db.query_bool_name());
            add(db.query_number_name());

            map
        })
}

fn struct_decl_id_or_panic(r#type: &Type) -> StructDeclId {
    match r#type {
        Type::Struct(struct_decl_id) => *struct_decl_id,
        Type::Enum(_) => panic!("Cannot get struct decl id of type Enum"),
        Type::Event(_) => panic!("Cannot get struct decl id of type Event"),
        Type::Unit => panic!("Cannot get struct decl id of type Unit"),
    }
}

pub(super) fn query_number_type(db: &dyn DeclQuery) -> QueryTrisult<StructDeclId> {
    db.query_primitives().flat_map(|map| {
        map.get(&db.query_number_name())
            .map(struct_decl_id_or_panic)
            .ok_or(CompilerError::CannotFindPrimitiveDecl(
                db.query_number_name(),
            ))
            .into()
    })
}

pub(super) fn query_string_type(db: &dyn DeclQuery) -> QueryTrisult<StructDeclId> {
    db.query_primitives().flat_map(|map| {
        map.get(&db.query_string_name())
            .map(struct_decl_id_or_panic)
            .ok_or(CompilerError::CannotFindPrimitiveDecl(
                db.query_string_name(),
            ))
            .into()
    })
}

pub(super) fn query_bool_type(db: &dyn DeclQuery) -> QueryTrisult<StructDeclId> {
    db.query_primitives().flat_map(|map| {
        map.get(&db.query_bool_name())
            .map(struct_decl_id_or_panic)
            .ok_or(CompilerError::CannotFindPrimitiveDecl(db.query_bool_name()))
            .into()
    })
}

pub(super) fn query_struct_namespace(
    db: &dyn DeclQuery,
    struct_decl_id: StructDeclId,
) -> QueryTrisult<NamespaceId> {
    let struct_id: StructId = db.lookup_intern_struct_decl(struct_decl_id).name.value;

    db.query_ast_struct_def(struct_id).flat_map(|struct_def| {
        println!("{}", struct_def.len());
        let (properties, functions): (PropertyDecls, FunctionDecls) =
            flatten(struct_def, |it| (it.properties, it.functions));

        Trisult::Ok(Namespace::new())
            .fold_with(
                db.query_struct_properties(struct_decl_id, properties),
                |mut namespace, property_id| {
                    let property: PropertyDecl = db.lookup_intern_property_decl(property_id);
                    let result = namespace.add(
                        true,
                        property.name.clone(),
                        RValue::Property(property.intern(db)),
                        db,
                    );
                    result.map(|_| namespace).into()
                },
            )
            .fold_with(
                db.query_struct_functions(struct_decl_id, functions),
                |mut namespace, function_id| {
                    let function: FunctionDecl = db.lookup_intern_function_decl(function_id);
                    let result = namespace.add(
                        true,
                        function.name.clone(),
                        RValue::Function(function.intern(db)),
                        db,
                    );
                    result.map(|_| namespace).into()
                },
            )
            .map(Rc::new)
            .intern(db)
    })
}

pub(super) fn query_namespaced_rvalue(
    db: &dyn DeclQuery,
    nameholders: Nameholders,
    ident: Ident,
) -> QueryTrisult<RValue> {
    db.query_namespace(nameholders).flat_map(|namespace| {
        namespace
            .get(ident.value)
            .ok_or_else(|| {
                println!("{}", db.lookup_intern_string(ident.value));
                namespace.print_content(db);

                dbg!(CompilerError::CannotFindIdent(ident))
            })
            .into()
    })
}

pub(super) fn query_namespace(
    db: &dyn DeclQuery,
    nameholders: Nameholders,
) -> QueryTrisult<Rc<Namespace>> {
    nameholders
        .into_iter()
        .map(|nameholder| {
            match nameholder {
                Nameholder::Root => db
                    .query_root_namespace()
                    .drop_errors(|| empty_namespace(db)),
                Nameholder::Enum(enum_placeholder) => match enum_placeholder {
                    // TODO Current match should return the enum constant id and not the namespace id
                    EnumNameholder::ByEnum(enum_decl) => db.query_enum_namespace(enum_decl),
                    EnumNameholder::ByConstant(enum_constant_id) => {
                        let enum_constant = db.lookup_intern_enum_constant(enum_constant_id);
                        db.query_enum_namespace(enum_constant.r#enum)
                    }
                },
                Nameholder::Struct(struct_decl) => db.query_struct_namespace(struct_decl),
                Nameholder::Event(event_decl) => db.query_event_namespace(event_decl),
                Nameholder::Empty => Trisult::Ok(db.intern_namespace(Rc::new(Namespace::new()))),
            }
        })
        .collect::<QueryTrisult<Vec<_>>>()
        .fold(Rc::new(Namespace::new()), |acc, item| {
            let namespace: Rc<Namespace> = db.lookup_intern_namespace(item);
            let mut namespace = (*namespace).clone();
            namespace.parent.push(acc);
            Trisult::Ok(Rc::new(namespace))
        })
}

pub(super) fn query_namespaced_type(
    db: &dyn DeclQuery,
    nameholders: Nameholders,
    ident: Ident,
) -> QueryTrisult<Type> {
    db.query_namespaced_rvalue(nameholders, ident.clone())
        .flat_map(|rvalue| match rvalue {
            RValue::Type(r#type) => Trisult::Ok(r#type),
            rvalue @ (RValue::EnumConstant(_) | RValue::Property(_) | RValue::Function(_)) => {
                CompilerError::NotA("Type", rvalue.name(db), ident).into()
            }
        })
}

pub(super) fn query_namespaced_function(
    db: &dyn DeclQuery,
    nameholders: Nameholders,
    ident: Ident,
) -> QueryTrisult<FunctionDecl> {
    db.query_namespaced_rvalue(nameholders, ident.clone())
        .flat_map(|rvalue| match rvalue {
            RValue::Function(function) => Trisult::Ok(db.lookup_intern_function_decl(function)),
            rvalue @ (RValue::Type(_) | RValue::Property(_) | RValue::EnumConstant(_)) => {
                query_error!(CompilerError::NotA("Function", rvalue.name(db), ident))
            }
        })
}

pub(super) fn query_namespaced_event(
    db: &dyn DeclQuery,
    nameholders: Nameholders,
    ident: Ident,
) -> QueryTrisult<EventDeclId> {
    db.query_namespaced_type(nameholders, ident.clone())
        .flat_map(|r#type| match r#type {
            Type::Event(event) => Trisult::Ok(event),
            r#type @ (Type::Enum(_) | Type::Struct(_) | Type::Unit) => {
                Trisult::Err(vec![CompilerError::NotA(
                    "Event",
                    RValue::Type(r#type).name(db),
                    ident,
                )])
            }
        })
}

/// Placeholder for [Namespace].
/// The name is a combination of **Name**space and Place**holder**
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Nameholder {
    Root,
    Empty,
    Enum(EnumNameholder),
    Struct(StructDeclId),
    Event(EventDeclId),
}

// TODO Reduce this to 1
pub type Nameholders = SmallVec<[Nameholder; 2]>;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum EnumNameholder {
    ByEnum(EnumDeclId),
    ByConstant(EnumConstantId),
}

impl From<VirtualType> for Nameholder {
    fn from(value: VirtualType) -> Self {
        Nameholder::from(value.r#type)
    }
}

impl From<&VirtualType> for Nameholder {
    fn from(value: &VirtualType) -> Self {
        Nameholder::from(value.r#type)
    }
}

impl From<EnumNameholder> for Nameholder {
    fn from(value: EnumNameholder) -> Self {
        Nameholder::Enum(value)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, Interned)]
pub struct Namespace {
    parent: Vec<Rc<Namespace>>,
    map: HashableMap<TextId, RValue>,
}

pub fn empty_namespace(db: &(impl Interner + ?Sized)) -> NamespaceId {
    Rc::new(Namespace::new()).intern(db)
}

impl Namespace {
    fn new() -> Namespace {
        Namespace {
            map: HashableMap::new(),
            parent: Vec::new(),
        }
    }

    pub fn print_content(&self, db: &(impl StringInterner + Interner + ?Sized)) {
        self.parent.iter().for_each(|it| it.print_content(db));
        for (key, value) in &self.map {
            println!("{} | {:?}", db.lookup_intern_string(*key), value.name(db))
        }
    }

    pub fn from_enum_def(db: &(impl Interner + ?Sized), def: EnumDef) -> Namespace {
        let map = def
            .constants
            .into_iter()
            .map::<(_, EnumConstant), _>(|enum_constant_id| {
                (
                    enum_constant_id,
                    db.lookup_intern_enum_constant(enum_constant_id),
                )
            })
            .map(|it| (it.1.name.value.clone(), RValue::EnumConstant(it.0)))
            .collect();
        Namespace {
            parent: Vec::new(),
            map,
        }
    }

    /// Ignore duplicates to support partial structs
    fn add<I: Interner + ?Sized>(
        &mut self,
        allow_duplicates: bool,
        ident: Ident,
        rvalue: RValue,
        db: &I,
    ) -> Result<(), CompilerError> {
        match (self.contains(ident.value), allow_duplicates) {
            (Some(root), false) => {
                CompilerError::DuplicateIdent {
                    first: root.name(db), // TODO How to deal with errors?
                    second: ident,
                }
                .into()
            }
            (Some(_), true) => Ok(()),
            (None, _) => {
                self.map.insert(ident.value.clone(), rvalue);
                Ok(())
            }
        }
    }

    fn get(&self, ident_value: TextId) -> Option<RValue> {
        self.contains(ident_value)
    }

    fn contains(&self, ident_value: TextId) -> Option<RValue> {
        let option = self.map.get(&ident_value).cloned();
        if option.is_some() {
            option
        } else {
            self.parent
                .iter()
                .map(|it| it.contains(ident_value))
                .filter(|it| it.is_some())
                .flatten()
                .collect::<Vec<_>>()
                .pop()
        }
    }
}

impl IntoInternId for Rc<Namespace> {
    type Interned = NamespaceId;

    fn intern<T: Interner + ?Sized>(self, db: &T) -> NamespaceId {
        db.intern_namespace(self)
    }
}

impl<E> Trisult<Namespace, E> {
    pub fn fold_with<T, I, F>(self, with: Trisult<I, E>, func: F) -> Trisult<Namespace, E>
    where
        F: Fn(Namespace, T) -> Trisult<Namespace, E>,
        I: IntoIterator<Item = T>,
    {
        self.flat_map(|acc| with.fold(acc, func))
    }
}

impl From<Type> for Nameholder {
    fn from(value: Type) -> Self {
        match value {
            Type::Enum(r#enum) => Nameholder::Enum(EnumNameholder::ByEnum(r#enum)),
            Type::Struct(r#struct) => Nameholder::Struct(r#struct),
            Type::Event(event) => Nameholder::Event(event),
            Type::Unit => Nameholder::Empty,
        }
    }
}

impl Nameholder {
    pub fn from_rvalue(value: RValue, db: &(impl Interner + ?Sized)) -> Self {
        match value {
            RValue::Type(r#type) => r#type.into(),
            RValue::EnumConstant(enum_constant_id) => {
                EnumNameholder::ByConstant(enum_constant_id).into()
            }
            RValue::Property(property_decl) => {
                db.lookup_intern_property_decl(property_decl).r#type.into()
            }
            RValue::Function(function_decl) => db
                .lookup_intern_function_decl(function_decl)
                .return_type
                .into(),
        }
    }
}
