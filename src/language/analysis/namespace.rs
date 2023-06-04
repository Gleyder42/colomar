use std::borrow::ToOwned;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use once_cell::unsync::Lazy;
use salsa::InternId;
use crate::{impl_intern_key, query_error};
use crate::language::{Ident, im, ImmutableString};
use crate::language::analysis::decl::DeclQuery;
use crate::language::analysis::error::{AnalysisError, QueryResult};
use crate::language::analysis::interner::{Interner, IntoInternId};
use crate::language::ast::EventDeclaration;
use crate::language::im::{EnumConstant, EnumConstantId, EnumDeclarationId, EnumDefinition, EventDeclarationId, FunctionDecl, FunctionDeclId, PropertyDecl, RValue, StructDeclaration, StructDeclarationId, Type};

pub(in super) fn query_root_namespace(db: &dyn DeclQuery) -> Result<NamespaceId, AnalysisError> {
    let mut namespace = Namespace::new();

    for (ident, r#type) in db.query_type_map() {
        namespace.add(ident, RValue::Type(r#type), db)?;
    }

    Ok(Rc::new(namespace).intern(db))
}

pub(in super) fn query_enum_namespace(db: &dyn DeclQuery, r#enum: EnumDeclarationId) -> QueryResult<NamespaceId, AnalysisError> {
    db.query_enum_def(r#enum)
        .map(|r#enum| Rc::new(Namespace::from_enum_definition(db, r#enum.definition)))
        .intern(db)
}

pub(in super) fn query_event_namespace(db: &dyn DeclQuery, event_decl: EventDeclarationId) -> QueryResult<NamespaceId, AnalysisError> {
    db.query_event_context_variables(event_decl)
        .flat_map(|properties| {
            let mut namespace = Namespace::new();
            properties.into_iter()
                .map(|property_decl| {
                    namespace.add(property_decl.name.clone(), RValue::Property(property_decl), db)
                }).collect::<QueryResult<Vec<_>, _>>()
                .map(|_| Rc::new(namespace).intern(db))
        })
}

const BOOL_NAME: Lazy<ImmutableString> = Lazy::new(|| ImmutableString::new("bool".to_owned()));
const STRING_NAME: Lazy<ImmutableString> = Lazy::new(|| ImmutableString::new("string".to_owned()));

pub(in super) fn query_primitives(
    db: &dyn DeclQuery
) -> QueryResult<HashMap<ImmutableString, Type>, AnalysisError> {
    db.query_namespace(vec![Nameholder::Root])
        .map(|namespace| {
            let mut map = HashMap::new();
            let mut add = |name: ImmutableString| {
                if let Some(RValue::Type(r#type)) = namespace.get(&BOOL_NAME) {
                    map.insert(name.clone(), r#type);
                }
            };

            add((*BOOL_NAME).clone());
            add((*STRING_NAME).clone());

            map
        })
}

fn struct_decl_id_or_panic(r#type: &Type) -> StructDeclarationId {
    match r#type {
        Type::Struct(struct_decl_id) => struct_decl_id.clone(),
        Type::Enum(_) => panic!("Cannot get struct decl id of type Enum"),
        Type::Event(_) => panic!("Cannot get struct decl id of type Event"),
        Type::Unit => panic!("Cannot get struct decl id of type Unit")
    }
}

pub(in super) fn query_string_type(db: &dyn DeclQuery) -> QueryResult<StructDeclarationId, AnalysisError> {
    db.query_primitives().flat_map(|map| {
        map.get(&*STRING_NAME)
            .map(struct_decl_id_or_panic)
            .ok_or_else(|| AnalysisError::WrongType).into()
    })
}

pub(in super) fn query_bool_type(db: &dyn DeclQuery) -> QueryResult<StructDeclarationId, AnalysisError> {
    db.query_primitives().flat_map(|map| {
        map.get(&*BOOL_NAME)
            .map(struct_decl_id_or_panic)
            .ok_or_else(|| AnalysisError::WrongType).into()
    })
}

pub(in super) fn query_struct_namespace(db: &dyn DeclQuery, struct_decl_id: StructDeclarationId) -> QueryResult<NamespaceId, AnalysisError> {
    db.query_ast_struct_def(struct_decl_id)
        .flat_map(|struct_def| {
            QueryResult::Ok(Namespace::new())
                .fold_with(
                    db.query_struct_properties(struct_def.properties),
                    |mut namespace, property_id| {
                        let property: PropertyDecl = db.lookup_intern_property_decl(property_id);
                        let result = namespace.add(property.name.clone(), RValue::Property(property), db);
                        result.map(|_| namespace).into()
                    })
                .fold_with(
                    db.query_struct_functions(struct_def.functions),
                    |mut namespace, function_id| {
                        let function: FunctionDecl = db.lookup_intern_function_decl(function_id);
                        let result = namespace.add(function.name.clone(), RValue::Function(function), db);
                        result.map(|_| namespace).into()
                    },
                ).map(Rc::new)
                .intern(db)
        })
}

pub(in super) fn query_namespaced_rvalue(db: &dyn DeclQuery, nameholders: Vec<Nameholder>, ident: Ident) -> QueryResult<RValue, AnalysisError> {
    db.query_namespace(nameholders)
        .flat_map(|namespace| {
            namespace.get(&ident.value).ok_or(AnalysisError::CannotFindIdent(ident)).into()
        })
}

pub(in super) fn query_namespace(db: &dyn DeclQuery, nameholders: Vec<Nameholder>) -> QueryResult<Rc<Namespace>, AnalysisError> {
    nameholders.into_iter()
        .map(|nameholder| {
            match nameholder {
                Nameholder::Root => db.query_root_namespace().into(),
                Nameholder::Enum(enum_placeholder) => match enum_placeholder {
                    // TODO Current match should return the enum constant id and not the namespace id
                    EnumNameholder::ByEnum(enum_decl) => db.query_enum_namespace(enum_decl),
                    EnumNameholder::ByConstant(enum_constant_id) => {
                        // Clion cannot verify functions generated by proc macros
                        // TODO Add this maybe to every lookup method
                        let enum_constant: EnumConstant = db.lookup_intern_enum_constant(enum_constant_id);
                        db.query_enum_namespace(enum_constant.r#enum)
                    }
                },
                Nameholder::Struct(struct_decl) => db.query_struct_namespace(struct_decl).into(),
                Nameholder::Event(event_decl) => db.query_event_namespace(event_decl),
                Nameholder::Empty => QueryResult::Ok(db.intern_namespace(Rc::new(Namespace::new())))
            }
        })
        .collect::<QueryResult<_, _>>()
        .fold(
            Rc::new(Namespace::new()),
            |acc, item| {
                let namespace: Rc<Namespace> = db.lookup_intern_namespace(item);
                // TODO Do we really need to clone here?
                let mut namespace = (*namespace).clone();
                namespace.parent.push(acc);
                QueryResult::Ok(Rc::new(namespace))
            })
}

pub(in super) fn query_namespaced_type(
    db: &dyn DeclQuery,
    nameholders: Vec<Nameholder>,
    ident: Ident,
) -> QueryResult<im::Type, AnalysisError> {
    db.query_namespaced_rvalue(nameholders, ident)
        .flat_map(|rvalue| match rvalue {
            RValue::Type(r#type) => QueryResult::Ok(r#type),
            RValue::EnumConstant(_) => AnalysisError::WrongType.into(),
            RValue::Property(_) => AnalysisError::WrongType.into(),
            RValue::Function(_) => AnalysisError::WrongType.into() // TODO is this correct?
        })
}

pub(in super) fn query_namespaced_function(
    db: &dyn DeclQuery,
    nameholders: Vec<Nameholder>,
    ident: Ident,
) -> QueryResult<FunctionDecl, AnalysisError> {
    db.query_namespaced_rvalue(nameholders, ident)
        .flat_map(|rvalue| match rvalue {
            RValue::Function(function) => QueryResult::Ok(function),
            RValue::Type(_) | RValue::Property(_) | RValue::EnumConstant(_) => {
                query_error!(AnalysisError::WrongType)
            }
        })
}

pub(in super) fn query_namespaced_event(
    db: &dyn DeclQuery,
    nameholders: Vec<Nameholder>,
    ident: Ident,
) -> QueryResult<EventDeclarationId, AnalysisError> {
    db.query_namespaced_type(nameholders, ident)
        .flat_map(|r#type| match r#type {
            im::Type::Event(event) => QueryResult::Ok(event),
            im::Type::Enum(_) | im::Type::Struct(_) | im::Type::Unit => {
                QueryResult::Err(vec![AnalysisError::WrongType])
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
    Struct(StructDeclarationId),
    Event(EventDeclarationId),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum EnumNameholder {
    ByEnum(EnumDeclarationId),
    ByConstant(EnumConstantId),
}

impl Into<Nameholder> for EnumNameholder {
    fn into(self) -> Nameholder {
        Nameholder::Enum(self)
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct NamespaceId(InternId);

impl_intern_key!(NamespaceId);

#[derive(Clone, Debug, Eq)]
pub struct Namespace {
    parent: Vec<Rc<Namespace>>,
    map: HashMap<ImmutableString, RValue>,
}

impl Namespace {
    fn new() -> Namespace {
        Namespace { map: HashMap::new(), parent: Vec::new() }
    }

    pub fn from_enum_definition(db: &(impl Interner + ?Sized), definition: EnumDefinition) -> Namespace {
        let map = definition.constants.into_iter()
            .map::<(_, EnumConstant), _>(|enum_constant_id| (
                enum_constant_id,
                db.lookup_intern_enum_constant(enum_constant_id)
            ))
            .map(|it| (it.1.name.value.clone(), RValue::EnumConstant(it.0)))
            .collect();
        Namespace { parent: Vec::new(), map }
    }

    fn add<I: Interner + ?Sized>(&mut self, ident: Ident, rvalue: RValue, db: &I) -> Result<(), AnalysisError> {
        match self.contains(&ident) {
            Some(root) => {
                AnalysisError::DuplicateIdent {
                    first: root.name(db), // TODO How to deal with errors?
                    second: ident,
                }.into()
            }
            None => {
                self.map.insert(ident.value.clone(), rvalue);
                Ok(())
            }
        }
    }

    fn get(&self, ident_value: &ImmutableString) -> Option<RValue> {
        self.map.get(ident_value).map(|it| it.clone())
    }

    fn contains(&self, ident: &Ident) -> Option<RValue> {
        let option = self.map.get(&ident.value).map(|it| it.clone());
        if option.is_some() {
            option
        } else {
            self.parent.iter()
                .map(|it| it.contains(&ident))
                .filter(|it| it.is_some())
                .flatten()
                .collect::<Vec<_>>()
                .pop()
        }
    }

    fn sorted_map_entries(&self) -> Vec<(&ImmutableString, &RValue)> {
        let mut content = self.map.iter().collect::<Vec<_>>();
        content.sort_by_key(|it| it.0);
        content
    }
}

impl PartialEq for Namespace {
    fn eq(&self, other: &Self) -> bool {
        self.parent == other.parent && self.sorted_map_entries() == other.sorted_map_entries()
    }
}

impl Hash for Namespace {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.parent.hash(state);
        self.sorted_map_entries().hash(state)
    }
}

impl IntoInternId for Rc<Namespace> {
    type Interned = NamespaceId;

    fn intern<T: Interner + ?Sized>(self, db: &T) -> NamespaceId {
        db.intern_namespace(self)
    }
}
