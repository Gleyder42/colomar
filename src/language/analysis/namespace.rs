use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::process::id;
use std::rc::Rc;
use salsa::InternId;
use crate::impl_intern_key;
use crate::language::{Ident, im, ImmutableString};
use crate::language::analysis::error::AnalysisError;
use crate::language::analysis::file::RootFileQuery;
use crate::language::analysis::interner::{Interner, IntoInternId};
use crate::language::analysis::r#enum::EnumQuery;
use crate::language::analysis::r#type::TypeQuery;
use crate::language::im::{EnumConstant, EnumDeclarationId, EnumDefinition, EventDeclarationId, RValue, StructDeclarationId};

#[salsa::query_group(NamespaceDatabase)]
pub trait NamespaceQuery: TypeQuery + EnumQuery {

    fn query_root_namespace(&self) -> Result<NamespaceId, AnalysisError>;

    fn query_enum_namespace(&self, r#enum: EnumDeclarationId) -> Result<NamespaceId, AnalysisError>;

    fn query_event_namespace(
        &self,
        event_decl: EventDeclarationId
    ) -> Result<NamespaceId, AnalysisError>;

    fn query_struct_namespace(
        &self,
        struct_decl: StructDeclarationId
    ) -> Result<NamespaceId, AnalysisError>;

    fn query_namespaced_rvalue(
        &self,
        namespace_placeholder: NamespacePlaceholder,
        ident: Ident
    ) -> Result<RValue, AnalysisError>;

    fn query_namespaced_type(
        &self,
        namespace_placeholder: NamespacePlaceholder,
        ident: Ident
    ) -> Result<im::Type, AnalysisError>;
}

fn query_namespaced_type(
    db: &dyn NamespaceQuery,
    namespace_placeholder: NamespacePlaceholder,
    ident: Ident
) -> Result<im::Type, AnalysisError> {
    db.query_namespaced_rvalue(namespace_placeholder, ident)
        .map(|it| match it {
            RValue::Type(r#type) => Ok(r#type),
            _ => AnalysisError::WrongType.into()
        }).flatten()
}

fn query_root_namespace(db: &dyn NamespaceQuery) -> Result<NamespaceId, AnalysisError> {
    let mut namespace = Namespace::new_root();

    for (ident, r#type) in db.query_type_map() {
        namespace.add(ident, RValue::Type(r#type), db)?;
    }

    Ok(Rc::new(namespace).intern(db))
}

fn query_enum_namespace(db: &dyn NamespaceQuery, r#enum: EnumDeclarationId) -> Result<NamespaceId, AnalysisError> {
    // TODO Maybe a reference here?
    let r#enum = db.find_enum_def(r#enum);
    let mut namespace = Namespace::new_root();

    for enum_constant_id in r#enum.definition.constants {
        let enum_constant: EnumConstant = db.lookup_intern_enum_constant(enum_constant_id);
        namespace.add(enum_constant.name, RValue::EnumConstant(enum_constant_id), db)?;
    }

    Ok(db.intern_namespace(Rc::new(namespace)))
}

fn query_event_namespace(db: &dyn NamespaceQuery, event_decl: EventDeclarationId) -> Result<NamespaceId, AnalysisError> {
    todo!()
}

fn query_struct_namespace(db: &dyn NamespaceQuery, struct_decl: StructDeclarationId) -> Result<NamespaceId, AnalysisError> {
    todo!()
}

fn query_namespaced_rvalue(db: &dyn NamespaceQuery, namespace_placeholder: NamespacePlaceholder, ident: Ident) -> Result<RValue, AnalysisError> {
    let namespace: NamespaceId = match namespace_placeholder {
        NamespacePlaceholder::Root => db.query_root_namespace()?,
        NamespacePlaceholder::Enum(enum_decl) => db.query_enum_namespace(enum_decl)?,
        NamespacePlaceholder::Struct(struct_decl) => db.query_struct_namespace(struct_decl)?,
        NamespacePlaceholder::Event(event_decl) => db.query_event_namespace(event_decl)?,
        NamespacePlaceholder::Empty => return Err(AnalysisError::CannotFindIdent(ident))
    };
    let namespace: Rc<Namespace> = db.lookup_intern_namespace(namespace);
    namespace.get(&ident).ok_or(AnalysisError::CannotFindIdent(ident))
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NamespacePlaceholder {
    Root,
    Empty,
    Enum(EnumDeclarationId),
    Struct(StructDeclarationId),
    Event(EventDeclarationId)
}

pub struct NamespaceProvider;

impl NamespaceProvider {

    pub fn get(&self) -> Rc<Namespace> {
        todo!()
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct NamespaceId(InternId);

impl_intern_key!(NamespaceId);

#[derive(Debug, Eq)]
pub struct Namespace {
    parent: Vec<Rc<Namespace>>,
    map: HashMap<ImmutableString, RValue>,
}

impl Namespace {
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

impl Namespace {
    fn new_root() -> Namespace {
        Namespace { map: HashMap::new(), parent: Vec::new() }
    }

    fn add<I: Interner + ?Sized>(&mut self, ident: Ident, rvalue: RValue, db: &I) -> Result<(), AnalysisError> {
        match self.contains(&ident) {
            Some(root) => {
                AnalysisError::DuplicateIdent {
                    first: root.name(db), // TODO How to deal with errors?
                    second: ident
                }.into()
            }
            None => {
                self.map.insert(ident.value.clone(), rvalue);
                Ok(())
            }
        }
    }

    fn get(&self, ident: &Ident) -> Option<RValue> {
        self.map.get(&ident.value).map(|it| it.clone())
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
}
