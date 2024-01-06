use super::super::analysis::namespace::{Namespace, NamespaceId};
use super::super::cir::*;
use super::super::trisult::Trisult;
use super::super::Ident;

use super::super::span::StringInterner;
use crate::span::SpanInterner;
use std::rc::Rc;

#[salsa::query_group(InternerDatabase)]
pub trait Interner: StringInterner + SpanInterner {
    #[salsa::interned]
    fn intern_struct_decl(&self, decl: StructDecl) -> StructDeclId;

    #[salsa::interned]
    fn intern_event_decl(&self, decl: EventDecl) -> EventDeclId;

    #[salsa::interned]
    fn intern_enum_decl(&self, decl: EnumDecl) -> EnumDeclId;

    #[salsa::interned]
    fn intern_decl_arg(&self, decl: DeclArg) -> DeclArgId;

    #[salsa::interned]
    fn intern_enum_constant(&self, enum_constant: EnumConstant) -> EnumConstantId;

    #[salsa::interned]
    fn intern_property_decl(&self, decl: PropertyDecl) -> PropertyDeclId;

    #[salsa::interned]
    fn intern_function_decl(&self, decl: FunctionDecl) -> FunctionDeclId;

    #[salsa::interned]
    fn intern_namespace(&self, namespace: Rc<Namespace>) -> NamespaceId;

    #[salsa::interned]
    fn intern_called_arg(&self, called_arg: CalledArg) -> CalledArgId;
}

pub trait IntoInternId {
    type Interned;

    fn intern<T: Interner + ?Sized>(self, db: &T) -> Self::Interned;
}

impl IntoInternId for FunctionDecl {
    type Interned = FunctionDeclId;

    fn intern<T: Interner + ?Sized>(self, db: &T) -> Self::Interned {
        db.intern_function_decl(self)
    }
}

impl IntoInternId for PropertyDecl {
    type Interned = PropertyDeclId;

    fn intern<T: Interner + ?Sized>(self, db: &T) -> Self::Interned {
        db.intern_property_decl(self)
    }
}

impl IntoInternId for StructDecl {
    type Interned = StructDeclId;

    fn intern<T: Interner + ?Sized>(self, db: &T) -> StructDeclId {
        db.intern_struct_decl(self)
    }
}

impl IntoInternId for EnumDecl {
    type Interned = EnumDeclId;

    fn intern<T: Interner + ?Sized>(self, db: &T) -> EnumDeclId {
        db.intern_enum_decl(self)
    }
}

impl IntoInternId for EnumConstant {
    type Interned = EnumConstantId;

    fn intern<T: Interner + ?Sized>(self, db: &T) -> EnumConstantId {
        db.intern_enum_constant(self)
    }
}

impl IntoInternId for EventDecl {
    type Interned = EventDeclId;

    fn intern<T: Interner + ?Sized>(self, db: &T) -> EventDeclId {
        db.intern_event_decl(self)
    }
}

impl IntoInternId for DeclArg {
    type Interned = DeclArgId;

    fn intern<T: Interner + ?Sized>(self, db: &T) -> DeclArgId {
        db.intern_decl_arg(self)
    }
}

impl IntoInternId for CalledArg {
    type Interned = CalledArgId;

    fn intern<T: Interner + ?Sized>(self, db: &T) -> Self::Interned {
        db.intern_called_arg(self)
    }
}

impl RValue {
    pub fn r#type<I: Interner + ?Sized>(&self, db: &I) -> Type {
        match self {
            // What is the type of type?
            RValue::Type(r#type) => (*r#type).into(),
            RValue::EnumConstant(enum_constant_id) => {
                let enum_constant: EnumConstant = db.lookup_intern_enum_constant(*enum_constant_id);
                TypeDesc::Enum(enum_constant.r#enum).into()
            }
            RValue::Property(property_decl_id) => {
                db.lookup_intern_property_decl(*property_decl_id).r#type
            }
            RValue::Function(func_decl_id) => {
                db.lookup_intern_function_decl(*func_decl_id).return_type
            }
        }
    }

    pub fn name<I: Interner + ?Sized>(&self, db: &I) -> Ident {
        match *self {
            RValue::Type(r#type) => match r#type {
                TypeDesc::Enum(r#enum) => db.lookup_intern_enum_decl(r#enum).name,
                TypeDesc::Struct(r#struct) => db.lookup_intern_struct_decl(r#struct).name,
                TypeDesc::Event(event) => db.lookup_intern_event_decl(event).name,
                TypeDesc::Unit => panic!("Unit type has no name"),
            },
            RValue::EnumConstant(enum_constant) => {
                db.lookup_intern_enum_constant(enum_constant).name
            }
            RValue::Property(property_decl_id) => {
                db.lookup_intern_property_decl(property_decl_id).name
            }
            RValue::Function(function_decl_id) => {
                db.lookup_intern_function_decl(function_decl_id).name
            }
        }
    }
}

impl AValue {
    pub fn return_called_type<I: Interner + ?Sized>(&self, db: &I) -> CalledType {
        match self {
            AValue::RValue(rvalue, span) => CalledType {
                r#type: rvalue.r#type(db).into(),
                span: *span,
            },
            AValue::CValue(cvalue) => CalledType {
                r#type: cvalue.r#type().into(),
                span: cvalue.span(),
            },
            AValue::FunctionCall(function_decl_id, _, span) => {
                let function_decl: FunctionDecl = db.lookup_intern_function_decl(*function_decl_id);
                CalledType {
                    r#type: function_decl.return_type.into(),
                    span: *span,
                }
            }
        }
    }
}

impl Type {
    pub fn name(&self, db: &(impl Interner + StringInterner + ?Sized)) -> String {
        self.desc.name(db)
    }
}

impl TypeDesc {
    pub fn name(&self, db: &(impl Interner + StringInterner + ?Sized)) -> String {
        match self {
            TypeDesc::Enum(decl_id) => db.lookup_intern_enum_decl(*decl_id).name.value.name(db),
            TypeDesc::Struct(decl_id) => db.lookup_intern_struct_decl(*decl_id).name.value.name(db),
            TypeDesc::Event(decl_id) => db.lookup_intern_event_decl(*decl_id).name.value.name(db),
            TypeDesc::Unit => "Unit".to_owned(),
        }
    }

    pub fn decl_ident(&self, db: &(impl Interner + ?Sized)) -> Option<Ident> {
        match self {
            TypeDesc::Enum(r#enum) => Some(db.lookup_intern_enum_decl(*r#enum).name),
            TypeDesc::Struct(r#struct) => Some(db.lookup_intern_struct_decl(*r#struct).name),
            TypeDesc::Event(event) => Some(db.lookup_intern_event_decl(*event).name),
            TypeDesc::Unit => None,
        }
    }
}

impl CalledType {
    pub fn name(&self, db: &(impl Interner + StringInterner + ?Sized)) -> String {
        self.r#type.name(db)
    }
}

impl CalledTypes {
    pub fn name(&self, db: &(impl Interner + StringInterner + ?Sized)) -> String {
        self.types
            .iter()
            .map(|it| it.name(db))
            .collect::<Vec<_>>()
            .join(", ")
            .into()
    }
}

impl<Id, T, I, E> Trisult<I, E>
where
    T: IntoInternId<Interned = Id>,
    I: IntoIterator<Item = T>,
{
    /// Available for [Trisult] having an [IntoIterator] as value.
    /// Interns the inner value [T] of iterator [I] to it's interned value [Id]
    pub fn intern_inner<Db, Iu>(self, db: &Db) -> Trisult<Iu, E>
    where
        Db: Interner + ?Sized,
        Iu: IntoIterator<Item = Id> + FromIterator<Id>,
    {
        self.map_inner(|t| t.intern(db))
    }
}

impl<Id, T: IntoInternId<Interned = Id>, E> Trisult<T, E> {
    pub fn intern<Db: Interner + ?Sized>(self, db: &Db) -> Trisult<Id, E> {
        self.map(|it| it.intern(db))
    }
}
