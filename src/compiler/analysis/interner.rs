use crate::compiler::analysis::namespace::{Namespace, NamespaceId};
use crate::compiler::cir::*;
use crate::compiler::trisult::Trisult;
use crate::compiler::Ident;

use crate::compiler::span::StringInterner;
use std::rc::Rc;

#[salsa::query_group(InternerDatabase)]
pub trait Interner {
    #[salsa::interned]
    fn intern_struct_decl(&self, decl: StructDeclaration) -> StructDeclarationId;

    #[salsa::interned]
    fn intern_event_decl(&self, decl: EventDeclaration) -> EventDeclarationId;

    #[salsa::interned]
    fn intern_enum_decl(&self, decl: EnumDeclaration) -> EnumDeclarationId;

    #[salsa::interned]
    fn intern_decl_arg(&self, decl: DeclaredArgument) -> DeclaredArgumentId;

    #[salsa::interned]
    fn intern_enum_constant(&self, enum_constant: EnumConstant) -> EnumConstantId;

    #[salsa::interned]
    fn intern_property_decl(&self, decl: PropertyDecl) -> PropertyDeclId;

    #[salsa::interned]
    fn intern_function_decl(&self, decl: FunctionDecl) -> FunctionDeclId;

    #[salsa::interned]
    fn intern_namespace(&self, namespace: Rc<Namespace>) -> NamespaceId;

    #[salsa::interned]
    fn intern_called_argument(&self, called_argument: CalledArgument) -> CalledArgumentId;
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

impl IntoInternId for StructDeclaration {
    type Interned = StructDeclarationId;

    fn intern<T: Interner + ?Sized>(self, db: &T) -> StructDeclarationId {
        db.intern_struct_decl(self)
    }
}

impl IntoInternId for EnumDeclaration {
    type Interned = EnumDeclarationId;

    fn intern<T: Interner + ?Sized>(self, db: &T) -> EnumDeclarationId {
        db.intern_enum_decl(self)
    }
}

impl IntoInternId for EnumConstant {
    type Interned = EnumConstantId;

    fn intern<T: Interner + ?Sized>(self, db: &T) -> EnumConstantId {
        db.intern_enum_constant(self)
    }
}

impl IntoInternId for EventDeclaration {
    type Interned = EventDeclarationId;

    fn intern<T: Interner + ?Sized>(self, db: &T) -> EventDeclarationId {
        db.intern_event_decl(self)
    }
}

impl IntoInternId for DeclaredArgument {
    type Interned = DeclaredArgumentId;

    fn intern<T: Interner + ?Sized>(self, db: &T) -> DeclaredArgumentId {
        db.intern_decl_arg(self)
    }
}

impl IntoInternId for CalledArgument {
    type Interned = CalledArgumentId;

    fn intern<T: Interner + ?Sized>(self, db: &T) -> Self::Interned {
        db.intern_called_argument(self)
    }
}

impl RValue {
    pub fn r#type<I: Interner + ?Sized>(&self, db: &I) -> Type {
        match self {
            // What is the type of type?
            RValue::Type(r#type) => *r#type,
            RValue::EnumConstant(enum_constant_id) => {
                let enum_constant: EnumConstant = db.lookup_intern_enum_constant(*enum_constant_id);
                Type::Enum(enum_constant.r#enum)
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
                Type::Enum(r#enum) => db.lookup_intern_enum_decl(r#enum).name,
                Type::Struct(r#struct) => db.lookup_intern_struct_decl(r#struct).name,
                Type::Event(event) => db.lookup_intern_event_decl(event).name,
                Type::Unit => panic!("Unit type has no name"),
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
                r#type: rvalue.r#type(db),
                span: *span,
            },
            AValue::CValue(cvalue) => CalledType {
                r#type: cvalue.r#type(),
                span: cvalue.span(),
            },
            AValue::FunctionCall(function_decl_id, _, span) => {
                let function_decl: FunctionDecl = db.lookup_intern_function_decl(*function_decl_id);
                CalledType {
                    r#type: function_decl.return_type,
                    span: *span,
                }
            }
        }
    }
}

impl Type {
    pub fn name(&self, db: &(impl Interner + StringInterner + ?Sized)) -> String {
        match self {
            Type::Enum(decl_id) => db.lookup_intern_enum_decl(*decl_id).name.value.name(db),
            Type::Struct(decl_id) => db.lookup_intern_struct_decl(*decl_id).name.value.name(db),
            Type::Event(decl_id) => db.lookup_intern_event_decl(*decl_id).name.value.name(db),
            Type::Unit => "Unit".to_owned(),
        }
    }

    pub fn decl_ident(&self, db: &(impl Interner + ?Sized)) -> Option<Ident> {
        match self {
            Type::Enum(r#enum) => Some(db.lookup_intern_enum_decl(*r#enum).name),
            Type::Struct(r#struct) => Some(db.lookup_intern_struct_decl(*r#struct).name),
            Type::Event(event) => Some(db.lookup_intern_event_decl(*event).name),
            Type::Unit => None,
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
