use std::rc::Rc;
use crate::language::analysis::namespace::{Namespace, NamespaceId};
use crate::language::im;
use crate::language::im::{DeclaredArgument, DeclaredArgumentId, EnumConstant, EnumConstantId, EnumDeclaration, EnumDeclarationId, EventDeclaration, EventDeclarationId, StructDeclaration, StructDeclarationId};

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
    fn intern_namespace(&self, namespace: Rc<Namespace>) -> NamespaceId;
}

pub trait IntoInternId {
    type Interned;

    fn intern<T: Interner + ?Sized>(self, db: &T) -> Self::Interned;
}

