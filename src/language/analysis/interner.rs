use crate::language::analysis::namespace::{Namespace, NamespaceId};
use crate::language::im::{
    CalledArgument, CalledArgumentId, DeclaredArgument, DeclaredArgumentId, EnumConstant,
    EnumConstantId, EnumDeclaration, EnumDeclarationId, EventDeclaration, EventDeclarationId,
    FunctionDecl, FunctionDeclId, PropertyDecl, PropertyDeclId, StructDeclaration,
    StructDeclarationId,
};
use crate::language::{SpanSource, SpanSourceId};
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

    #[salsa::interned]
    fn intern_span_source(&self, span_source: SpanSource) -> SpanSourceId;
}

pub trait IntoInternId {
    type Interned;

    fn intern<T: Interner + ?Sized>(self, db: &T) -> Self::Interned;
}
