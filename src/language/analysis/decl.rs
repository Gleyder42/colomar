use std::collections::HashMap;
use std::rc::Rc;
use crate::language::analysis::error::{AnalysisError, QueryResult};
use crate::language::{ast, Ident, im};
use crate::language::analysis::interner::Interner;
use crate::language::analysis::namespace::{Nameholder, Namespace, NamespaceId};
use crate::language::ast::PropertyDeclaration;
use crate::language::im::{EnumDeclarationId, EventDeclarationId, PropertyDecl, RValue, StructDeclarationId};

use super::arg;
use super::call;
use super::r#enum;
use super::event;
use super::namespace;
use super::property;
use super::r#struct;
use super::r#type;
use super::function;

#[salsa::query_group(DeclDatabase)]
pub trait DeclQuery: Interner {

    // Input

    #[salsa::input]
    fn input_content(&self) -> ast::Ast;

    // Arg

    #[salsa::invoke(arg::query_declared_arg)]
    fn query_declared_arg(
        &self,
        decl_arg: ast::DeclaredArgument
    ) -> QueryResult<im::DeclaredArgumentId, AnalysisError>;

    #[salsa::invoke(arg::query_declared_args)]
    fn query_declared_args(
        &self,
        decl_args: Vec<ast::DeclaredArgument>
    ) -> QueryResult<Vec<im::DeclaredArgumentId>, AnalysisError>;

    // Call

    #[salsa::invoke(call::query_call_chain)]
    fn query_call_chain(
        &self,
        nameholders: Vec<Nameholder>,
        call_chain: ast::CallChain,
    ) -> QueryResult<im::AValue, AnalysisError>;

    // Enum

    #[salsa::invoke(r#enum::query_enum)]
    fn query_enum(
        &self,
        r#enum: ast::Enum
    ) -> QueryResult<im::Enum, AnalysisError>;

    #[salsa::invoke(r#enum::query_enum_ast_map)]
    fn query_enum_ast_map(
        &self
    ) -> HashMap<EnumDeclarationId, ast::Enum>;

    #[salsa::invoke(r#enum::query_enum_ast)]
    fn query_enum_ast(
        &self,
        enum_decl: EnumDeclarationId
    ) -> Result<ast::Enum, AnalysisError>;

    #[salsa::invoke(r#enum::query_enum_def)]
    fn query_enum_def(
        &self,
        enum_decl: EnumDeclarationId
    ) -> QueryResult<im::Enum, AnalysisError>;

    #[salsa::invoke(r#enum::query_enum_decl)]
    fn query_enum_decl(
        &self,
        r#enum: ast::EnumDeclaration
    ) -> EnumDeclarationId;

    // Event

    #[salsa::invoke(event::query_event_decl)]
    fn query_event_decl(
        &self, event_decl:
        ast::EventDeclaration
    ) -> EventDeclarationId;

    #[salsa::invoke(function::query_function_decl)]
    fn query_function_decl(
        &self,
        function: ast::FunctionDeclaration
    ) -> QueryResult<im::FunctionDecl, AnalysisError>;

    // Namespace

    #[salsa::invoke(namespace::query_root_namespace)]
    fn query_root_namespace(
        &self
    ) -> Result<NamespaceId, AnalysisError>;

    #[salsa::invoke(namespace::query_enum_namespace)]
    fn query_enum_namespace(
        &self,
        r#enum: EnumDeclarationId
    ) -> QueryResult<NamespaceId, AnalysisError>;

    #[salsa::invoke(namespace::query_event_namespace)]
    fn query_event_namespace(
        &self,
        event_decl: EventDeclarationId
    ) -> Result<NamespaceId, AnalysisError>;

    #[salsa::invoke(namespace::query_struct_namespace)]
    fn query_struct_namespace(
        &self,
        struct_decl: StructDeclarationId
    ) -> Result<NamespaceId, AnalysisError>;

    #[salsa::invoke(namespace::query_namespaced_rvalue)]
    fn query_namespaced_rvalue(
        &self,
        nameholders: Vec<Nameholder>,
        ident: Ident,
    ) -> QueryResult<RValue, AnalysisError>;

    #[salsa::invoke(namespace::query_namespace)]
    fn query_namespace(
        &self,
        nameholders: Vec<Nameholder>
    ) -> QueryResult<Rc<Namespace>, AnalysisError>;

    #[salsa::invoke(namespace::query_namespaced_type)]
    fn query_namespaced_type(
        &self,
        nameholders: Vec<Nameholder>,
        ident: Ident,
    ) -> QueryResult<im::Type, AnalysisError>;

    #[salsa::invoke(namespace::query_namespaced_event)]
    fn query_namespaced_event(
        &self,
        nameholders: Vec<Nameholder>,
        ident: Ident,
    ) -> QueryResult<EventDeclarationId, AnalysisError>;

    // Property

    #[salsa::invoke(property::query_property)]
    fn query_property(
        &self,
        property_decl: PropertyDeclaration
    ) -> QueryResult<PropertyDecl, AnalysisError>;

    // Struct

    #[salsa::invoke(r#struct::query_struct_decl)]
    fn query_struct_decl(
        &self,
        r#struct: ast::StructDeclaration
    ) -> StructDeclarationId;

    // Type

    /// Queries the type map.
    /// If you want to find a type by ident, use [query_type] instead.
    #[salsa::invoke(r#type::query_type_map)]
    fn query_type_map(&self) -> HashMap<Ident, im::Type>;
}