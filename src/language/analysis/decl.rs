use std::collections::HashMap;
use std::rc::Rc;
use crate::language::analysis::error::{AnalysisError, QueryResult};
use crate::language::{ast, Ident, im, ImmutableString};
use crate::language::analysis::file::DefKey;
use crate::language::analysis::interner::Interner;
use crate::language::analysis::namespace::{Nameholder, Namespace, NamespaceId};
use crate::language::ast::{Action, PropertyDeclaration};
use crate::language::im::{EnumDeclarationId, EventDeclarationId, FunctionDecl, FunctionDeclId, PropertyDecl, PropertyDeclId, RValue, StructDeclarationId, Type};

use super::arg;
use super::call;
use super::r#enum;
use super::event;
use super::namespace;
use super::property;
use super::r#struct;
use super::r#type;
use super::function;
use super::file;

#[salsa::query_group(DeclDatabase)]
pub trait DeclQuery: Interner {

    // Input

    #[salsa::input]
    fn input_content(&self) -> ast::Ast;

    // Ast

    /// Queries a map containing declaration ids and definitions.
    /// If you want to get the definition by declaration id use [AstDefQuery::query_ast_event_def]
    /// instead
    #[salsa::invoke(file::query_ast_def_map)]
    fn query_ast_def_map(&self) -> HashMap<DefKey, ast::Definition>;

    #[salsa::invoke(file::query_ast_event_def_map)]
    fn query_ast_event_def_map(&self) -> HashMap<DefKey, ast::EventDefinition>;

    #[salsa::invoke(file::query_ast_struct_def_map)]
    fn query_ast_struct_def_map(&self) -> HashMap<DefKey, ast::StructDefinition>;

    /// Queries an event definition my even declaration id
    #[salsa::invoke(file::query_ast_event_def)]
    fn query_ast_event_def(
        &self,
        event_decl_id: EventDeclarationId
    ) -> QueryResult<ast::EventDefinition, AnalysisError>;

    #[salsa::invoke(file::query_ast_struct_def)]
    fn query_ast_struct_def(
        &self,
        struct_decl_id: StructDeclarationId
    ) -> QueryResult<ast::StructDefinition, AnalysisError>;

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

    #[salsa::invoke(event::query_event_properties)]
    fn query_event_properties(
        self,
        actions: Vec<Action>
    ) -> QueryResult<Vec<PropertyDecl>, AnalysisError>;

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

    #[salsa::invoke(event::query_event_context_variables)]
    fn query_event_context_variables(
        &self,
        event_decl: EventDeclarationId
    ) -> QueryResult<Vec<PropertyDecl>, AnalysisError>;


    // Namespace

    #[salsa::invoke(namespace::query_primitives)]
    fn query_primitives(&self) -> QueryResult<HashMap<ImmutableString, Type>, AnalysisError>;

    #[salsa::invoke(namespace::query_bool_type)]
    fn query_bool_type(&self) -> QueryResult<StructDeclarationId, AnalysisError>;

    #[salsa::invoke(namespace::query_string_type)]
    fn query_string_type(&self) -> QueryResult<StructDeclarationId, AnalysisError>;

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
    ) -> QueryResult<NamespaceId, AnalysisError>;

    #[salsa::invoke(namespace::query_struct_namespace)]
    fn query_struct_namespace(
        &self,
        struct_decl: StructDeclarationId
    ) -> QueryResult<NamespaceId, AnalysisError>;

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

    #[salsa::invoke(namespace::query_namespaced_function)]
    fn query_namespaced_function(
        &self,
        nameholders: Vec<Nameholder>,
        ident: Ident
    ) -> QueryResult<FunctionDecl, AnalysisError>;

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
        property_decl: ast::PropertyDeclaration
    ) -> QueryResult<PropertyDecl, AnalysisError>;

    // Struct

    #[salsa::invoke(r#struct::query_struct_functions)]
    fn query_struct_functions(
        &self,
        functions: Vec<ast::FunctionDeclaration>
    ) -> QueryResult<Vec<FunctionDeclId>, AnalysisError>;

    #[salsa::invoke(r#struct::query_struct_properties)]
    fn query_struct_properties(
        &self,
        properties: Vec<ast::PropertyDeclaration>
    ) -> QueryResult<Vec<PropertyDeclId>, AnalysisError>;

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