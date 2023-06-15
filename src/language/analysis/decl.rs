use crate::language::analysis::file::DefKey;
use crate::language::analysis::interner::Interner;
use crate::language::analysis::namespace::{Nameholders, Namespace, NamespaceId};
use crate::language::analysis::{AnalysisError, QueryTrisult};
use crate::language::ast::Actions;
use crate::language::im::{
    AValueChain, CalledArguments, DeclaredArgumentIds, EnumDeclarationId, EventDeclarationId,
    FunctionDeclIds, PropertyDeclIds, PropertyDecls, StructDeclarationId,
};
use crate::language::{ast, im, Ident, ImmutableString};
use ast::Ast;
use im::{AValue, DeclaredArgumentId};
use std::collections::HashMap;
use std::rc::Rc;

use super::arg;
use super::call;
use super::event;
use super::file;
use super::function;
use super::namespace;
use super::property;
use super::r#enum;
use super::r#struct;
use super::r#type;

#[salsa::query_group(DeclDatabase)]
pub trait DeclQuery: Interner {
    // Input

    #[salsa::input]
    fn input_content(&self) -> Ast;

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
        event_decl_id: EventDeclarationId,
    ) -> QueryTrisult<ast::EventDefinition>;

    #[salsa::invoke(file::query_ast_struct_def)]
    fn query_ast_struct_def(
        &self,
        struct_decl_id: StructDeclarationId,
    ) -> QueryTrisult<ast::StructDefinition>;

    // Arg

    /// [arg::query_called_args_by_chain]
    #[salsa::invoke(arg::query_called_args_by_chain)]
    fn query_called_args_by_chain(
        &self,
        called_avalue_chains: Vec<AValueChain>,
        decl_arg_ids: DeclaredArgumentIds,
    ) -> QueryTrisult<CalledArguments>;

    /// [arg::query_called_args]
    #[salsa::invoke(arg::query_called_args)]
    fn query_called_args(
        &self,
        called_arg_avalues: Vec<AValue>,
        decl_arg_ids: DeclaredArgumentIds,
    ) -> QueryTrisult<CalledArguments>;

    #[salsa::invoke(arg::query_declared_arg)]
    fn query_declared_arg(
        &self,
        decl_arg: ast::DeclaredArgument,
    ) -> QueryTrisult<DeclaredArgumentId>;

    #[salsa::invoke(arg::query_declared_args)]
    fn query_declared_args(
        &self,
        decl_args: ast::DeclaredArguments,
    ) -> QueryTrisult<DeclaredArgumentIds>;

    // Call

    /// Impl: [call::query_call_chain]
    #[salsa::invoke(call::query_call_chain)]
    fn query_call_chain(
        &self,
        nameholders: Nameholders,
        call_chain: ast::CallChain,
    ) -> QueryTrisult<AValueChain>;

    // Enum

    #[salsa::invoke(r#enum::query_enum)]
    fn query_enum(&self, r#enum: ast::Enum) -> QueryTrisult<im::Enum>;

    #[salsa::invoke(r#enum::query_enum_ast_map)]
    fn query_enum_ast_map(&self) -> HashMap<EnumDeclarationId, ast::Enum>;

    #[salsa::invoke(r#enum::query_enum_ast)]
    fn query_enum_ast(&self, enum_decl: EnumDeclarationId) -> Result<ast::Enum, AnalysisError>;

    #[salsa::invoke(r#enum::query_enum_def)]
    fn query_enum_def(&self, enum_decl: EnumDeclarationId) -> QueryTrisult<im::Enum>;

    #[salsa::invoke(r#enum::query_enum_decl)]
    fn query_enum_decl(&self, r#enum: ast::EnumDeclaration) -> EnumDeclarationId;

    // Event

    #[salsa::invoke(event::query_event_properties)]
    fn query_event_properties(self, actions: Actions) -> QueryTrisult<im::PropertyDecls>;

    #[salsa::invoke(event::query_event_decl)]
    fn query_event_decl(&self, event_decl: ast::EventDeclaration) -> EventDeclarationId;

    #[salsa::invoke(function::query_function_decl)]
    fn query_function_decl(
        &self,
        function: ast::FunctionDeclaration,
    ) -> QueryTrisult<im::FunctionDecl>;

    #[salsa::invoke(event::query_event_context_variables)]
    fn query_event_context_variables(
        &self,
        event_decl: EventDeclarationId,
    ) -> QueryTrisult<PropertyDecls>;

    // Namespace

    #[salsa::invoke(namespace::query_bool_name)]
    fn query_bool_name(&self) -> ImmutableString;

    #[salsa::invoke(namespace::query_string_name)]
    fn query_string_name(&self) -> ImmutableString;

    #[salsa::invoke(namespace::query_number_name)]
    fn query_number_name(&self) -> ImmutableString;

    #[salsa::invoke(namespace::query_primitives)]
    fn query_primitives(&self) -> QueryTrisult<HashMap<ImmutableString, im::Type>>;

    #[salsa::invoke(namespace::query_bool_type)]
    fn query_bool_type(&self) -> QueryTrisult<StructDeclarationId>;

    #[salsa::invoke(namespace::query_string_type)]
    fn query_string_type(&self) -> QueryTrisult<StructDeclarationId>;

    #[salsa::invoke(namespace::query_number_type)]
    fn query_number_type(&self) -> QueryTrisult<StructDeclarationId>;

    #[salsa::invoke(namespace::query_root_namespace)]
    fn query_root_namespace(&self) -> QueryTrisult<NamespaceId>;

    #[salsa::invoke(namespace::query_enum_namespace)]
    fn query_enum_namespace(&self, r#enum: EnumDeclarationId) -> QueryTrisult<NamespaceId>;

    #[salsa::invoke(namespace::query_event_namespace)]
    fn query_event_namespace(&self, event_decl: EventDeclarationId) -> QueryTrisult<NamespaceId>;

    #[salsa::invoke(namespace::query_struct_namespace)]
    fn query_struct_namespace(&self, struct_decl: StructDeclarationId)
        -> QueryTrisult<NamespaceId>;

    #[salsa::invoke(namespace::query_namespaced_rvalue)]
    fn query_namespaced_rvalue(
        &self,
        nameholders: Nameholders,
        ident: Ident,
    ) -> QueryTrisult<im::RValue>;

    #[salsa::invoke(namespace::query_namespace)]
    fn query_namespace(&self, nameholders: Nameholders) -> QueryTrisult<Rc<Namespace>>;

    #[salsa::invoke(namespace::query_namespaced_type)]
    fn query_namespaced_type(
        &self,
        nameholders: Nameholders,
        ident: Ident,
    ) -> QueryTrisult<im::Type>;

    /// [namespace::query_namespaced_function]
    #[salsa::invoke(namespace::query_namespaced_function)]
    fn query_namespaced_function(
        &self,
        nameholders: Nameholders,
        ident: Ident,
    ) -> QueryTrisult<im::FunctionDecl>;

    #[salsa::invoke(namespace::query_namespaced_event)]
    fn query_namespaced_event(
        &self,
        nameholders: Nameholders,
        ident: Ident,
    ) -> QueryTrisult<EventDeclarationId>;

    // Property

    #[salsa::invoke(property::query_property)]
    fn query_property(
        &self,
        property_decl: ast::PropertyDeclaration,
    ) -> QueryTrisult<im::PropertyDecl>;

    // Struct

    #[salsa::invoke(r#struct::query_struct_functions)]
    fn query_struct_functions(
        &self,
        functions: ast::FunctionDecls,
    ) -> QueryTrisult<FunctionDeclIds>;

    #[salsa::invoke(r#struct::query_struct_properties)]
    fn query_struct_properties(
        &self,
        properties: ast::PropertyDecls,
    ) -> QueryTrisult<PropertyDeclIds>;

    #[salsa::invoke(r#struct::query_struct_decl)]
    fn query_struct_decl(&self, r#struct: ast::StructDeclaration) -> StructDeclarationId;

    // Type

    /// Queries the type map.
    /// If you want to find a type by ident, use [query_type] instead.
    #[salsa::invoke(r#type::query_type_map)]
    fn query_type_map(&self) -> HashMap<Ident, im::Type>;
}
