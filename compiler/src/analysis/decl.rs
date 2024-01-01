use super::super::analysis::file::DefKey;
use super::super::analysis::interner::Interner;
use super::super::analysis::namespace::{Nameholders, Namespace, NamespaceId};
use super::super::cir::{
    AValueChain, CalledArgs, DeclArgIds, EnumDeclId, EventDeclId, FunctionDeclIds, PropertyDeclIds,
    PropertyDecls, StructDeclId,
};
use super::super::cst::{Actions, TypeRoot};
use super::super::{cir, cst, Ident, QueryTrisult, SVMultiMap, StructId, TextId};

use super::super::span::Spanned;
use crate::cst::{Def, Root};
use cir::DeclArgId;
use cst::Cst;
use hashlink::LinkedHashMap;
use smallvec::SmallVec;
use std::collections::HashMap;
use std::rc::Rc;

use super::arg;
use super::call;
use super::event;
use super::file;
use super::function;
use super::namespace;
use super::property;
use super::r#enum as eenum;
use super::r#struct as sstruct;
use super::r#type as ttype;

#[salsa::query_group(DeclDatabase)]
pub trait DeclQuery: Interner {
    #[salsa::input]
    fn main_file_name(&self) -> cst::PathName;

    #[salsa::input]
    fn secondary_files(&self) -> LinkedHashMap<cst::PathName, Cst>;

    /// Impl [file::query_secondary_file]
    #[salsa::invoke(file::query_secondary_file)]
    fn query_secondary_file(&self, path: cst::Path) -> QueryTrisult<Cst>;

    /// Impl [file::query_main_file]
    #[salsa::invoke(file::query_main_file)]
    fn query_main_file(&self) -> QueryTrisult<Cst>;

    // Ast

    /// Impl [file::query_file]
    #[salsa::invoke(file::query_file)]
    fn query_file(&self, path: cst::Path, include_only_public: bool) -> QueryTrisult<Cst>;

    /// Impl [file::query_structs]
    #[salsa::invoke(file::query_structs)]
    fn query_structs(&self) -> QueryTrisult<HashMap<TextId, SmallVec<[cst::Struct; 1]>>>;

    /// Impl [file::query_struct_by_name]
    #[salsa::invoke(file::query_struct_by_name)]
    fn query_struct_by_name(&self, text: TextId) -> QueryTrisult<SmallVec<[cst::Struct; 1]>>;

    /// Impl: [file::query_type_items]
    #[salsa::invoke(file::query_type_items)]
    fn query_type_items(&self) -> QueryTrisult<Vec<TypeRoot>>;

    /// Impl: [file::query_action_items]
    #[salsa::invoke(file::query_action_items)]
    fn query_action_items(&self) -> QueryTrisult<Vec<Root>>;

    /// Queries a map containing declaration ids and definitions.
    /// If you want to get the definition by declaration id use [AstDefQuery::query_ast_event_def]
    /// instead
    /// Impl: [file::query_ast_def_map]
    #[salsa::invoke(file::query_ast_def_map)]
    fn query_ast_def_map(&self) -> QueryTrisult<SVMultiMap<DefKey, Def, 1>>;

    /// Impl: [file::query_ast_event_def_map]
    #[salsa::invoke(file::query_ast_event_def_map)]
    fn query_ast_event_def_map(&self) -> QueryTrisult<HashMap<DefKey, cst::EventDef>>;

    /// Impl: [file::query_ast_struct_def_map]
    #[salsa::invoke(file::query_ast_struct_def_map)]
    fn query_ast_struct_def_map(&self) -> QueryTrisult<SVMultiMap<DefKey, cst::StructDef, 1>>;

    /// Queries an event definition my even declaration id
    /// Impl: [file::query_ast_event_def]
    #[salsa::invoke(file::query_ast_event_def)]
    fn query_ast_event_def(&self, event_decl_id: EventDeclId) -> QueryTrisult<cst::EventDef>;

    /// Impl: [file::query_ast_struct_def]
    #[salsa::invoke(file::query_ast_struct_def)]
    fn query_ast_struct_def(
        &self,
        struct_id: StructId,
    ) -> QueryTrisult<SmallVec<[cst::StructDef; 1]>>;

    // Arg

    /// [arg::query_called_args]
    #[salsa::invoke(arg::query_called_args)]
    fn query_called_args(
        &self,
        called_arg_avalue_chain: Spanned<Vec<(Option<Ident>, AValueChain)>>,
        decl_arg_ids: DeclArgIds,
    ) -> QueryTrisult<CalledArgs>;

    /// [arg::query_declared_arg]
    #[salsa::invoke(arg::query_declared_arg)]
    fn query_declared_arg(&self, decl_arg: cst::DeclArg) -> QueryTrisult<DeclArgId>;

    /// [arg::query_declared_args]
    #[salsa::invoke(arg::query_declared_args)]
    fn query_declared_args(&self, decl_args: cst::DeclArgs) -> QueryTrisult<DeclArgIds>;

    // Call

    /// Impl: [call::checked_return_avalue]
    #[salsa::invoke(call::checked_return_avalue)]
    fn checked_return_avalue(&self, expr: cir::Expr) -> QueryTrisult<cir::AValue>;

    /// Impl: [call::check_equal_return_avalue]
    #[salsa::invoke(call::check_equal_return_avalue)]
    fn check_equal_return_avalue(
        &self,
        lhs: cir::AValue,
        rhs: cir::AValue,
    ) -> QueryTrisult<cir::AValue>;
    /// Impl: [call::query_expr]
    #[salsa::invoke(call::query_expr)]
    fn query_expr(
        &self,
        inital_nameholders: Nameholders,
        enforce_bool: bool,
        expr: cst::Expr,
    ) -> QueryTrisult<cir::Expr>;

    /// Impl: [call::query_call_chain]
    #[salsa::invoke(call::query_call_chain)]
    fn query_call_chain(
        &self,
        nameholders: Nameholders,
        call_chain: cst::CallChain,
    ) -> QueryTrisult<AValueChain>;

    // Enum

    /// Impl: [eenum::query_enum]
    #[salsa::invoke(eenum::query_enum)]
    fn query_enum(&self, r#enum: cst::Enum) -> QueryTrisult<cir::Enum>;

    /// Impl: [eenum::query_enum_ast_map]
    #[salsa::invoke(eenum::query_enum_ast_map)]
    fn query_enum_ast_map(&self) -> QueryTrisult<HashMap<EnumDeclId, cst::Enum>>;

    /// Impl: [eenum::query_enum_ast]
    #[salsa::invoke(eenum::query_enum_ast)]
    fn query_enum_ast(&self, enum_decl: EnumDeclId) -> QueryTrisult<cst::Enum>;

    /// Impl: [eenum::query_enum_def]
    #[salsa::invoke(eenum::query_enum_def)]
    fn query_enum_def(&self, enum_decl: EnumDeclId) -> QueryTrisult<cir::Enum>;

    /// Impl: [eenum::query_enum_decl]
    #[salsa::invoke(eenum::query_enum_decl)]
    fn query_enum_decl(&self, r#enum: cst::EnumDecl) -> EnumDeclId;

    // Event

    /// [event::query_event_properties]
    #[salsa::invoke(event::query_event_properties)]
    fn query_event_properties(
        self,
        event_decl_id: EventDeclId,
        actions: Actions,
    ) -> QueryTrisult<cir::PropertyDecls>;

    /// [event::query_event_decl]
    #[salsa::invoke(event::query_event_decl)]
    fn query_event_decl(&self, event_decl: cst::EventDecl) -> EventDeclId;

    /// [function::query_function_decl]
    #[salsa::invoke(function::query_function_decl)]
    fn query_function_decl(
        &self,
        instance: Option<cir::Type>,
        function: cst::FunctionDecl,
    ) -> QueryTrisult<cir::FunctionDecl>;

    /// [event::query_event_context_variables]
    #[salsa::invoke(event::query_event_context_variables)]
    fn query_event_context_variables(&self, event_decl: EventDeclId)
        -> QueryTrisult<PropertyDecls>;

    // Namespace

    /// [namespace::query_bool_name]
    #[salsa::invoke(namespace::query_bool_name)]
    fn query_bool_name(&self) -> TextId;

    /// [namespace::query_string_name]
    #[salsa::invoke(namespace::query_string_name)]
    fn query_string_name(&self) -> TextId;

    /// [namespace::query_player_name]
    #[salsa::invoke(namespace::query_player_name)]
    fn query_player_name(&self) -> TextId;

    /// [namespace::query_number_name]
    #[salsa::invoke(namespace::query_number_name)]
    fn query_number_name(&self) -> TextId;

    /// [namespace::query_primitives]
    #[salsa::invoke(namespace::query_primitives)]
    fn query_primitives(&self) -> QueryTrisult<HashMap<TextId, cir::Type>>;

    /// [namespace::query_bool_type]
    #[salsa::invoke(namespace::query_bool_type)]
    fn query_bool_type(&self) -> QueryTrisult<StructDeclId>;

    /// [namespace::query_string_type]
    #[salsa::invoke(namespace::query_string_type)]
    fn query_string_type(&self) -> QueryTrisult<StructDeclId>;

    /// [namespace::query_number_type]
    #[salsa::invoke(namespace::query_number_type)]
    fn query_number_type(&self) -> QueryTrisult<StructDeclId>;

    /// [namespace::query_player_type]
    #[salsa::invoke(namespace::query_player_type)]
    fn query_player_type(&self) -> QueryTrisult<StructDeclId>;

    /// Impl: [namespace::query_root_namespace]
    #[salsa::invoke(namespace::query_root_namespace)]
    fn query_root_namespace(&self) -> QueryTrisult<NamespaceId>;

    /// [namespace::query_enum_namespace]
    #[salsa::invoke(namespace::query_enum_namespace)]
    fn query_enum_namespace(&self, r#enum: EnumDeclId) -> QueryTrisult<NamespaceId>;

    /// [namespace::query_event_namespace]
    #[salsa::invoke(namespace::query_event_namespace)]
    fn query_event_namespace(&self, event_decl: EventDeclId) -> QueryTrisult<NamespaceId>;

    /// [namespace::query_struct_namespace]
    #[salsa::invoke(namespace::query_struct_namespace)]
    fn query_struct_namespace(&self, struct_decl: StructDeclId) -> QueryTrisult<NamespaceId>;

    /// [namespace::query_namespaced_rvalue]
    #[salsa::invoke(namespace::query_namespaced_rvalue)]
    fn query_namespaced_rvalue(
        &self,
        nameholders: Nameholders,
        ident: Ident,
    ) -> QueryTrisult<cir::RValue>;

    /// [namespace::query_namespace]
    #[salsa::invoke(namespace::query_namespace)]
    fn query_namespace(&self, nameholders: Nameholders) -> QueryTrisult<Rc<Namespace>>;

    /// [namespace::query_namespaced_type]
    #[salsa::invoke(namespace::query_namespaced_type)]
    fn query_namespaced_type(
        &self,
        nameholders: Nameholders,
        ident: Ident,
    ) -> QueryTrisult<cir::Type>;

    /// Impl [namespace::query_namespaced_function]
    #[salsa::invoke(namespace::query_namespaced_function)]
    fn query_namespaced_function(
        &self,
        nameholders: Nameholders,
        ident: Ident,
    ) -> QueryTrisult<cir::FunctionDecl>;

    /// Impl [namespace::query_namespaced_event]
    #[salsa::invoke(namespace::query_namespaced_event)]
    fn query_namespaced_event(
        &self,
        nameholders: Nameholders,
        ident: Ident,
    ) -> QueryTrisult<EventDeclId>;

    // Property

    /// Impl [property::query_property]
    #[salsa::invoke(property::query_property)]
    fn query_property(
        &self,
        instance: Option<cir::Type>,
        property_decl: cst::PropertyDecl,
    ) -> QueryTrisult<cir::PropertyDecl>;

    // Struct

    /// [sstruct::query_struct_functions]
    #[salsa::invoke(sstruct::query_struct_functions)]
    fn query_struct_functions(
        &self,
        struct_decl_id: StructDeclId,
        functions: cst::FunctionDecls,
    ) -> QueryTrisult<FunctionDeclIds>;

    /// [sstruct::query_struct_properties]
    #[salsa::invoke(sstruct::query_struct_properties)]
    fn query_struct_properties(
        &self,
        struct_decl_id: StructDeclId,
        properties: cst::PropertyDecls,
    ) -> QueryTrisult<PropertyDeclIds>;

    /// [sstruct::query_struct_decl]
    #[salsa::invoke(sstruct::query_struct_decl)]
    fn query_struct_decl(&self, r#struct: cst::StructDecl) -> StructDeclId;

    // Type

    /// Queries the type map.
    /// If you want to find a type by ident, use [query_type] instead.
    /// [ttype::query_type_map]
    #[salsa::invoke(ttype::query_type_map)]
    fn query_type_map(&self) -> QueryTrisult<HashMap<Ident, cir::Type>>;

    // TODO Move this into the namespace file
    /// Impl [ttype::query_namespaced_type2]
    #[salsa::invoke(ttype::query_namespaced_type2)]
    fn query_namespaced_type2(
        &self,
        nameholders: Nameholders,
        r#type: cst::Type,
    ) -> QueryTrisult<cir::VirtualType>;

    /// Impl [ttype::resolve_generics]
    #[salsa::invoke(ttype::resolve_generics)]
    fn resolve_generics(
        &self,
        nameholders: Nameholders,
        generics: Vec<cst::BoundGeneric>,
    ) -> QueryTrisult<Vec<cir::BoundGeneric>>;
}
