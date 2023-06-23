use crate::language::analysis::def::DefQuery;
use crate::language::analysis::QueryTrisult;
use crate::language::codegen::{owscript_impl, HashableMap};
use crate::language::im::{AValue, PropertyDecl};
use crate::language::lim::{Call, LiteralOwscript, Owscript};
use crate::language::{im, Text};

use super::call;
use super::native;

#[salsa::query_group(LimDefDatabase)]
pub trait LimDefQuery: DefQuery {
    #[salsa::input]
    fn input_owscript_impls(&self) -> Vec<owscript_impl::Element>;

    /// Impl [call::query_lim_call]
    #[salsa::invoke(call::query_lim_call)]
    fn query_lim_call(&self, avalue_chain: im::AValueChain) -> QueryTrisult<Call>;

    /// Impl [native::query_native_code]
    #[salsa::invoke(native::query_native_code)]
    fn query_native_code(&self, avalue: AValue) -> QueryTrisult<Owscript>;

    /// Impl [native::query_owscript_event_impl]
    #[salsa::invoke(native::query_owscript_event_impl)]
    fn query_owscript_event_impl(&self, name: Text) -> QueryTrisult<owscript_impl::Event>;

    /// Impl [native::query_owscript_enum_impl]
    #[salsa::invoke(native::query_owscript_enum_impl)]
    fn query_owscript_enum_impl(&self, name: Text) -> QueryTrisult<owscript_impl::Enum>;

    /// Impl [native::query_owscript_struct_impl]
    #[salsa::invoke(native::query_owscript_struct_impl)]
    fn query_owscript_struct_impl(&self, name: Text) -> QueryTrisult<owscript_impl::Struct>;

    /// Impl [native::query_owscript_event_context_variable_impl]
    #[salsa::invoke(native::query_owscript_event_context_variable_impl)]
    fn query_owscript_event_context_variable_impl(
        &self,
        struct_name: Text,
        property_name: Text,
    ) -> QueryTrisult<Owscript>;

    /// Impl [native::query_owscript_struct_property_impl]
    #[salsa::invoke(native::query_owscript_struct_property_impl)]
    fn query_owscript_struct_property_impl(
        &self,
        struct_name: Text,
        property_name: Text,
    ) -> QueryTrisult<Owscript>;

    /// Impl [native::query_owscript_property_impl]
    #[salsa::invoke(native::query_owscript_property_impl)]
    fn query_owscript_property_impl(&self, property_decl: PropertyDecl) -> QueryTrisult<Owscript>;

    /// Impl: [native::query_owscript_struct_impls]
    #[salsa::invoke(native::query_owscript_struct_impls)]
    fn query_owscript_struct_impls(&self) -> HashableMap<Text, owscript_impl::Struct>;

    /// Impl: [native::query_owscript_event_impls]
    #[salsa::invoke(native::query_owscript_event_impls)]
    fn query_owscript_event_impls(&self) -> HashableMap<Text, owscript_impl::Event>;

    /// Impl: [native::query_owscript_enum_impls]
    #[salsa::invoke(native::query_owscript_enum_impls)]
    fn query_owscript_enum_impls(&self) -> HashableMap<Text, owscript_impl::Enum>;
}
