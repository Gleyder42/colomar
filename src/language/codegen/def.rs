use crate::language::analysis::def::DefQuery;
use crate::language::analysis::QueryTrisult;
use crate::language::codegen::native_conf;
use crate::language::im::AValue;
use crate::language::lim::{Call, NativeCode};
use crate::language::{im, Text};
use std::collections::BTreeMap;

use super::call;
use super::native;

#[salsa::query_group(LimDefDatabase)]
pub trait LimDefQuery: DefQuery {
    #[salsa::input]
    fn input_native_code(&self) -> Vec<native_conf::Element>;

    /// Impl [call::query_lim_call]
    #[salsa::invoke(call::query_lim_call)]
    fn query_lim_call(&self, avalue_chain: im::AValueChain) -> QueryTrisult<Call>;

    /// Impl [native::query_native_code]
    #[salsa::invoke(native::query_native_code)]
    fn query_native_code(&self, avalue: AValue) -> QueryTrisult<NativeCode>;

    /// Impl [native::query_native_struct_code_map]
    #[salsa::invoke(native::query_native_struct_code_map)]
    fn query_native_struct_code_map(&self) -> BTreeMap<Text, native_conf::Struct>;

    /// Impl [native::query_native_struct_code]
    #[salsa::invoke(native::query_native_struct_code)]
    fn query_native_struct_code(&self, name: Text) -> QueryTrisult<native_conf::Struct>;

    /// Impl [native::query_native_struct_property_code]
    #[salsa::invoke(native::query_native_struct_property_code)]
    fn query_native_struct_property_code(
        &self,
        struct_name: Text,
        property_name: Text,
    ) -> QueryTrisult<NativeCode>;
}
