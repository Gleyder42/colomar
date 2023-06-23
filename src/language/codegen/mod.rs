mod call;
pub mod def;
mod native;
mod owscript_impl;

use std::collections::BTreeMap;

#[salsa::database()]
#[derive(Default)]
pub struct AnalysisDatabase {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for AnalysisDatabase {}

pub type HashableMap<K, V> = BTreeMap<K, V>;
