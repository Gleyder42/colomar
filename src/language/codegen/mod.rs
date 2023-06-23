mod call;
pub mod def;
mod native;
mod owscript_impl;

use crate::language::analysis::def::DefQuery;
use crate::language::analysis::QueryTrisult;
use std::collections::BTreeMap;

#[salsa::database()]
#[derive(Default)]
pub struct AnalysisDatabase {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for AnalysisDatabase {}

pub type HashableMap<K, V> = BTreeMap<K, V>;
