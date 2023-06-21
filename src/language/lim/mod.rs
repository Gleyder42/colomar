mod call;
mod def;
mod function;
mod tree;
mod native;
mod hashable_map;

use crate::language::analysis::def::DefQuery;
use crate::language::analysis::QueryTrisult;

#[salsa::database()]
#[derive(Default)]
pub struct AnalysisDatabase {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for AnalysisDatabase {}
