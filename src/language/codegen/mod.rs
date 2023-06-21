mod call;
mod def;
mod native_conf;
mod native;

use crate::language::analysis::def::DefQuery;
use crate::language::analysis::QueryTrisult;

#[salsa::database()]
#[derive(Default)]
pub struct AnalysisDatabase {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for AnalysisDatabase {}
