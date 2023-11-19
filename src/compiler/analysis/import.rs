use crate::compiler::analysis::def::DefQuery;
use crate::compiler::cst::Path;
use crate::compiler::{cst, QueryTrisult};
use hashlink::LinkedHashSet;

pub(super) fn load_imports(
    _db: &dyn DefQuery,
    mut _imported: LinkedHashSet<Path>,
) -> QueryTrisult<Vec<cst::Root>> {
    todo!()
}
