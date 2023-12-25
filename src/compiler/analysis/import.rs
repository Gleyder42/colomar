use super::super::analysis::def::DefQuery;
use super::super::cst::Path;
use super::super::{cst, QueryTrisult};
use hashlink::LinkedHashSet;

pub(super) fn load_imports(
    _db: &dyn DefQuery,
    mut _imported: LinkedHashSet<Path>,
) -> QueryTrisult<Vec<cst::Root>> {
    todo!()
}
