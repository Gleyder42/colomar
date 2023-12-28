use super::super::analysis::def::DefQuery;
use super::super::cst::PathName;
use super::super::{cst, QueryTrisult};
use hashlink::LinkedHashSet;

pub(super) fn load_imports(
    _db: &dyn DefQuery,
    mut _imported: LinkedHashSet<PathName>,
) -> QueryTrisult<Vec<cst::Root>> {
    todo!()
}
