use crate::compiler::analysis::def::DefQuery;
use crate::compiler::cst::{Ast, Import, Path};
use crate::compiler::{cst, QueryTrisult};
use hashlink::LinkedHashSet;

pub(super) fn load_imports(
    db: &dyn DefQuery,
    mut imported: LinkedHashSet<Path>,
) -> QueryTrisult<Vec<cst::Root>> {
    todo!()
}
