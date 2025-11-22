// Database trait for salsa 0.24
pub trait Db: salsa::Database {}

#[salsa::db]
#[derive(Default, Clone)]
pub struct CompilerDatabase {
    storage: salsa::Storage<Self>,
}

#[salsa::db]
impl salsa::Database for CompilerDatabase {}

impl Db for CompilerDatabase {}
