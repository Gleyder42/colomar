use file::FileDatabase;
use file::AstDefDatabase;
use r#enum::EnumDatabase;
use r#enum::EnumDeclarationDatabase;
use im::ImDatabase;
use arg::ArgDatabase;
use call::CallDatabase;
use event::EventDatabase;
use event::EventDeclarationDatabase;
use namespace::NamespaceDatabase;
use rule::RuleDeclDatabase;
use r#struct::StructDefDatabase;
use r#struct::StructDeclDatabase;
use r#struct::StructDatabase;
use r#type::TypeDatabase;
use interner::InternerDatabase;
use property::PropertyDatabase;
use function::FunctionDatabase;

pub mod r#enum;
pub mod file;
pub mod interner;
pub mod event;
pub mod arg;
pub mod call;
pub mod namespace;
pub mod r#type;
pub mod im;
pub mod r#struct;
pub mod error;
pub mod rule;
pub mod property;
pub mod function;

#[salsa::database(
    InternerDatabase,
    FileDatabase, AstDefDatabase,
    EnumDatabase, EnumDeclarationDatabase,
    ImDatabase,
    ArgDatabase,
    CallDatabase,
    EventDatabase, EventDeclarationDatabase,
    NamespaceDatabase,
    RuleDeclDatabase,
    StructDatabase, StructDeclDatabase, StructDefDatabase,
    TypeDatabase,
    PropertyDatabase,
    FunctionDatabase,
)]
#[derive(Default)]
pub struct AnalysisDatabase {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for AnalysisDatabase {}

#[macro_export]
macro_rules! impl_intern_key {
    ($name:ident) => {
        impl $crate::salsa::InternKey for $name {
            fn from_intern_id(v: $crate::salsa::InternId) -> Self {
                $name(v)
            }
            fn as_intern_id(&self) -> $crate::salsa::InternId {
                self.0
            }
        }
    };
}
