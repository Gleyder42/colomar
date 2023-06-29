use crate::compiler::wir;

#[salsa::query_group(InternerDatabase)]
pub trait Interner {

    #[salsa::interned]
    fn intern_wir_function(&self, func: wir::NativeFunc) -> wir::NativeFuncId;
}