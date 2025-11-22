use proc_macro2::*;
use quote::quote;
use syn::DeriveInput;

pub fn derive_salsa_interned_id(_input: DeriveInput) -> TokenStream {
    // In salsa 0.24, interned types are defined using #[salsa::interned] directly
    // This derive macro is kept for backwards compatibility but generates no code
    quote! {}
}
