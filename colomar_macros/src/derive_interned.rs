use proc_macro2::*;
use quote::{format_ident, quote};
use syn::DeriveInput;

pub fn derive_salsa_interned_id(input: DeriveInput) -> TokenStream {
    let name = format_ident!("{}Id", input.ident);

    quote! {
        #[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
        pub struct #name(salsa::InternId);

        impl salsa::InternKey for #name {
            fn from_intern_id(v:salsa::InternId) -> Self {
                #name(v)
            }
            fn as_intern_id(&self) -> salsa::InternId {
                self.0
            }
        }
    }
}
