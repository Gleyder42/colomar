use syn::{parse_macro_input, DeriveInput};

mod derive_interned;

#[proc_macro_derive(Interned)]
pub fn derive_salsa_interned_id(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let output = derive_interned::derive_salsa_interned_id(input);

    proc_macro::TokenStream::from(output)
}
