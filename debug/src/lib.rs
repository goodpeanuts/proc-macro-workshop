use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(CustomDebug)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    // eprintln!("{:#?}", ast);
    match do_expand(&ast) {
        Ok(expanded) => expanded.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn do_expand(_ast: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let rt = proc_macro2::TokenStream::new();
    return Ok(rt);
}
