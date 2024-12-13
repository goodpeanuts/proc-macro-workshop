use proc_macro::TokenStream;
use quote::quote;
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

fn do_expand(ast: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let struct_name = &ast.ident;
    let fields = get_fields(ast)?;
    let idents = fields.iter().map(|f| &f.ident).collect::<Vec<_>>();

    let expanded = quote! {
        impl std::fmt::Debug for #struct_name {
            fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
                fmt.debug_struct(std::stringify!(#struct_name))
                    // .field("name", &self.name)
                    // .field("age", &self.age)
                    #( .field(std::stringify!(#idents), &self.#idents))*
                    .finish()
            }
        }
    };
    Ok(expanded)
}

type StructFields = syn::punctuated::Punctuated<syn::Field, syn::Token![,]>;

/// get fields
fn get_fields(ast: &syn::DeriveInput) -> syn::Result<&StructFields> {
    if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = ast.data
    {
        Ok(named)
    } else {
        Err(syn::Error::new(
            ast.ident.span(),
            "only structs with named fields are supported",
        ))
    }
}
