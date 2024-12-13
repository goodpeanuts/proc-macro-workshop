use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    // println!("{:#?}", ast);
    match do_expand(&ast) {
        Ok(expanded) => expanded.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn do_expand(ast: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let struct_name = &ast.ident;
    let fields = get_fields(ast)?;
    let format_string_def = generate_debug_format_string(fields)?;

    let expanded = quote! {
        impl std::fmt::Debug for #struct_name {
            fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
                fmt.debug_struct(std::stringify!(#struct_name))
                    #format_string_def
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

fn get_debug_format_string(field: &syn::Field) -> syn::Result<std::option::Option<String>> {
    for attr in &field.attrs {
        if attr.path().is_ident("debug") {
            if let syn::Meta::NameValue(ref meta_name_value) = attr.meta {
                if let syn::Expr::Lit(ref expr_lit) = meta_name_value.value {
                    if let syn::Lit::Str(ref lit_str) = expr_lit.lit {
                        return Ok(Some(lit_str.value()));
                    }
                }
            }
        } else {
            return Err(syn::Error::new_spanned(
                attr.path(),
                "only #[debug] is supported",
            ));
        }
    }
    Ok(None)
}

fn generate_debug_format_string(fields: &StructFields) -> syn::Result<proc_macro2::TokenStream> {
    let mut format_string = proc_macro2::TokenStream::new();
    for field in fields {
        let ident = &field.ident;

        if let Some(debug_format_string) = get_debug_format_string(field)? {
            format_string.extend(quote::quote! {
                .field(std::stringify!(#ident), &format_args!(#debug_format_string, self.#ident))
            });
        } else {
            format_string.extend(quote::quote! {
                .field(std::stringify!(#ident), &self.#ident)
            });
        }
    }
    Ok(format_string)
}
