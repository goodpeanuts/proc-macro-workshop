use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    match do_expand(&ast) {
        Ok(expanded) => expanded.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn do_expand(ast: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let name = &ast.ident;
    let builder_name = format!("{}Builder", name);
    let builder_ident = syn::Ident::new(&builder_name, name.span());
    let fields = get_fields(&ast)?;
    let builder_def = generate_builder_def(fields)?;
    let builder_setters = generate_builder_setter(fields)?;
    let builder_build = generate_build(name, fields)?;
    let default_builders = generate_builder_default(&builder_ident, fields)?;

    let expanded = quote! {
       pub struct #builder_ident {
            #builder_def
       }
       impl #builder_ident {
            #builder_setters
            #builder_build
        }
       impl #name {
            #default_builders
       }
    };
    Ok(expanded)
}

type StructFields = syn::punctuated::Punctuated<syn::Field, syn::Token![,]>;

fn get_fields(d: &syn::DeriveInput) -> syn::Result<&StructFields> {
    if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = d.data
    {
        Ok(named)
    } else {
        Err(syn::Error::new(
            d.ident.span(),
            "only structs with named fields are supported",
        ))
    }
}

fn generate_builder_default(
    builder_ident: &syn::Ident,
    fields: &StructFields,
) -> syn::Result<proc_macro2::TokenStream> {
    let idents = fields.iter().map(|f| &f.ident).collect::<Vec<_>>();
    let builders = quote! {
        pub fn builder() -> #builder_ident {
            #builder_ident {
                #( #idents: None ),*
            }
        }
    };
    Ok(builders)
}

fn generate_builder_def(fields: &StructFields) -> syn::Result<proc_macro2::TokenStream> {
    let idents = fields.iter().map(|f| &f.ident).collect::<Vec<_>>();
    let types = fields.iter().map(|f| &f.ty).collect::<Vec<_>>();
    let builders = quote! {
        #( #idents: std::option::Option<#types> ),*
    };
    Ok(builders)
}

fn generate_builder_setter(fields: &StructFields) -> syn::Result<proc_macro2::TokenStream> {
    let idents = fields.iter().map(|f| &f.ident).collect::<Vec<_>>();
    let types = fields.iter().map(|f| &f.ty).collect::<Vec<_>>();

    let setters = quote! {
        #(
            pub fn #idents(&mut self, #idents: #types) -> &mut Self {
                self.#idents = Some(#idents);
                self
            }
        )*
    };
    Ok(setters)
}

fn generate_build(
    original_ident: &syn::Ident,
    fields: &StructFields,
) -> syn::Result<proc_macro2::TokenStream> {
    let idents = fields.iter().map(|f| &f.ident).collect::<Vec<_>>();
    // let types = fields.iter().map(|f| &f.ty).collect::<Vec<_>>();

    let build = quote! {
        pub fn build(&mut self) -> Result<#original_ident, Box<dyn std::error::Error>> {
            Ok(#original_ident {
                #( #idents: self.#idents.take().ok_or(std::format!("{} is required", std::stringify!(#idents)))?),*
            })
        }
    };
    Ok(build)
}
