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
    let fields = get_fields(ast)?;
    let builder_def = generate_builder_define(fields)?;
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

fn get_optional_inner_type(ty: &syn::Type) -> std::option::Option<&syn::Type> {
    if let syn::Type::Path(syn::TypePath { ref path, .. }) = ty {
        if let Some(path_seg) = path.segments.last() {
            if path_seg.ident == "Option" {
                if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                    ref args,
                    ..
                }) = path_seg.arguments
                {
                    if let Some(syn::GenericArgument::Type(inner_ty)) = args.first() {
                        return Some(inner_ty);
                    }
                }
            }
        }
    }
    None
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

fn generate_builder_define(fields: &StructFields) -> syn::Result<proc_macro2::TokenStream> {
    let idents = fields.iter().map(|f| &f.ident).collect::<Vec<_>>();
    let types = fields
        .iter()
        .map(|f| {
            if let Some(inner_type) = get_optional_inner_type(&f.ty) {
                quote::quote! {
                    std::option::Option<#inner_type>
                }
            } else {
                let origin_type = &f.ty;
                quote::quote! {
                    std::option::Option<#origin_type>
                }
            }
        })
        .collect::<Vec<_>>();
    let builders = quote! {
        #( #idents: #types ),*
    };
    Ok(builders)
}

fn generate_builder_setter(fields: &StructFields) -> syn::Result<proc_macro2::TokenStream> {
    let idents = fields.iter().map(|f| &f.ident).collect::<Vec<_>>();
    let types = fields
        .iter()
        .map(|f| {
            if let Some(inner_ty) = get_optional_inner_type(&f.ty) {
                quote::quote! {
                    #inner_ty
                }
            } else {
                let origin_type = &f.ty;
                quote::quote! {
                    #origin_type
                }
            }
        })
        .collect::<Vec<_>>();

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
    let types = fields.iter().map(|f| &f.ty).collect::<Vec<_>>();

    let mut define_content = proc_macro2::TokenStream::new();
    for (&ident, &ty) in idents.iter().zip(types.iter()) {
        define_content.extend(if get_optional_inner_type(ty).is_some() {
            quote::quote! { #ident: self.#ident.clone(), }
        } else {
            quote::quote! { #ident: self.#ident.take().ok_or(std::format!("{} is required", std::stringify!(#ident)))?, }
        });
    }
    let build = quote! {
        pub fn build(&mut self) -> Result<#original_ident, Box<dyn std::error::Error>> {
            Ok(#original_ident {
                #define_content
            })
        }
    };
    Ok(build)
}
