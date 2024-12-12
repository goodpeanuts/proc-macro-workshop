use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    // eprintln!("{:#?}", ast);
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
    let builder_build = generate_fn_build(name, fields)?;
    let default_builders = generate_fn_builder_default(&builder_ident, fields)?;

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

/// extract fields from `DeriveInput`->`data`->`fields` in AST
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

/// if type is `G<T>` then return `T`, otherwise return None
fn get_generic_inner_type<'a>(
    ty: &'a syn::Type,
    g_str: &str,
) -> std::option::Option<&'a syn::Type> {
    if let syn::Type::Path(syn::TypePath { ref path, .. }) = ty {
        // get last path seg such as std::A::B::G<T>
        if let Some(path_seg) = path.segments.last() {
            if path_seg.ident == g_str {
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

/// parse `#[builder(each = "Value")]`, return `Value`
fn get_ident_for_vec(field: &syn::Field) -> std::option::Option<syn::Ident> {
    for attr in &field.attrs {
        if attr.path().is_ident("builder") {
            // parse builder
            let mut result = None;

            _ = attr.parse_nested_meta(|nested| {
                // parse  `(`
                if nested.path.is_ident("each") {
                    // parse each
                    let value = nested.value()?; // parse =
                    let ident: syn::LitStr = value.parse()?; // parse Value
                    result = Some(syn::Ident::new(ident.value().as_str(), ident.span()));
                    return Ok(());
                }
                Ok(())
            });

            if result.is_some() {
                return result;
            }
        }
    }

    None
}

/// provide builder function to construct Builder itself from other struct
fn generate_fn_builder_default(
    builder_ident: &syn::Ident,
    fields: &StructFields,
) -> syn::Result<proc_macro2::TokenStream> {
    let members = fields
        .iter()
        .map(|f| {
            let ident = &f.ident;
            if get_ident_for_vec(f).is_some() {
                quote::quote! {
                    #ident: std::vec::Vec::new(),
                }
            } else {
                quote::quote! {
                    #ident: std::option::Option::None,
                }
            }
        })
        .collect::<Vec<_>>();
    let builders = quote! {
        pub fn builder() -> #builder_ident {
            #builder_ident {
                #(#members)*
            }
        }
    };
    Ok(builders)
}

/// according to struct to generate builder define
fn generate_builder_define(fields: &StructFields) -> syn::Result<proc_macro2::TokenStream> {
    let idents = fields.iter().map(|f| &f.ident).collect::<Vec<_>>();
    let types = fields
        .iter()
        .map(|f| {
            if let Some(inner_type) = get_generic_inner_type(&f.ty, "Option") {
                quote::quote! {
                    std::option::Option<#inner_type>
                }
            } else if get_ident_for_vec(f).is_some() {
                // according to the test consuming, it can be sure that it is Vec
                let origin_type = &f.ty;
                quote::quote! {
                    #origin_type
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

/// according to struct to generate builder setters
fn generate_builder_setter(fields: &StructFields) -> syn::Result<proc_macro2::TokenStream> {
    // let idents = fields.iter().map(|f| &f.ident).collect::<Vec<_>>();
    let mut res = proc_macro2::TokenStream::new();
    for f in fields {
        let ident = &f.ident;
        let ty = &f.ty;

        let tokens = if let Some(inner_ty) = get_generic_inner_type(&f.ty, "Option") {
            quote::quote! {
                pub fn #ident(&mut self, #ident: #inner_ty) -> &mut Self {
                    self.#ident = std::option::Option::Some(#ident);
                    self
                }
            }
        } else if let Some(ref marco_ident) = get_ident_for_vec(f) {
            let inner_type = get_generic_inner_type(&f.ty, "Vec");
            let mut t = quote::quote! {
                pub fn #marco_ident(&mut self, #marco_ident: #inner_type) -> &mut Self {
                    self.#ident.push(#marco_ident);
                    self
                }
            };
            if marco_ident != ident.as_ref().unwrap() {
                t.extend(
                    quote::quote! {
                        pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                            self.#ident = #ident.clone();
                            self
                        }
                    }
                );
            }
            t
        } else {
            quote::quote! {
                pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                    self.#ident = std::option::Option::Some(#ident);
                    self
                }
            }
        };
        res.extend(tokens);
    }

    // eprintln!("{:#?}", res);

    Ok(res)
}

/// according to struct to generate build function return struct
fn generate_fn_build(
    original_ident: &syn::Ident,
    fields: &StructFields,
) -> syn::Result<proc_macro2::TokenStream> {
    let mut define_content = proc_macro2::TokenStream::new();
    for f in fields{
        let ident = &f.ident;
        let ty = &f.ty;
        define_content.extend(if get_generic_inner_type(ty, "Option").is_some() || get_ident_for_vec(f).is_some(){
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
