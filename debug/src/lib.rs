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

    let mut struct_member_types = Vec::new();
    let mut phantomdata_inner_types = Vec::new();
    for field in fields {
        if let Some(s) = get_struct_member_type(field)? {
            struct_member_types.push(s);
        }
        if let Some(s) = get_phantomdata_innner_generic(field)? {
            phantomdata_inner_types.push(s);
        }
    }

    let mut generics = ast.generics.clone();
    generics.params.iter_mut().for_each(|generic| {
        if let syn::GenericParam::Type(ref mut type_param) = generic {
            let ty_str = type_param.ident.to_string();
            if phantomdata_inner_types.contains(&ty_str) && !struct_member_types.contains(&ty_str) {
            } else {
                type_param.bounds.push(syn::parse_quote!(std::fmt::Debug));
            }
        }
    });
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let expanded = quote! {
        impl #impl_generics std::fmt::Debug for #struct_name #ty_generics #where_clause {
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
            if let syn::Meta::NameValue(syn::MetaNameValue {
                value:
                    syn::Expr::Lit(syn::ExprLit {
                        lit: syn::Lit::Str(ref lit_str),
                        ..
                    }),
                ..
            }) = attr.meta
            {
                return Ok(Some(lit_str.value()));
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

fn get_phantomdata_innner_generic(field: &syn::Field) -> syn::Result<Option<String>> {
    if let syn::Type::Path(syn::TypePath {
        path: syn::Path { ref segments, .. },
        ..
    }) = field.ty
    {
        if let Some(syn::PathSegment {
            ref ident,
            ref arguments,
        }) = segments.last()
        {
            if ident == "PhantomData" {
                if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                    args,
                    ..
                }) = arguments
                {
                    if let Some(syn::GenericArgument::Type(syn::Type::Path(ref gp))) = args.first()
                    {
                        if let Some(generic_ident) = gp.path.segments.first() {
                            return Ok(Some(generic_ident.ident.to_string()));
                        }
                    }
                }
            }
        }
    }
    return Ok(None);
}

fn get_struct_member_type(field: &syn::Field) -> syn::Result<Option<String>> {
    if let syn::Type::Path(syn::TypePath {
        path: syn::Path { ref segments, .. },
        ..
    }) = field.ty
    {
        if let Some(syn::PathSegment { ref ident, .. }) = segments.last() {
            return Ok(Some(ident.to_string()));
        }
    }
    return Ok(None);
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
