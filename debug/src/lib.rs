use proc_macro::TokenStream;
use quote::quote;
use std::collections::HashMap;
use syn::visit::{self, Visit};
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
    let associated_types = get_generic_associated_types(ast);

    for field in fields {
        if let Some(s) = get_struct_member_type(field)? {
            struct_member_types.push(s);
        }
        if let Some(s) = get_phantomdata_innner_generic(field)? {
            phantomdata_inner_types.push(s);
        }
    }

    let mut generics = ast.generics.clone();

    if let Some(hatch) = get_struct_escape_hatch(ast) {
        generics.make_where_clause();
        generics
            .where_clause
            .as_mut()
            .unwrap()
            .predicates
            .push(syn::parse_str(hatch.as_str()).unwrap());
    } else {
        generics.params.iter_mut().for_each(|generic| {
            if let syn::GenericParam::Type(ref mut type_param) = generic {
                let ty_str = type_param.ident.to_string();

                if phantomdata_inner_types.contains(&ty_str)
                    && !struct_member_types.contains(&ty_str)
                {
                } else if associated_types.contains_key(&ty_str)
                    && !struct_member_types.contains(&ty_str)
                {
                } else {
                    type_param.bounds.push(syn::parse_quote!(std::fmt::Debug));
                }
            }
        });
    }

    generics.make_where_clause();
    for (_, associated_types) in &associated_types {
        for associated_type in associated_types {
            generics
                .where_clause
                .as_mut()
                .unwrap()
                .predicates
                .push(syn::parse_quote!(#associated_type:std::fmt::Debug));
        }
    }

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

fn get_struct_escape_hatch(st: &syn::DeriveInput) -> Option<String> {
    if let Some(inert_attr) = st.attrs.last() {
        let mut result = None;
        _ = inert_attr.parse_nested_meta(|nested| {
            if nested.path.is_ident("bound") {
                let value = nested.value()?;
                let ident: syn::LitStr = value.parse()?;
                result = Some(ident.value().to_string());
                return Ok(());
            } else {
                return Err(syn::Error::new_spanned(
                    nested.path,
                    "only #[bound] is supported",
                ));
            }
        });
        return result;
    }
    None
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

struct TypePathVisitor {
    generic_type_names: Vec<String>,
    associated_types: HashMap<String, Vec<syn::TypePath>>,
}

impl<'ast> Visit<'ast> for TypePathVisitor {
    // visit_type_path 这个回调函数就是我们所关心的
    fn visit_type_path(&mut self, node: &'ast syn::TypePath) {
        if node.path.segments.len() >= 2 {
            let generic_type_name = node.path.segments[0].ident.to_string();
            if self.generic_type_names.contains(&generic_type_name) {
                // 如果满足上面的两个筛选条件，那么就把结果存起来
                self.associated_types
                    .entry(generic_type_name)
                    .or_insert(Vec::new())
                    .push(node.clone());
            }
        }
        // Visit 模式要求在当前节点访问完成后，继续调用默认实现的visit方法，从而遍历到所有的
        // 必须调用这个函数，否则遍历到这个节点就不再往更深层走了
        visit::visit_type_path(self, node);
    }
}

fn get_generic_associated_types(st: &syn::DeriveInput) -> HashMap<String, Vec<syn::TypePath>> {
    // 首先构建筛选条件
    let origin_generic_param_names: Vec<String> = st
        .generics
        .params
        .iter()
        .filter_map(|f| {
            if let syn::GenericParam::Type(ty) = f {
                return Some(ty.ident.to_string());
            }
            return None;
        })
        .collect();

    let mut visitor = TypePathVisitor {
        generic_type_names: origin_generic_param_names, // 用筛选条件初始化Visitor
        associated_types: HashMap::new(),
    };

    // 以st语法树节点为起点，开始Visit整个st节点的子节点
    visitor.visit_derive_input(st);
    return visitor.associated_types;
}
