use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let name = &ast.ident;
    let bname = format!("{}Builder", name);
    let bident = syn::Ident::new(&bname, name.span());
    let expanded = quote! {
       pub struct #bident {
        executable: Option<String>,
        args: Option<Vec<String>>,
        env: Option<Vec<String>>,
        current_dir: Option<String>,
       }
       impl #bident {
           pub fn executable(&mut self, executable: String) -> &mut Self {
               self.executable = Some(executable);
               self
           }
           pub fn args(&mut self, args: Vec<String>) -> &mut Self {
               self.args = Some(args);
               self
           }
           pub fn env(&mut self, env: Vec<String>) -> &mut Self {
               self.env = Some(env);
               self
           }
           pub fn current_dir(&mut self, current_dir: String) -> &mut Self {
               self.current_dir = Some(current_dir);
               self
           }
           pub fn build(&mut self) -> Result<#name, Box<dyn std::error::Error>> {
               Ok(#name {
                   executable: self.executable.take().ok_or("executable is required")?,
                   args: self.args.take().ok_or("args is required")?,
                   env: self.env.take().ok_or("env is required")?,
                   current_dir: self.current_dir.take().ok_or("current_dir is required")?,
               })
           }
        }
       impl #name {
           pub fn builder() -> #bident {
               #bident {
                executable: None,
                args: None,
                env: None,
                current_dir: None,
               }
           }
       }
    };
    expanded.into()
}
