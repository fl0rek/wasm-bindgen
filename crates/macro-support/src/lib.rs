//! This crate contains the part of the implementation of the `#[wasm_bindgen]` optsibute that is
//! not in the shared backend crate.

#![doc(html_root_url = "https://docs.rs/wasm-bindgen-macro-support/0.2")]

extern crate proc_macro2;
extern crate quote;
#[macro_use]
extern crate syn;
#[macro_use]
extern crate wasm_bindgen_backend as backend;
extern crate wasm_bindgen_shared as shared;

pub use crate::parser::BindgenAttrs;
use crate::parser::MacroParse;
use backend::{Diagnostic, TryToTokens};
use proc_macro2::TokenStream;
use quote::ToTokens;
use quote::TokenStreamExt;
use syn::parse::{Parse, ParseStream, Result as SynResult};

mod parser;

/// Takes the parsed input from a `#[wasm_bindgen]` macro and returns the generated bindings
pub fn expand(attr: TokenStream, input: TokenStream) -> Result<TokenStream, Diagnostic> {
    parser::reset_attrs_used();
    let item = syn::parse2::<syn::Item>(input)?;
    let opts = syn::parse2(attr)?;

    let mut tokens = proc_macro2::TokenStream::new();
    let mut program = backend::ast::Program::default();
    item.macro_parse(&mut program, (Some(opts), &mut tokens))?;
    program.try_to_tokens(&mut tokens)?;

    // If we successfully got here then we should have used up all attributes
    // and considered all of them to see if they were used. If one was forgotten
    // that's a bug on our end, so sanity check here.
    //parser::check_unused_attrs(&mut tokens);

    Ok(tokens)
}

/// Takes the parsed input from a `wasm_bindgen::link_to` macro and returns the generated link
pub fn expand_link_to(input: TokenStream) -> Result<TokenStream, Diagnostic> {
    parser::reset_attrs_used();
    let opts = syn::parse2(input)?;

    let mut tokens = proc_macro2::TokenStream::new();
    let link = parser::link_to(opts)?;
    link.try_to_tokens(&mut tokens)?;

    Ok(tokens)
}

/// Takes the parsed input from a `#[wasm_bindgen]` macro and returns the generated bindings
pub fn expand_class_marker(
    attr: TokenStream,
    input: TokenStream,
) -> Result<TokenStream, Diagnostic> {
    parser::reset_attrs_used();
    let mut item = syn::parse2::<syn::ImplItemFn>(input)?;
    let opts: ClassMarker = syn::parse2(attr)?;

    let mut program = backend::ast::Program::default();
    item.macro_parse(&mut program, &opts)?;

    // This is where things are slightly different, we are being expanded in the
    // context of an impl so we can't inject arbitrary item-like tokens into the
    // output stream. If we were to do that then it wouldn't parse!
    //
    // Instead what we want to do is to generate the tokens for `program` into
    // the header of the function. This'll inject some no_mangle functions and
    // statics and such, and they should all be valid in the context of the
    // start of a function.
    //
    // We manually implement `ToTokens for ImplItemFn` here, injecting our
    // program's tokens before the actual method's inner body tokens.
    let mut tokens = proc_macro2::TokenStream::new();
    tokens.append_all(
        item.attrs
            .iter()
            .filter(|attr| matches!(attr.style, syn::AttrStyle::Outer)),
    );
    item.vis.to_tokens(&mut tokens);
    item.sig.to_tokens(&mut tokens);
    let mut err = None;
    item.block.brace_token.surround(&mut tokens, |tokens| {
        if let Err(e) = program.try_to_tokens(tokens) {
            err = Some(e);
        }
        parser::check_unused_attrs(tokens); // same as above
        tokens.append_all(
            item.attrs
                .iter()
                .filter(|attr| matches!(attr.style, syn::AttrStyle::Inner(_))),
        );
        tokens.append_all(&item.block.stmts);
    });

    if let Some(err) = err {
        return Err(err);
    }

    Ok(tokens)
}

struct ClassMarker {
    class: syn::Ident,
    js_class: String,
    wasm_bindgen: syn::Path,
    wasm_bindgen_futures: syn::Path,
}

impl Parse for ClassMarker {
    fn parse(input: ParseStream) -> SynResult<Self> {
        let class = input.parse::<syn::Ident>()?;
        input.parse::<Token![=]>()?;
        let mut js_class = input.parse::<syn::LitStr>()?.value();
        js_class = js_class
            .strip_prefix("r#")
            .map(String::from)
            .unwrap_or(js_class);

        let mut wasm_bindgen = None;
        let mut wasm_bindgen_futures = None;

        loop {
            if input.parse::<Option<Token![,]>>()?.is_some() {
                let ident = input.parse::<syn::Ident>()?;

                if ident == "wasm_bindgen" {
                    if wasm_bindgen.is_some() {
                        return Err(syn::Error::new(
                            ident.span(),
                            "found duplicate `wasm_bindgen`",
                        ));
                    }

                    input.parse::<Token![=]>()?;
                    wasm_bindgen = Some(input.parse::<syn::Path>()?);
                } else if ident == "wasm_bindgen_futures" {
                    if wasm_bindgen_futures.is_some() {
                        return Err(syn::Error::new(
                            ident.span(),
                            "found duplicate `wasm_bindgen_futures`",
                        ));
                    }

                    input.parse::<Token![=]>()?;
                    wasm_bindgen_futures = Some(input.parse::<syn::Path>()?);
                } else {
                    return Err(syn::Error::new(
                        ident.span(),
                        "expected `wasm_bindgen` or `wasm_bindgen_futures`",
                    ));
                }
            } else {
                break;
            }
        }

        Ok(ClassMarker {
            class,
            js_class,
            wasm_bindgen: wasm_bindgen.unwrap_or_else(|| syn::parse_quote! { wasm_bindgen }),
            wasm_bindgen_futures: wasm_bindgen_futures
                .unwrap_or_else(|| syn::parse_quote! { wasm_bindgen_futures }),
        })
    }
}

pub fn expand_struct_marker(item: TokenStream) -> Result<TokenStream, Diagnostic> {
    println!("II == {item}");

    // TODO: let's make it work first
    //parser::reset_attrs_used();

    let mut tokens = proc_macro2::TokenStream::new();
    let mut program = backend::ast::Program::default();

    println!("pars");
    let mut s : syn::ItemStruct = syn::parse2(item)?;
    let opts = StructMarker::find(&mut s.attrs)?;
    //println!("parsed s: {s:?}");
    //let opts = BindgenAttrs::find(&mut s.attrs)?;

    let ast_struct = convert_to_programs_ast(&program, s, opts)?;

    //program.structs.push((&mut s).convert((&program, opts))?);
    println!("postconvert");

    program.structs.push(ast_struct);

    program.try_to_tokens(&mut tokens)?;

    // TODO: 
    //parser::check_unused_attrs()

    //println!("RES: {tokens:?}");

    Ok(tokens)
}

struct StructMarker {
    js_name: Option<String>,
    is_inspectable: bool,
    getter_with_clone: Option<Span>,
    wasm_bindgen: Option<syn::Path>,
}

use syn::MacroDelimiter;

impl StructMarker {
    fn find(attrs: &mut Vec<syn::Attribute>) -> Result<Self, Diagnostic> {
        let mut ret = StructMarker {
            js_name: None,
            is_inspectable: false,
            getter_with_clone: None,
            wasm_bindgen: None,
        };
        loop {
            // TODO: disallow multiple
            let pos = attrs
                .iter()
                .enumerate()
                .find(|&(_, m)| m.path().segments[0].ident == "__wasm_bindgen_attrs")
                .map(|a| a.0);
            let pos = match pos {
                Some(i) => i,
                None => return Ok(ret),
            };
            let attr = attrs.remove(pos);

            let tokens = match attr.meta {
                syn::Meta::Path(_) => continue,
                syn::Meta::List(syn::MetaList {
                    delimiter: MacroDelimiter::Paren(_),
                    tokens,
                    ..
                }) => tokens,
                syn::Meta::List(_) | syn::Meta::NameValue(_) => {
                    bail_span!(attr, "malformed #[wasm_bindgen] attribute")
                }
            };
            //println!("tk: {tokens:#?}");
            ret = syn::parse2(tokens)?;

            //let m : syn::MetaNameValue = syn::parse2(tokens)?;
            //println!("meta: {m:#?}");
        }
    }
}

impl Parse for StructMarker {
    fn parse(input: ParseStream) -> SynResult<Self> {
        let name = input.parse::<syn::LitStr>()?;
        input.parse::<Token![,]>()?;
        let is_inspectable = input.parse::<syn::LitBool>()?.value;
        input.parse::<Token![,]>()?;
        let getter_with_clone = input.parse::<syn::LitBool>()?;
        input.parse::<Token![,]>()?;
        let wasm_bindgen = input.parse::<syn::Ident>()?;
        
        let js_name = Some(name.value());
        println!("nn: {js_name:?}");

        Ok(StructMarker {
            js_name: Some(name.value()),
            is_inspectable,
            getter_with_clone: if getter_with_clone.value {Some(getter_with_clone.span)} else { None },
            wasm_bindgen : Some(wasm_bindgen.into()),
        })
    }
}

/*
impl Parse for StructMarker {
    fn parse(input: ParseStream) -> SynResult<Self> {
        // XXX: not exactly, but works
        let class = input.parse::<syn::Ident>()?;
        input.parse::<Token![=]>()?;
        let mut js_class = input.parse::<syn::LitStr>()?.value();
        js_class = js_class
            .strip_prefix("r#")
            .map(String::from)
            .unwrap_or(js_class);

        let mut wasm_bindgen = None;
        let mut is_inspectable = None;
        let mut getter_with_clone = None;

        loop {
            if input.parse::<Option<Token![,]>>()?.is_some() {
                let ident = input.parse::<syn::Ident>()?;

                if ident == "wasm_bindgen" {
                    if wasm_bindgen.is_some() {
                        return Err(syn::Error::new(
                            ident.span(),
                            "found duplicate `wasm_bindgen`",
                        ));
                    }

                    input.parse::<Token![=]>()?;
                    wasm_bindgen = Some(input.parse::<syn::Path>()?);
                } else if ident == "is_inspectable" {
                    input.parse::<Token![=]>()?;
                    is_inspectable = Some(input.parse::<syn::LitBool>()?.value);
                } else if ident == "getter_with_clone" {
                    input.parse::<Token![=]>()?;
                    getter_with_clone = Some(input.parse::<syn::LitBool>()?.value);
                } else {
                    return Err(syn::Error::new(
                        ident.span(),
                        "expected `wasm_bindgen` or `wasm_bindgen_futures`",
                    ));
                }
            } else {
                break;
            }
        }

        Ok(StructMarker {
            js_class,
            wasm_bindgen: wasm_bindgen.unwrap_or_else(|| syn::parse_quote! { wasm_bindgen }),
            is_inspectable: is_inspectable.unwrap(),
            getter_with_clone: getter_with_clone.unwrap(),
        })
    }
}
*/

use crate::backend::ast;
use crate::syn::ext::IdentExt;
use crate::parser::extract_doc_comments;
use proc_macro2::{Ident, Span};

// TODO: pretended span for getter_with_clone
fn convert_to_programs_ast(program: &ast::Program, mut s: syn::ItemStruct, opts: StructMarker/*js_name: String, is_inspectable: bool, getter_with_clone: Option<&Span>*/) -> Result<ast::Struct, Diagnostic> {

    let js_name = opts.js_name.unwrap_or(s.ident.unraw().to_string());
    let is_inspectable = opts.is_inspectable;
    let getter_with_clone = opts.getter_with_clone;

    let mut fields = Vec::new();

    for (i, field) in s.fields.iter_mut().enumerate() {
        match field.vis {
            syn::Visibility::Public(..) => {}
            _ => continue,
        }
        let (js_field_name, member) = match &field.ident {
            Some(ident) => (ident.unraw().to_string(), syn::Member::Named(ident.clone())),
            None => (i.to_string(), syn::Member::Unnamed(i.into())),
        };

        let attrs = BindgenAttrs::find_custom(&mut field.attrs, "__wasm_bindgen_attrs")?;
        if attrs.skip().is_some() {
            attrs.check_used();
            continue;
        }

        let wasm_bindgen = &program.wasm_bindgen;

        let js_field_name = match attrs.js_name() {
            Some((name, _)) => name.to_string(),
            None => js_field_name,
        };

        let comments = extract_doc_comments(&field.attrs);
        let getter = shared::struct_field_get(&js_name, &js_field_name);
        let setter = shared::struct_field_set(&js_name, &js_field_name);

        fields.push(ast::StructField {
            rust_name: member,
            js_name: js_field_name,
            struct_name: s.ident.clone(),
            readonly: attrs.readonly().is_some(),
            ty: field.ty.clone(),
            getter: Ident::new(&getter, Span::call_site()),
            setter: Ident::new(&setter, Span::call_site()),
            comments,
            generate_typescript: attrs.skip_typescript().is_none(),
            generate_jsdoc: attrs.skip_jsdoc().is_none(),
            getter_with_clone: attrs.getter_with_clone().or(getter_with_clone.as_ref()).copied(),
            wasm_bindgen: program.wasm_bindgen.clone(),
        });
        attrs.check_used();
    }
    // TODO:  fix
    let generate_typescript = false; // attrs.skip_typescript().is_none();
    let comments: Vec<String> = extract_doc_comments(&s.attrs);
    // TODO: uncomment
    //attrs.check_used();
    Ok(ast::Struct {
        rust_name: s.ident.clone(),
        js_name,
        fields,
        comments,
        is_inspectable,
        generate_typescript,
        wasm_bindgen: program.wasm_bindgen.clone(),
    })
}
