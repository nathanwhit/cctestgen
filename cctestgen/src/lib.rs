pub(crate) mod ast;
pub(crate) mod gen;
pub(crate) mod parser;

// use ast::{Descriptor, PairExt};
// use gen::{Mode, ToRust};
// use once_cell::sync::OnceCell;

// use std::sync::Mutex;

use color_eyre::Result;

// use quote::quote;

// pub(crate) mod parse {

//     use pest_derive::Parser;

//     #[derive(Parser)]
//     #[grammar = "descriptor.pest"]
//     pub(crate) struct DescriptorParser;
// }

// use parse::{DescriptorParser, Rule};
use proc_macro_error::proc_macro_error;

// use crate::{ast::ParseAst, gen::CodegenCtx};

// pub(crate) static SOURCE: OnceCell<Mutex<ariadne::Source>> = OnceCell::new();

use proc_macro::TokenStream;

#[proc_macro_error]
#[proc_macro]
pub fn cc_test(input: TokenStream) -> TokenStream {
    let i = input.to_string();
    match entry(&i) {
        Ok(out) => {
            println!("{}", out);
            out
        }
        Err(e) => proc_macro_error::abort_call_site!("Test generation failed: {:#?}", e),
    }
}

fn entry(_input: &str) -> Result<TokenStream> {
    // let result = DescriptorParser::parse(Rule::descriptors, &input)?;
    // let unit = handle_parsed(result.clone(), Mode::Unit)?;

    // let integration = handle_parsed(result, Mode::Integration)?;

    // Ok(quote! {
    //     #unit

    //     #integration
    // }
    // .into())
    todo!()
}
