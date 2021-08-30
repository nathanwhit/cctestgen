pub(crate) mod ast;
pub(crate) mod gen;
pub(crate) mod parser;

// use ast::{Descriptor, PairExt};
// use gen::{Mode, ToRust};
// use once_cell::sync::OnceCell;

// use std::sync::Mutex;

use color_eyre::Result;

use pest::{iterators::Pairs, Parser};
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

fn handle_parsed(mut input: Pairs<()>, mode: ()) -> Result<proc_macro2::TokenStream> {
    // let descriptors = input.next().expecting(Rule::descriptors)?;
    // let mut code = Vec::new();
    // let mut ctx = CodegenCtx::new();
    // for p in descriptors.into_inner() {
    //     if let Rule::EOI = p.as_rule() {
    //         continue;
    //     }
    //     let descriptor = Descriptor::parse(p)?;
    //     let rust = descriptor.to_rust(mode, &mut ctx)?;
    //     code.push(rust);
    // }
    // if let Mode::Unit = mode {
    //     Ok(quote! {
    //         #[cfg(all(not(feature = "integration-testing"), test, feature = "mock"))]
    //         mod test {
    //             use crate::handler::tests::execution::*;
    //             use crate::test_utils::*;

    //             #( #code )*
    //         }
    //     })
    // } else {
    //     Ok(quote! {
    //         #[cfg(feature = "integration-testing")]
    //         mod test {
    //             use crate::common::*;
    //             use ccprocessor_rust::test_utils::*;

    //             #( #code )*
    //         }
    //     })
    // }
    todo!()
}

fn entry(input: &str) -> Result<TokenStream> {
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
