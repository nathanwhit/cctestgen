#![allow(unused)]
pub mod ast;
pub mod gen;
pub mod parser;

use ariadne::{Label, Source};
// use ast::{Descriptor, PairExt};
use gen::Mode;
use once_cell::sync::OnceCell;
use proc_macro2::TokenStream;

use std::{convert::TryFrom, fs, sync::Mutex};

use clap::{crate_version, Arg};
use color_eyre::{eyre::Context, Result};

use pest::Parser;

use crate::parser::{Descriptors, Foldable};

// pub mod parse {

//     use pest_derive::Parser;

//     #[derive(Parser)]
//     #[grammar = "descriptor.pest"]
//     pub struct DescriptorParser;
// }

// use parse::{DescriptorParser, Rule};

// use crate::{ast::ParseAst, gen::CodegenCtx};

// pub static SOURCE: OnceCell<Mutex<ariadne::Source>> = OnceCell::new();
// pub static FILENAME: OnceCell<String> = OnceCell::new();

fn main() -> Result<()> {
    color_eyre::install()?;
    pretty_env_logger::init();
    let app = clap::App::new("cctestgen")
        .version(crate_version!())
        .arg(Arg::with_name("filename").help("Input file").required(true))
        .arg(
            Arg::with_name("mode")
                .help("Test mode to generate code for")
                .required(true)
                .value_name("mode")
                .long("mode")
                .short("m"),
        );
    let matches = app.get_matches();
    let file = matches.value_of("filename").unwrap();
    let mode = matches.value_of("mode").unwrap();

    // let _ = FILENAME.set(file.into());

    let contents = fs::read_to_string(file)
        .wrap_err_with(|| color_eyre::eyre::eyre!("failed to read file {}", file))?;
    // let _ = SOURCE.set(Mutex::new(ariadne::Source::from(&contents)));
    let mode = Mode::try_from(mode)?;
    let f: TokenStream =
        std::str::FromStr::from_str(&contents).map_err(|e| color_eyre::eyre::eyre!("{:?}", e))?;
    let result: syn::Result<Descriptors> = syn::parse2(f);
    match result {
        Ok(res) => {
           
            println!("{:#?}", res.descriptors[0]);
        }
        Err(e) => {
            let parser_span = parser::Span::from_span(&contents, e.span());
            ariadne::Report::build(ariadne::ReportKind::Error, (), parser_span.lo)
                .with_message(e)
                .with_label(
                    Label::new(parser_span.lo..parser_span.hi).with_message("At this location"),
                )
                .finish()
                .eprint(Source::from(&contents))?;
            // log::error!("{} at {:?}", e, parser_span);
        }
    }
    // println!("{:?}", result);
    // let mut result = DescriptorParser::parse(Rule::descriptors, &contents)?;
    // let descriptors = result.next().expecting(Rule::descriptors)?;
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
    //     println!("use super::*;");
    // } else {
    //     println!(r##"#![cfg(feature = "integration-testing")]"##);

    //     println!("use super::common::*;");
    // }
    // for c in code {
    //     println!("\n{}", c);
    // }
    // let descriptor = Descriptor::parse(result)?;
    // println!("{:?}", descriptor);
    // let rust = descriptor.to_rust(Mode::Unit)?;
    // println!("\n{}", rust);
    Ok(())
}
