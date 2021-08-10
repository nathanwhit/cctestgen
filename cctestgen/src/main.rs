pub mod ast;
pub mod gen;

use ast::{Descriptor, PairExt};
use gen::{Mode, ToRust};

use std::{convert::TryFrom, fs};

use clap::Arg;
use color_eyre::{Result};


use pest::{
    Parser,
};





pub mod parse {
    
    use pest_derive::Parser;

    #[derive(Parser)]
    #[grammar = "descriptor.pest"]
    pub struct DescriptorParser;
}

use parse::{DescriptorParser, Rule};

use crate::ast::ParseAst;

fn main() -> Result<()> {
    color_eyre::install()?;
    let app = clap::App::new("cctestgen")
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

    let contents = fs::read_to_string(file)?;
    let mode = Mode::try_from(mode)?;

    let mut result = DescriptorParser::parse(Rule::descriptors, &contents)?;
    let descriptors = result.next().expecting(Rule::descriptors)?;
    let mut code = Vec::new();
    for p in descriptors.into_inner() {
        if let Rule::EOI = p.as_rule() {
            continue;
        }
        let descriptor = Descriptor::parse(p)?;
        let rust = descriptor.to_rust(mode)?;
        code.push(rust);
    }
    println!("use super::*;");
    for c in code {
        println!("\n{}", c);
    }
    // let descriptor = Descriptor::parse(result)?;
    // println!("{:?}", descriptor);
    // let rust = descriptor.to_rust(Mode::Unit)?;
    // println!("\n{}", rust);
    Ok(())
}
