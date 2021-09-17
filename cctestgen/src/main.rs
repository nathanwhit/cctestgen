pub mod gen;
pub mod parser;

use gen::Mode;

use std::{convert::TryFrom, fs};

use clap::{crate_version, Arg};
use color_eyre::{eyre::Context, Result};

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

    let contents = fs::read_to_string(file)
        .wrap_err_with(|| color_eyre::eyre::eyre!("failed to read file {}", file))?;
    let mode = Mode::try_from(mode)?;

    let descriptors = parser::Parser::parse(&contents)?;
    let output = gen::codegen(descriptors, mode)?;

    println!("{}", output);

    Ok(())
}
