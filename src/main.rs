use std::fs::{read_to_string, File};
use std::io::Write;

mod arg_parse;
mod frontend;
mod preprocessor;

/// 每个人承担自己的风险！
#[macro_export]
macro_rules! risk {
    ($expression:expr, $pattern:pat => $extracted_expression:expr) => {
        match $expression {
            $pattern => $extracted_expression,
            _ => unreachable!(),
        }
    };
}

fn compile() -> Result<(), Box<dyn std::error::Error>> {
    let (mode, input, output) = arg_parse::parse(std::env::args())?;
    let code = preprocessor::preprocess(&read_to_string(input)?.replace("\r\n", "\n"));
    let ir = frontend::generate_ir(&code)?;
    let mut f = File::create(output)?;
    match mode {
        _ => f.write_fmt(format_args!("{:#?}", ir))?,
    }
    Ok(())
}

fn main() {
    if let Err(s) = compile() {
        println!("{}", s);
        std::process::exit(0);
    }
}
