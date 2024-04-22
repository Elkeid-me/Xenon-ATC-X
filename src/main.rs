mod arg_parse;
mod backend;
mod frontend;
mod preprocessor;

use arg_parse::*;
use backend::generate_asm;
use frontend::generate_ir;
use preprocessor::preprocess;
use std::fs::{read_to_string, File};
use std::io::Write;

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
    let (mode, input, output) = parse(std::env::args())?;
    let code = preprocess(read_to_string(input)?);
    let mut f = File::create(output)?;
    let ir = generate_ir(&code)?;
    match mode {
        Mode::Koopa => write!(f, "{ir}")?,
        Mode::RiscV | Mode::Optimization => {
            for item in generate_asm(ir) {
                write!(f, "{item}")?;
            }
        }
    }
    Ok(())
}

fn main() {
    if let Err(s) = compile() {
        println!("{s}");
        std::process::exit(1);
    }
}
