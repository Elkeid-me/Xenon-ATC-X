// Copyright (C) 2024 Elkeid-me
//
// This file is part of Xenon ATC-X.
//
// Xenon ATC-X is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Xenon ATC-X is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Xenon ATC-X.  If not, see <http://www.gnu.org/licenses/>.

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
