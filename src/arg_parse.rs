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

use std::env::Args;

pub enum Mode {
    Koopa,
    RiscV,
    Optimization,
}

pub type ParsedArgs = (Mode, String, String);

pub fn parse(mut args: Args) -> Result<ParsedArgs, String> {
    let args = &mut args;
    let mode = match args.nth(1).unwrap().as_str() {
        "-koopa" => Ok(Mode::Koopa),
        "-riscv" => Ok(Mode::RiscV),
        "-perf" => Ok(Mode::Optimization),
        s => Err(format!("未知的模式: {s}")),
    }?;
    let input = args.next().unwrap();
    let output = args.nth(1).unwrap();
    Ok((mode, input, output))
}
