use std::env::Args;

pub enum Mode {
    Koopa,
    RiscV,
    Optimization,
}

pub type ParsedArgs = (Mode, String, String);

pub fn parse(mut args: Args) -> Result<ParsedArgs, String> {
    let args = &mut args;
    let mode = match args.skip(1).next().unwrap().as_str() {
        "-koopa" => Ok(Mode::Koopa),
        "-riscv" => Ok(Mode::RiscV),
        "-perf" => Ok(Mode::Optimization),
        s => Err(format!("未知的模式: {}", s)),
    }?;
    let input = args.next().unwrap();
    let output = args.skip(1).next().unwrap();
    Ok((mode, input, output))
}
