mod ast;
mod expr;
mod ir_generator;
mod parser;
mod ty;

pub(super) fn generate_ir(code: &str) -> Result<String, String> {
    Ok(ir_generator::generator_ir(parser::parse(code)?))
}
