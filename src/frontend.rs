mod ast;
mod ir_generator;
mod parser;
mod ty;

pub fn generate_ir(code: &str) -> Result<String, String> {
    Ok(ir_generator::generator_ir(parser::parse(code)?))
}
