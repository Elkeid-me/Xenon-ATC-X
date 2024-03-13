mod ast;
mod expr;
mod ir_generator;
mod parser;
mod ty;

pub fn generate_ast(code: &str) -> Result<ast::TranslationUnit, String> {
    parser::parse(code)
}

pub fn generate_ir(code: &str) -> Result<koopa::ir::entities::Program, String> {
    // Ok(ir_generator::generate_ir(generate_ast(code)?))
    todo!()
}

pub fn generate_ir_str(code: &str) -> Result<String, String> {
    Ok(ir_generator::generator_ir_eval(generate_ast(code)?))
}
