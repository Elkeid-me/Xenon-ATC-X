mod ast;
mod parser;
mod ty;

fn generate_ast(code: &str) -> Result<ast::TranslationUnit, String> {
    parser::parse(code)
}

pub fn generate_ir(code: &str) -> Result<ast::TranslationUnit, String> {
    generate_ast(code)
}
