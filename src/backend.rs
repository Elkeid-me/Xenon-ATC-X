use self::risc_v::RiscV;
mod asm_generator;
mod risc_v;

pub fn generate_asm(ir: String) -> RiscV {
    let driver = koopa::front::Driver::from(ir);
    let program = driver.generate_program().unwrap();
    asm_generator::generate(program)
}
