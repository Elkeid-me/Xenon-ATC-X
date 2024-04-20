mod asm_generator;
mod risc_v;

use risc_v::RiscV;

pub fn generate_asm(ir: String) -> RiscV {
    let driver = koopa::front::Driver::from(ir);
    let program = driver.generate_program().unwrap();
    asm_generator::generate(program)
}
