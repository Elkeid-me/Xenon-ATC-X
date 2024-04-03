mod asm_generator;
mod risc_v;

pub fn generate_asm(ir: String) -> String {
    let driver = koopa::front::Driver::from(ir);
    let program = driver.generate_program().unwrap();
    for i in program.funcs() {
        dbg!(i.1.name());
    }
    todo!()
}