mod asm_generator;
mod risc_v;

use generator::{done, Gn};
use risc_v::{Inst::*, RiscV, RiscVItem::*};

fn post_process(rv: RiscV) -> RiscV {
    let mut g = Gn::new_scoped(move |mut s| {
        let mut iter = rv.into_iter();
        loop {
            match iter.next() {
                Some(Inst(Bnez(reg, label_1))) => match iter.next() {
                    Some(Inst(J(label_2))) => match iter.next() {
                        Some(Label(label_3)) if label_2 == label_3 => {
                            s.yield_(Inst(Bnez(reg, label_1)));
                            s.yield_(Label(label_3));
                        }
                        Some(Label(label_3)) if label_1 == label_3 => {
                            s.yield_(Inst(Beqz(reg, label_2)));
                            s.yield_(Label(label_3));
                        }
                        Some(i) => {
                            s.yield_(Inst(Bnez(reg, label_1)));
                            s.yield_(Inst(J(label_2)));
                            s.yield_(i);
                        }
                        None => {
                            s.yield_(Inst(Bnez(reg, label_1)));
                            s.yield_(Inst(J(label_2)));
                            break;
                        }
                    },
                    Some(i) => {
                        s.yield_(Inst(Beqz(reg, label_1)));
                        s.yield_(i);
                    }
                    None => {
                        s.yield_(Inst(Beqz(reg, label_1)));
                        break;
                    }
                },
                Some(Inst(J(label_1))) => match iter.next() {
                    Some(Label(label_2)) if label_1 == label_2 => {
                        s.yield_(Label(label_2));
                    }
                    Some(i) => {
                        s.yield_(Inst(J(label_1)));
                        s.yield_(i);
                    }
                    None => {
                        s.yield_(Inst(J(label_1)));
                        break;
                    }
                },
                Some(i) => {
                    s.yield_(i);
                }
                None => break,
            }
        }
        done!()
    });
    Gn::new_scoped(move |mut s| {
        loop {
            match g.next() {
                Some(Inst(Sw(rd_1, offset_1, rs_1))) => match g.next() {
                    Some(Inst(Lw(rd_2, offset_2, rs_2))) if offset_1 == offset_2 && rs_1 == rs_2 => {
                        s.yield_(Inst(Sw(rd_1, offset_1, rs_1)));
                        if rd_1 != rd_2 {
                            s.yield_(Inst(Mv(rd_2, rd_1)));
                        }
                    }
                    Some(i) => {
                        s.yield_(Inst(Sw(rd_1, offset_1, rs_1)));
                        s.yield_(i);
                    }
                    None => {
                        s.yield_(Inst(Sw(rd_1, offset_1, rs_1)));
                        break;
                    }
                },
                Some(i) => {
                    s.yield_(i);
                }
                None => break,
            }
        }
        done!()
    })
    .collect()
}

pub fn generate_asm(ir: String) -> RiscV {
    let driver = koopa::front::Driver::from(ir);
    let program = driver.generate_program().unwrap();
    post_process(asm_generator::generate(program))
}
