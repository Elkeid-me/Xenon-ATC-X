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

mod asm_generator;
mod risc_v;

use generator::{done, Gn};
use risc_v::{Inst::*, Reg::*, RiscV, RiscVItem::*};
type HashMap<K, V> = rustc_hash::FxHashMap<K, V>;

fn post_process(rv: RiscV) -> RiscV {
    let mut g = Gn::new_scoped(move |mut s| {
        let mut iter = rv.into_iter();
        loop {
            match iter.next() {
                Some(Inst(Bne(rs_1, rs_2, label_1))) => match iter.next() {
                    Some(Inst(J(label_2))) => match iter.next() {
                        Some(Label(label_3)) if label_2 == label_3 => {
                            s.yield_(Inst(Bne(rs_1, rs_2, label_1)));
                            s.yield_(Label(label_3));
                        }
                        Some(Label(label_3)) if label_1 == label_3 => {
                            s.yield_(Inst(Beq(rs_1, rs_2, label_2)));
                            s.yield_(Label(label_3));
                        }
                        Some(i) => {
                            s.yield_(Inst(Bne(rs_1, rs_2, label_1)));
                            s.yield_(Inst(J(label_2)));
                            s.yield_(i);
                        }
                        None => {
                            s.yield_(Inst(Bne(rs_1, rs_2, label_1)));
                            s.yield_(Inst(J(label_2)));
                            break;
                        }
                    },
                    Some(i) => {
                        s.yield_(Inst(Bne(rs_1, rs_2, label_1)));
                        s.yield_(i);
                    }
                    None => {
                        s.yield_(Inst(Bne(rs_1, rs_2, label_1)));
                        break;
                    }
                },

                Some(Inst(Beq(rs_1, rs_2, label_1))) => match iter.next() {
                    Some(Inst(J(label_2))) => match iter.next() {
                        Some(Label(label_3)) if label_2 == label_3 => {
                            s.yield_(Inst(Beq(rs_1, rs_2, label_1)));
                            s.yield_(Label(label_3));
                        }
                        Some(Label(label_3)) if label_1 == label_3 => {
                            s.yield_(Inst(Bne(rs_1, rs_2, label_2)));
                            s.yield_(Label(label_3));
                        }
                        Some(i) => {
                            s.yield_(Inst(Beq(rs_1, rs_2, label_1)));
                            s.yield_(Inst(J(label_2)));
                            s.yield_(i);
                        }
                        None => {
                            s.yield_(Inst(Beq(rs_1, rs_2, label_1)));
                            s.yield_(Inst(J(label_2)));
                            break;
                        }
                    },
                    Some(i) => {
                        s.yield_(Inst(Beq(rs_1, rs_2, label_1)));
                        s.yield_(i);
                    }
                    None => {
                        s.yield_(Inst(Beq(rs_1, rs_2, label_1)));
                        break;
                    }
                },

                Some(Inst(Blt(rs_1, rs_2, label_1))) => match iter.next() {
                    Some(Inst(J(label_2))) => match iter.next() {
                        Some(Label(label_3)) if label_2 == label_3 => {
                            s.yield_(Inst(Blt(rs_1, rs_2, label_1)));
                            s.yield_(Label(label_3));
                        }
                        Some(Label(label_3)) if label_1 == label_3 => {
                            s.yield_(Inst(Bge(rs_1, rs_2, label_2)));
                            s.yield_(Label(label_3));
                        }
                        Some(i) => {
                            s.yield_(Inst(Blt(rs_1, rs_2, label_1)));
                            s.yield_(Inst(J(label_2)));
                            s.yield_(i);
                        }
                        None => {
                            s.yield_(Inst(Blt(rs_1, rs_2, label_1)));
                            s.yield_(Inst(J(label_2)));
                            break;
                        }
                    },
                    Some(i) => {
                        s.yield_(Inst(Blt(rs_1, rs_2, label_1)));
                        s.yield_(i);
                    }
                    None => {
                        s.yield_(Inst(Blt(rs_1, rs_2, label_1)));
                        break;
                    }
                },

                Some(Inst(Bgt(rs_1, rs_2, label_1))) => match iter.next() {
                    Some(Inst(J(label_2))) => match iter.next() {
                        Some(Label(label_3)) if label_2 == label_3 => {
                            s.yield_(Inst(Bgt(rs_1, rs_2, label_1)));
                            s.yield_(Label(label_3));
                        }
                        Some(Label(label_3)) if label_1 == label_3 => {
                            s.yield_(Inst(Ble(rs_1, rs_2, label_2)));
                            s.yield_(Label(label_3));
                        }
                        Some(i) => {
                            s.yield_(Inst(Bgt(rs_1, rs_2, label_1)));
                            s.yield_(Inst(J(label_2)));
                            s.yield_(i);
                        }
                        None => {
                            s.yield_(Inst(Bgt(rs_1, rs_2, label_1)));
                            s.yield_(Inst(J(label_2)));
                            break;
                        }
                    },
                    Some(i) => {
                        s.yield_(Inst(Bgt(rs_1, rs_2, label_1)));
                        s.yield_(i);
                    }
                    None => {
                        s.yield_(Inst(Bgt(rs_1, rs_2, label_1)));
                        break;
                    }
                },

                Some(Inst(Bge(rs_1, rs_2, label_1))) => match iter.next() {
                    Some(Inst(J(label_2))) => match iter.next() {
                        Some(Label(label_3)) if label_2 == label_3 => {
                            s.yield_(Inst(Bge(rs_1, rs_2, label_1)));
                            s.yield_(Label(label_3));
                        }
                        Some(Label(label_3)) if label_1 == label_3 => {
                            s.yield_(Inst(Blt(rs_1, rs_2, label_2)));
                            s.yield_(Label(label_3));
                        }
                        Some(i) => {
                            s.yield_(Inst(Bge(rs_1, rs_2, label_1)));
                            s.yield_(Inst(J(label_2)));
                            s.yield_(i);
                        }
                        None => {
                            s.yield_(Inst(Bge(rs_1, rs_2, label_1)));
                            s.yield_(Inst(J(label_2)));
                            break;
                        }
                    },
                    Some(i) => {
                        s.yield_(Inst(Bge(rs_1, rs_2, label_1)));
                        s.yield_(i);
                    }
                    None => {
                        s.yield_(Inst(Bge(rs_1, rs_2, label_1)));
                        break;
                    }
                },

                Some(Inst(Ble(rs_1, rs_2, label_1))) => match iter.next() {
                    Some(Inst(J(label_2))) => match iter.next() {
                        Some(Label(label_3)) if label_2 == label_3 => {
                            s.yield_(Inst(Ble(rs_1, rs_2, label_1)));
                            s.yield_(Label(label_3));
                        }
                        Some(Label(label_3)) if label_1 == label_3 => {
                            s.yield_(Inst(Bgt(rs_1, rs_2, label_2)));
                            s.yield_(Label(label_3));
                        }
                        Some(i) => {
                            s.yield_(Inst(Ble(rs_1, rs_2, label_1)));
                            s.yield_(Inst(J(label_2)));
                            s.yield_(i);
                        }
                        None => {
                            s.yield_(Inst(Ble(rs_1, rs_2, label_1)));
                            s.yield_(Inst(J(label_2)));
                            break;
                        }
                    },
                    Some(i) => {
                        s.yield_(Inst(Ble(rs_1, rs_2, label_1)));
                        s.yield_(i);
                    }
                    None => {
                        s.yield_(Inst(Ble(rs_1, rs_2, label_1)));
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
    let g_2 = Gn::new_scoped(move |mut s| {
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
    });
    let v: Vec<_> = g_2.collect();
    let mut index = 0i64;
    let labels: HashMap<_, _> = v
        .iter()
        .filter_map(|i| match i {
            Label(label) => Some((label, index)),
            Inst(Call(_)) | Inst(Li(_, _)) | Inst(La(_, _)) => {
                index += 8;
                None
            }
            Inst(_) => {
                index += 4;
                None
            }
            Directive(_) => None,
        })
        .collect();
    let mut v_2 = RiscV::new();
    let mut index_2 = 0i64;
    for i in v.iter() {
        match i {
            Label(_) => v_2.push(i.clone()),
            Inst(J(label)) => {
                index_2 += 4;
                let offset = labels.get(label).unwrap() - index_2;
                if offset <= 1048570 && offset >= -1048570 {
                    v_2.push(i.clone());
                } else {
                    v_2.push(Inst(La(T4, label.clone())));
                    v_2.push(Inst(Jr(T4)));
                    index_2 += 4;
                }
            }
            Inst(Beq(rs_1, rs_2, label)) => {
                index_2 += 4;
                let offset = labels.get(label).unwrap() - index_2;
                if offset <= 4090 && offset >= -4090 {
                    v_2.push(i.clone());
                } else if offset <= 1048570 && offset >= -1048570 {
                    v_2.push(Inst(Bne(*rs_1, *rs_2, format!("_T_{label}"))));
                    v_2.push(Inst(J(label.clone())));
                    index_2 += 4;
                    v_2.push(Label(format!("_T_{label}")));
                } else {
                    v_2.push(Inst(Bne(*rs_1, *rs_2, format!("_T_{label}"))));
                    v_2.push(Inst(La(T4, label.clone())));
                    v_2.push(Inst(Jr(T4)));
                    index_2 += 8;
                    v_2.push(Label(format!("_T_{label}")));
                }
            }
            Inst(Bne(rs_1, rs_2, label)) => {
                index_2 += 4;
                let offset = labels.get(label).unwrap() - index_2;
                if offset <= 4090 && offset >= -4090 {
                    v_2.push(i.clone());
                } else if offset <= 1048570 && offset >= -1048570 {
                    v_2.push(Inst(Beq(*rs_1, *rs_2, format!("_T_{label}"))));
                    v_2.push(Inst(J(label.clone())));
                    index_2 += 4;
                    v_2.push(Label(format!("_T_{label}")));
                } else {
                    v_2.push(Inst(Beq(*rs_1, *rs_2, format!("_T_{label}"))));
                    v_2.push(Inst(La(T4, label.clone())));
                    v_2.push(Inst(Jr(T4)));
                    index_2 += 8;
                    v_2.push(Label(format!("_T_{label}")));
                }
            }
            Inst(Blt(rs_1, rs_2, label)) => {
                index_2 += 4;
                let offset = labels.get(label).unwrap() - index_2;
                if offset <= 4090 && offset >= -4090 {
                    v_2.push(i.clone());
                } else if offset <= 1048570 && offset >= -1048570 {
                    v_2.push(Inst(Bge(*rs_1, *rs_2, format!("_T_{label}"))));
                    v_2.push(Inst(J(label.clone())));
                    index_2 += 4;
                    v_2.push(Label(format!("_T_{label}")));
                } else {
                    v_2.push(Inst(Bge(*rs_1, *rs_2, format!("_T_{label}"))));
                    v_2.push(Inst(La(T4, label.clone())));
                    v_2.push(Inst(Jr(T4)));
                    index_2 += 8;
                    v_2.push(Label(format!("_T_{label}")));
                }
            }
            Inst(Bgt(rs_1, rs_2, label)) => {
                index_2 += 4;
                let offset = labels.get(label).unwrap() - index_2;
                if offset <= 4090 && offset >= -4090 {
                    v_2.push(i.clone());
                } else if offset <= 1048570 && offset >= -1048570 {
                    v_2.push(Inst(Ble(*rs_1, *rs_2, format!("_T_{label}"))));
                    v_2.push(Inst(J(label.clone())));
                    index_2 += 4;
                    v_2.push(Label(format!("_T_{label}")));
                } else {
                    v_2.push(Inst(Ble(*rs_1, *rs_2, format!("_T_{label}"))));
                    v_2.push(Inst(La(T4, label.clone())));
                    v_2.push(Inst(Jr(T4)));
                    index_2 += 8;
                    v_2.push(Label(format!("_T_{label}")));
                }
            }
            Inst(Ble(rs_1, rs_2, label)) => {
                index_2 += 4;
                let offset = labels.get(label).unwrap() - index_2;
                if offset <= 4090 && offset >= -4090 {
                    v_2.push(i.clone());
                } else if offset <= 1048570 && offset >= -1048570 {
                    v_2.push(Inst(Bgt(*rs_1, *rs_2, format!("_T_{label}"))));
                    v_2.push(Inst(J(label.clone())));
                    index_2 += 4;
                    v_2.push(Label(format!("_T_{label}")));
                } else {
                    v_2.push(Inst(Bgt(*rs_1, *rs_2, format!("_T_{label}"))));
                    v_2.push(Inst(La(T4, label.clone())));
                    v_2.push(Inst(Jr(T4)));
                    index_2 += 8;
                    v_2.push(Label(format!("_T_{label}")));
                }
            }
            Inst(Bge(rs_1, rs_2, label)) => {
                index_2 += 4;
                let offset = labels.get(label).unwrap() - index_2;
                if offset <= 4090 && offset >= -4090 {
                    v_2.push(i.clone());
                } else if offset <= 1048570 && offset >= -1048570 {
                    v_2.push(Inst(Blt(*rs_1, *rs_2, format!("_T_{label}"))));
                    v_2.push(Inst(J(label.clone())));
                    index_2 += 4;
                    v_2.push(Label(format!("_T_{label}")));
                } else {
                    v_2.push(Inst(Blt(*rs_1, *rs_2, format!("_T_{label}"))));
                    v_2.push(Inst(La(T4, label.clone())));
                    v_2.push(Inst(Jr(T4)));
                    index_2 += 8;
                    v_2.push(Label(format!("_T_{label}")));
                }
            }
            Inst(Call(_)) | Inst(Li(_, _)) | Inst(La(_, _)) => {
                index_2 += 8;
                v_2.push(i.clone());
            }
            Inst(_) => {
                index_2 += 4;
                v_2.push(i.clone());
            }
            Directive(_) => v_2.push(i.clone()),
        }
    }
    v_2
}

pub fn generate_asm(ir: String) -> RiscV {
    let driver = koopa::front::Driver::from(ir);
    let program = driver.generate_program().unwrap();
    post_process(asm_generator::generate(program))
}
