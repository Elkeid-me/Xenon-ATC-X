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

use super::risc_v::{Reg::*, *};
use crate::risk;
use koopa::ir::{entities::ValueData, BasicBlock, BinaryOp, FunctionData, Program, Type, TypeKind, Value, ValueKind::*};
use std::{cmp::max, vec};
type HashMap<K, V> = rustc_hash::FxHashMap<K, V>;

const CALL_REGS: [Reg; 8] = [A0, A1, A2, A3, A4, A5, A6, A7];

struct Context {
    vars: HashMap<Value, i32>,
    labels: HashMap<BasicBlock, String>,
    frame_size: i32,
    save_ra: bool,
    save_s0: bool,
}
fn generate_context(func_data: &FunctionData) -> Context {
    let name = &func_data.name()[1..];
    let labels: HashMap<BasicBlock, String> = func_data
        .layout()
        .bbs()
        .keys()
        .map(|&bb| {
            let bb_name = &func_data.dfg().bb(bb).name().clone().unwrap()[1..];
            let label = format!("{name}_{bb_name}");
            (bb, label)
        })
        .collect();
    let mut frame_size = 0;
    let calls: Vec<_> = func_data
        .dfg()
        .values()
        .values()
        .filter_map(|value_data| match value_data.kind() {
            Call(func) => Some(func.args().len() as i32),
            _ => None,
        })
        .collect();
    let args_size = calls.iter().map(|len| max(len - 7, 1)).max().unwrap_or(0) * 4;
    frame_size += args_size;
    let mut vars = HashMap::default();
    for node in func_data.layout().bbs().nodes() {
        for &inst in node.insts().keys() {
            match func_data.dfg().value(inst).kind() {
                Alloc(_) => {
                    let size = risk!(func_data.dfg().value(inst).ty().kind(), TypeKind::Pointer(base) => base.size() as i32);
                    vars.insert(inst, frame_size);
                    frame_size += size;
                }
                _ => {
                    let size = func_data.dfg().value(inst).ty().size() as i32;
                    vars.insert(inst, frame_size);
                    frame_size += size;
                }
            }
        }
    }
    let save_ra = !calls.is_empty();
    if save_ra {
        frame_size += 4;
    }
    frame_size = (frame_size + 15) & !15;
    let save_s0 = frame_size > 2048;
    Context { vars, labels, frame_size, save_ra, save_s0 }
}

fn load_value(context: &Context, func_data: &FunctionData, value: Value, reg: &mut Reg) -> RiscV {
    match func_data.dfg().value(value).kind() {
        Integer(i) if i.value() == 0 => {
            *reg = Zero;
            RiscV::new()
        }
        Integer(i) => vec![RiscVItem::Inst(Inst::Li(*reg, i.value()))],
        FuncArgRef(arg_ref) => {
            let index = arg_ref.index();
            if index < 8 {
                *reg = CALL_REGS[index];
                RiscV::new()
            } else {
                // 第 9 个参数的 index 为 8，而第 9 个参数的 offset 为 0.
                let offset = (index - 8) as i32 * 4 + context.frame_size;
                load_value_offset(context, offset, *reg)
            }
        }
        _ => {
            let offset = *context.vars.get(&value).unwrap();
            load_value_offset(context, offset, *reg)
        }
    }
}

fn store_value_offset(context: &Context, offset: i32, reg: Reg) -> RiscV {
    if context.save_s0 {
        let s0_offset = offset - context.frame_size;
        if offset >= -2048 && offset <= 2047 {
            vec![RiscVItem::Inst(Inst::Sw(reg, offset, Sp))]
        } else if s0_offset >= -2048 && s0_offset <= 2047 {
            vec![RiscVItem::Inst(Inst::Sw(reg, s0_offset, S0))]
        } else if offset >= -4096 && offset < -2048 {
            vec![RiscVItem::Inst(Inst::Addi(T3, Sp, -2048)), RiscVItem::Inst(Inst::Sw(reg, offset + 2048, T3))]
        } else if offset > 2047 && offset <= 4094 {
            vec![RiscVItem::Inst(Inst::Addi(T3, Sp, 2047)), RiscVItem::Inst(Inst::Sw(reg, offset - 2047, T3))]
        } else if s0_offset >= -4096 && s0_offset < -2048 {
            vec![RiscVItem::Inst(Inst::Addi(T3, S0, -2048)), RiscVItem::Inst(Inst::Sw(reg, s0_offset + 2048, T3))]
        } else if s0_offset > 2047 && s0_offset <= 4094 {
            vec![RiscVItem::Inst(Inst::Addi(T3, S0, 2047)), RiscVItem::Inst(Inst::Sw(reg, s0_offset - 2047, T3))]
        } else {
            vec![
                RiscVItem::Inst(Inst::Li(T3, offset)),
                RiscVItem::Inst(Inst::Add(T3, Sp, T3)),
                RiscVItem::Inst(Inst::Sw(reg, 0, T3)),
            ]
        }
    } else {
        if offset >= -2048 && offset <= 2047 {
            vec![RiscVItem::Inst(Inst::Sw(reg, offset, Sp))]
        } else {
            vec![
                RiscVItem::Inst(Inst::Li(T3, offset)),
                RiscVItem::Inst(Inst::Add(T3, Sp, T3)),
                RiscVItem::Inst(Inst::Sw(reg, 0, T3)),
            ]
        }
    }
}

fn load_value_offset(context: &Context, offset: i32, reg: Reg) -> RiscV {
    if context.save_s0 {
        let s0_offset = offset - context.frame_size;
        if offset >= -2048 && offset <= 2047 {
            vec![RiscVItem::Inst(Inst::Lw(reg, offset, Sp))]
        } else if s0_offset >= -2048 && s0_offset <= 2047 {
            vec![RiscVItem::Inst(Inst::Lw(reg, s0_offset, S0))]
        } else if offset >= -4096 && offset < -2048 {
            vec![RiscVItem::Inst(Inst::Addi(T3, Sp, -2048)), RiscVItem::Inst(Inst::Lw(reg, offset + 2048, T3))]
        } else if offset > 2047 && offset <= 4094 {
            vec![RiscVItem::Inst(Inst::Addi(T3, Sp, 2047)), RiscVItem::Inst(Inst::Lw(reg, offset - 2047, T3))]
        } else if s0_offset >= -4096 && s0_offset < -2048 {
            vec![RiscVItem::Inst(Inst::Addi(T3, S0, -2048)), RiscVItem::Inst(Inst::Lw(reg, s0_offset + 2048, T3))]
        } else if s0_offset > 2047 && s0_offset <= 4094 {
            vec![RiscVItem::Inst(Inst::Addi(T3, S0, 2047)), RiscVItem::Inst(Inst::Lw(reg, s0_offset - 2047, T3))]
        } else {
            vec![
                RiscVItem::Inst(Inst::Li(T3, offset)),
                RiscVItem::Inst(Inst::Add(T3, Sp, T3)),
                RiscVItem::Inst(Inst::Lw(reg, 0, T3)),
            ]
        }
    } else {
        if offset >= -2048 && offset <= 2047 {
            vec![RiscVItem::Inst(Inst::Lw(reg, offset, Sp))]
        } else {
            vec![
                RiscVItem::Inst(Inst::Li(T3, offset)),
                RiscVItem::Inst(Inst::Add(T3, Sp, T3)),
                RiscVItem::Inst(Inst::Lw(reg, 0, T3)),
            ]
        }
    }
}

fn store_value(context: &Context, value: Value, reg: Reg) -> RiscV {
    let offset = *context.vars.get(&value).unwrap();
    store_value_offset(context, offset, reg)
}

fn get_ptr(
    global_vars: &HashMap<Value, String>,
    context: &Context,
    func_data: &FunctionData,
    base: Value,
    index: Value,
    step: i32,
) -> (RiscV, Reg) {
    let mut insts = RiscV::new();
    let mut base_reg = T0;
    let mut index_reg = T1;
    let step_reg = T2;
    if global_vars.contains_key(&base) {
        let addr = global_vars.get(&base).unwrap();
        insts.add_inst(Inst::La(T0, addr.clone()))
    } else {
        match func_data.dfg().value(base).kind() {
            Alloc(_) => {
                let offset = *context.vars.get(&base).unwrap();
                if offset <= 2047 {
                    insts.add_inst(Inst::Addi(base_reg, Sp, offset));
                } else {
                    insts.add_inst(Inst::Li(base_reg, offset));
                    insts.add_inst(Inst::Add(base_reg, Sp, base_reg));
                }
            }
            _ => insts.extend(load_value(context, func_data, base, &mut base_reg)),
        }
    }
    insts.extend(load_value(context, func_data, index, &mut index_reg));
    insts.add_inst(Inst::Li(step_reg, step));
    insts.add_inst(Inst::Mul(index_reg, index_reg, step_reg));
    insts.add_inst(Inst::Add(base_reg, base_reg, index_reg));
    (insts, base_reg)
}

fn gen_value(
    ir: &Program,
    global_vars: &HashMap<Value, String>,
    context: &Context,
    func_data: &FunctionData,
    value: Value,
) -> RiscV {
    let mut insts = RiscV::new();
    let value_kind = func_data.dfg().value(value).kind();
    let value_type = func_data.dfg().value(value).ty();
    match value_kind {
        Load(load) => {
            let src = load.src();
            if global_vars.contains_key(&src) {
                let addr = global_vars.get(&src).unwrap();
                insts.add_inst(Inst::La(T0, addr.clone()));
                insts.add_inst(Inst::Lw(T0, 0, T0));
            } else {
                match func_data.dfg().value(src).kind() {
                    Alloc(_) => {
                        let offset = *context.vars.get(&src).unwrap();
                        insts.extend(load_value_offset(context, offset, T0));
                    }
                    _ => {
                        let offset = *context.vars.get(&src).unwrap();
                        insts.extend(load_value_offset(context, offset, T0));
                        insts.add_inst(Inst::Lw(T0, 0, T0));
                    }
                }
            }
            let offset = *context.vars.get(&value).unwrap();
            insts.extend(store_value_offset(context, offset, T0));
        }
        Store(store) => {
            let src = store.value();
            let dst = store.dest();
            let mut src_reg = T0;
            insts.extend(load_value(context, func_data, src, &mut src_reg));
            if global_vars.contains_key(&dst) {
                let addr = global_vars.get(&dst).unwrap();
                insts.add_inst(Inst::La(T1, addr.clone()));
                insts.add_inst(Inst::Sw(src_reg, 0, T1));
            } else {
                match func_data.dfg().value(dst).kind() {
                    Alloc(_) => {
                        let offset = *context.vars.get(&dst).unwrap();
                        insts.extend(store_value_offset(context, offset, src_reg));
                    }
                    _ => {
                        let offset = *context.vars.get(&dst).unwrap();
                        insts.extend(load_value_offset(context, offset, T1));
                        insts.add_inst(Inst::Sw(src_reg, 0, T1));
                    }
                }
            }
        }
        GetPtr(ptr) => {
            let base = ptr.src();
            let index = ptr.index();
            let step = risk!(value_type.kind(), TypeKind::Pointer(base) => base.size());
            let (inst, base_reg) = get_ptr(global_vars, context, func_data, base, index, step as i32);
            insts.extend(inst);
            insts.extend(store_value(context, value, base_reg));
        }
        GetElemPtr(ptr) => {
            let base = ptr.src();
            let index = ptr.index();
            let step = risk!(value_type.kind(), TypeKind::Pointer(base) => base.size());
            let (inst, base_reg) = get_ptr(global_vars, context, func_data, base, index, step as i32);
            insts.extend(inst);
            insts.extend(store_value(context, value, base_reg));
        }
        Binary(binary) => {
            let rd = T2;
            let name = func_data.dfg().value(value).name().as_deref().unwrap();
            let second = name.chars().skip(1).next().unwrap();
            let last = name.split('_').last().unwrap();
            if second == 'L' || last != "br" {
                match (binary.lhs(), binary.op(), binary.rhs()) {
                    (l, BinaryOp::Mul, r) | (r, BinaryOp::Mul, l) if matches!(func_data.dfg().value(l).kind(), Integer(i) if i.value().count_ones() == 1) =>
                    {
                        let imm = risk!(func_data.dfg().value(l).kind(), Integer(i) => i.value());
                        let mut rs = T0;
                        insts.extend(load_value(context, func_data, r, &mut rs));
                        insts.add_inst(Inst::Slli(rd, rs, imm.trailing_zeros() as i32));
                    }
                    (l, BinaryOp::Sub, r) if matches!(func_data.dfg().value(r).kind(), Integer(i) if i.value() >= -2047 && i.value() <= 2048) =>
                    {
                        let imm = risk!(func_data.dfg().value(r).kind(), Integer(i) => i.value());
                        let mut rs = T0;
                        insts.extend(load_value(context, func_data, l, &mut rs));
                        insts.add_inst(Inst::Addi(rd, rs, -imm));
                    }
                    (l, BinaryOp::Shl, r) if matches!(func_data.dfg().value(r).kind(), Integer(_)) => {
                        let imm = risk!(func_data.dfg().value(r).kind(), Integer(i) => i.value());
                        let mut rs = T0;
                        insts.extend(load_value(context, func_data, l, &mut rs));
                        insts.add_inst(Inst::Slli(rd, rs, imm & 31));
                    }
                    (l, BinaryOp::Shr, r) if matches!(func_data.dfg().value(r).kind(), Integer(_)) => {
                        let imm = risk!(func_data.dfg().value(r).kind(), Integer(i) => i.value());
                        let mut rs = T0;
                        insts.extend(load_value(context, func_data, l, &mut rs));
                        insts.add_inst(Inst::Srai(rd, rs, imm & 31));
                    }
                    (l, BinaryOp::Sar, r) if matches!(func_data.dfg().value(r).kind(), Integer(_)) => {
                        let imm = risk!(func_data.dfg().value(l).kind(), Integer(i) => i.value());
                        let mut rs = T0;
                        insts.extend(load_value(context, func_data, r, &mut rs));
                        insts.add_inst(Inst::Srli(rd, rs, imm & 31));
                    }
                    (l, op, r) | (r, op, l)
                        if matches!(func_data.dfg().value(l).kind(), Integer(i) if i.value() >= -2048 && i.value() <= 2047)
                            && matches!(
                                op,
                                BinaryOp::NotEq | BinaryOp::Eq | BinaryOp::Add | BinaryOp::And | BinaryOp::Or | BinaryOp::Xor
                            ) =>
                    {
                        let imm = risk!(func_data.dfg().value(l).kind(), Integer(i) => i.value());
                        let mut rs = T0;
                        insts.extend(load_value(context, func_data, r, &mut rs));
                        match op {
                            BinaryOp::NotEq => {
                                insts.add_inst(Inst::Xori(rd, rs, imm));
                                insts.add_inst(Inst::Snez(rd, rd));
                            }
                            BinaryOp::Eq => {
                                insts.add_inst(Inst::Xori(rd, rs, imm));
                                insts.add_inst(Inst::Seqz(rd, rd));
                            }
                            BinaryOp::Add => insts.add_inst(Inst::Addi(rd, rs, imm)),
                            BinaryOp::And => insts.add_inst(Inst::Andi(rd, rs, imm)),
                            BinaryOp::Or => insts.add_inst(Inst::Ori(rd, rs, imm)),
                            BinaryOp::Xor => insts.add_inst(Inst::Xori(rd, rs, imm)),
                            _ => unreachable!(),
                        }
                    }
                    (l, op, r) => {
                        let mut rs_1 = T0;
                        insts.extend(load_value(context, func_data, l, &mut rs_1));
                        let mut rs_2 = T1;
                        insts.extend(load_value(context, func_data, r, &mut rs_2));
                        match op {
                            BinaryOp::NotEq => {
                                insts.add_inst(Inst::Xor(rd, rs_1, rs_2));
                                insts.add_inst(Inst::Snez(rd, rd));
                            }
                            BinaryOp::Eq => {
                                insts.add_inst(Inst::Xor(rd, rs_1, rs_2));
                                insts.add_inst(Inst::Seqz(rd, rd));
                            }
                            BinaryOp::Gt => insts.add_inst(Inst::Sgt(rd, rs_1, rs_2)),
                            BinaryOp::Lt => insts.add_inst(Inst::Slt(rd, rs_1, rs_2)),
                            BinaryOp::Ge => {
                                insts.add_inst(Inst::Slt(rd, rs_1, rs_2));
                                insts.add_inst(Inst::Seqz(rd, rd));
                            }
                            BinaryOp::Le => {
                                insts.add_inst(Inst::Sgt(rd, rs_1, rs_2));
                                insts.add_inst(Inst::Seqz(rd, rd));
                            }
                            BinaryOp::Add => insts.add_inst(Inst::Add(rd, rs_1, rs_2)),
                            BinaryOp::Sub => insts.add_inst(Inst::Sub(rd, rs_1, rs_2)),
                            BinaryOp::Mul => insts.add_inst(Inst::Mul(rd, rs_1, rs_2)),
                            BinaryOp::Div => insts.add_inst(Inst::Div(rd, rs_1, rs_2)),
                            BinaryOp::Mod => insts.add_inst(Inst::Rem(rd, rs_1, rs_2)),
                            BinaryOp::And => insts.add_inst(Inst::And(rd, rs_1, rs_2)),
                            BinaryOp::Or => insts.add_inst(Inst::Or(rd, rs_1, rs_2)),
                            BinaryOp::Xor => insts.add_inst(Inst::Xor(rd, rs_1, rs_2)),
                            BinaryOp::Shl => insts.add_inst(Inst::Sll(rd, rs_1, rs_2)),
                            BinaryOp::Shr => insts.add_inst(Inst::Srl(rd, rs_1, rs_2)),
                            BinaryOp::Sar => insts.add_inst(Inst::Sra(rd, rs_1, rs_2)),
                        }
                    }
                }
                insts.extend(store_value(context, value, rd));
            }
        }
        Branch(branch) => {
            let cond = branch.cond();
            let name = func_data.dfg().value(cond).name().as_deref().unwrap();
            let second = name.chars().skip(1).next().unwrap();
            let last = name.split('_').last().unwrap();
            let true_label = context.labels.get(&branch.true_bb()).unwrap().clone();
            let false_label = context.labels.get(&branch.false_bb()).unwrap().clone();
            if second == 'L' || last != "br" {
                let mut rs = T0;
                insts.extend(load_value(context, func_data, cond, &mut rs));
                insts.add_inst(Inst::Bne(rs, Zero, true_label));
                insts.add_inst(Inst::J(false_label));
            } else {
                let binary = risk!(func_data.dfg().value(cond).kind(), Binary(binary) => binary);
                match (binary.lhs(), binary.op(), binary.rhs()) {
                    (l, op, r) => {
                        let mut rs_1 = T0;
                        insts.extend(load_value(context, func_data, l, &mut rs_1));
                        let mut rs_2 = T1;
                        insts.extend(load_value(context, func_data, r, &mut rs_2));
                        match op {
                            BinaryOp::NotEq => {
                                insts.add_inst(Inst::Bne(rs_1, rs_2, true_label));
                                insts.add_inst(Inst::J(false_label));
                            }
                            BinaryOp::Eq => {
                                insts.add_inst(Inst::Beq(rs_1, rs_2, true_label));
                                insts.add_inst(Inst::J(false_label));
                            }
                            BinaryOp::Gt => {
                                insts.add_inst(Inst::Bgt(rs_1, rs_2, true_label));
                                insts.add_inst(Inst::J(false_label));
                            }
                            BinaryOp::Lt => {
                                insts.add_inst(Inst::Blt(rs_1, rs_2, true_label));
                                insts.add_inst(Inst::J(false_label));
                            }
                            BinaryOp::Ge => {
                                insts.add_inst(Inst::Bge(rs_1, rs_2, true_label));
                                insts.add_inst(Inst::J(false_label));
                            }
                            BinaryOp::Le => {
                                insts.add_inst(Inst::Ble(rs_1, rs_2, true_label));
                                insts.add_inst(Inst::J(false_label));
                            }
                            _ => unreachable!()
                        }
                    }
                }
            }
        }
        Jump(jump) => {
            let label = context.labels.get(&jump.target()).unwrap();
            insts.add_inst(Inst::J(label.clone()));
        }
        Call(func) => {
            let args = func.args();
            for (&value, &reg) in args.iter().zip(CALL_REGS.iter()) {
                let mut reg_ = reg;
                insts.extend(load_value(context, func_data, value, &mut reg_));
                if reg_ != reg {
                    insts.add_inst(Inst::Mv(reg, reg_))
                }
            }
            if args.len() > 8 {
                for (i, &arg) in args[8..].iter().enumerate() {
                    let mut reg = T0;
                    insts.extend(load_value(context, func_data, arg, &mut reg));
                    let offset = i as i32 * 4;
                    insts.extend(store_value_offset(context, offset, T0));
                }
            }
            if context.save_s0 {
                let offset = if args.len() <= 8 { 0 } else { (args.len() - 8) * 4 } as i32;
                insts.extend(store_value_offset(context, offset, S0));
            }
            insts.add_inst(Inst::Call(ir.func(func.callee()).name()[1..].to_string()));
            if value_type.is_i32() {
                insts.extend(store_value(context, value, A0));
            }
            if context.save_s0 {
                let offset = if args.len() <= 8 { 0 } else { (args.len() - 8) * 4 } as i32;
                if offset <= 2047 {
                    insts.add_inst(Inst::Lw(S0, offset, Sp));
                } else {
                    insts.add_inst(Inst::Li(T1, offset));
                    insts.add_inst(Inst::Add(T1, Sp, T1));
                    insts.add_inst(Inst::Lw(S0, 0, T1));
                }
            }
        }
        Return(ret) => {
            if let Some(value) = ret.value() {
                let mut rd = A0;
                insts.extend(load_value(context, func_data, value, &mut rd));
                if rd != A0 {
                    insts.add_inst(Inst::Mv(A0, rd))
                }
            }
            if context.frame_size > 0 {
                if context.frame_size <= 2047 {
                    insts.add_inst(Inst::Addi(Sp, Sp, context.frame_size))
                } else {
                    insts.add_inst(Inst::Li(T0, context.frame_size));
                    insts.add_inst(Inst::Add(Sp, Sp, T0))
                }
            }
            if context.save_ra {
                insts.add_inst(Inst::Lw(Ra, -4, Sp))
            }
            insts.add_inst(Inst::Jr(Ra))
        }
        _ => (),
    }
    insts
}
fn func(ir: &Program, func_data: &FunctionData, global_vars: &HashMap<Value, String>) -> RiscV {
    if func_data.layout().entry_bb().is_none() {
        return RiscV::new();
    }
    let context = generate_context(func_data);
    let mut insts = RiscV::new();
    let name = &func_data.name()[1..];
    insts.add_directive(Directive::Text);
    insts.add_directive(Directive::Global(name.to_string()));
    insts.add_label(name.to_string());
    if context.save_ra {
        insts.add_inst(Inst::Sw(Ra, -4, Sp));
    }
    if context.frame_size > 0 {
        if !context.save_s0 {
            insts.add_inst(Inst::Addi(Sp, Sp, -context.frame_size));
        } else {
            insts.add_inst(Inst::Mv(S0, Sp));
            insts.add_inst(Inst::Li(T0, context.frame_size));
            insts.add_inst(Inst::Sub(Sp, Sp, T0));
        }
    }
    for (bb, node) in func_data.layout().bbs() {
        let label = context.labels.get(bb).unwrap();
        insts.add_label(label.clone());
        insts.extend(node.insts().keys().flat_map(|&value| gen_value(ir, global_vars, &context, func_data, value)));
    }
    insts
}
fn flatten_init_list(ir: &Program, init: Value) -> Vec<i32> {
    match ir.borrow_value(init).kind() {
        Integer(i) => vec![i.value()],
        Aggregate(aggs) => aggs.elems().iter().flat_map(|init| flatten_init_list(ir, *init)).collect(),
        _ => unreachable!(),
    }
}
fn global_var(ir: &Program, value_data: &ValueData) -> RiscV {
    let mut insts = RiscV::new();
    let init = risk!(value_data.kind(), GlobalAlloc(alloc) => alloc.init());
    let name = &value_data.name().as_deref().unwrap()[1..];
    insts.add_directive(Directive::Data);
    insts.add_directive(Directive::Global(name.to_string()));
    insts.add_label(name.to_string());
    match ir.borrow_value(init).kind() {
        ZeroInit(_) => {
            let size = risk!(value_data.ty().kind(), TypeKind::Pointer(base) => base.size());
            insts.add_directive(Directive::Zero(size));
        }
        _ => insts.add_directive(Directive::Word(flatten_init_list(ir, init))),
    }
    insts
}
pub fn generate(ir: Program) -> RiscV {
    Type::set_ptr_size(4);
    let mut global_vars = HashMap::default();
    let mut global_insts = RiscV::new();
    for (&value, value_data) in ir.borrow_values().iter().filter(|(_, value_data)| matches!(value_data.kind(), GlobalAlloc(_))) {
        global_vars.insert(value, value_data.name().as_ref().unwrap()[1..].to_string());
        global_insts.extend(global_var(&ir, value_data));
    }
    global_insts.into_iter().chain(ir.funcs().values().flat_map(|func_data| func(&ir, func_data, &global_vars))).collect()
}
