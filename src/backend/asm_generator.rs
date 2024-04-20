use super::risc_v::*;
use crate::risk;
use koopa::ir::{entities::ValueData, BasicBlock, BinaryOp, FunctionData, Program, Type, TypeKind, Value, ValueKind::*};
use std::{cmp::max, collections::HashMap, vec};

const CALL_REGS: [Reg; 8] = [Reg::A0, Reg::A1, Reg::A2, Reg::A3, Reg::A4, Reg::A5, Reg::A6, Reg::A7];

struct Context {
    vars: HashMap<Value, i32>,
    labels: HashMap<BasicBlock, String>,
    frame_size: i32,
    save_ra: bool,
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
    let args_size = calls.iter().map(|len| max(len - 8, 0)).max().unwrap_or(0) * 4;
    frame_size += args_size;
    let mut vars = HashMap::new();
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
    Context { vars, labels, frame_size, save_ra }
}

fn load_value(
    global_vars: &HashMap<Value, String>,
    context: &Context,
    func_data: &FunctionData,
    value: Value,
    reg: &mut Reg,
) -> RiscV {
    match func_data.dfg().value(value).kind() {
        Integer(i) if i.value() == 0 => {
            *reg = Reg::Zero;
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
                if offset <= 2047 {
                    vec![RiscVItem::Inst(Inst::Lw(*reg, offset, Reg::Sp))]
                } else {
                    vec![
                        RiscVItem::Inst(Inst::Li(*reg, offset)),
                        RiscVItem::Inst(Inst::Add(*reg, Reg::Sp, *reg)),
                        RiscVItem::Inst(Inst::Lw(*reg, 0, *reg)),
                    ]
                }
            }
        }
        _ => {
            let offset = *context.vars.get(&value).unwrap();
            if offset <= 2047 {
                vec![RiscVItem::Inst(Inst::Lw(*reg, offset, Reg::Sp))]
            } else {
                vec![
                    RiscVItem::Inst(Inst::Li(*reg, offset)),
                    RiscVItem::Inst(Inst::Add(*reg, Reg::Sp, *reg)),
                    RiscVItem::Inst(Inst::Lw(*reg, 0, *reg)),
                ]
            }
        }
    }
}

fn store_value(global_vars: &HashMap<Value, String>, context: &Context, func_data: &FunctionData, value: Value, reg: Reg) -> RiscV {
    let offset = *context.vars.get(&value).unwrap();
    if offset <= 2047 {
        vec![RiscVItem::Inst(Inst::Sw(reg, offset, Reg::Sp))]
    } else {
        vec![
            RiscVItem::Inst(Inst::Li(Reg::T3, offset)),
            RiscVItem::Inst(Inst::Add(Reg::T3, Reg::Sp, Reg::T3)),
            RiscVItem::Inst(Inst::Sw(reg, 0, Reg::T3)),
        ]
    }
}

fn get_ptr(
    ir: &Program,
    global_vars: &HashMap<Value, String>,
    context: &Context,
    func_data: &FunctionData,
    base: Value,
    index: Value,
    step: i32,
) -> (RiscV, Reg) {
    let mut insts = RiscV::new();
    let mut base_reg = Reg::T0;
    let mut index_reg = Reg::T1;
    let step_reg = Reg::T2;
    if global_vars.contains_key(&base) {
        let addr = global_vars.get(&base).unwrap();
        insts.add_inst(Inst::La(Reg::T0, addr.clone()))
    } else {
        match func_data.dfg().value(base).kind() {
            Alloc(_) => {
                let offset = *context.vars.get(&base).unwrap();
                if offset <= 2047 {
                    insts.add_inst(Inst::Addi(base_reg, Reg::Sp, offset));
                } else {
                    insts.add_inst(Inst::Li(base_reg, offset));
                    insts.add_inst(Inst::Add(base_reg, Reg::Sp, base_reg));
                }
            }
            _ => insts.extend(load_value(global_vars, context, func_data, base, &mut base_reg)),
        }
    }
    insts.extend(load_value(global_vars, context, func_data, index, &mut index_reg));
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
                insts.add_inst(Inst::La(Reg::T0, addr.clone()));
                insts.add_inst(Inst::Lw(Reg::T0, 0, Reg::T0));
            } else {
                match func_data.dfg().value(src).kind() {
                    Alloc(_) => {
                        let offset = *context.vars.get(&src).unwrap();
                        if offset <= 2047 {
                            insts.add_inst(Inst::Lw(Reg::T0, offset, Reg::Sp));
                        } else {
                            insts.add_inst(Inst::Li(Reg::T0, offset));
                            insts.add_inst(Inst::Add(Reg::T0, Reg::Sp, Reg::T0));
                            insts.add_inst(Inst::Lw(Reg::T0, 0, Reg::T0));
                        }
                    }
                    _ => {
                        let offset = *context.vars.get(&src).unwrap();
                        if offset <= 2047 {
                            insts.add_inst(Inst::Lw(Reg::T0, offset, Reg::Sp));
                            insts.add_inst(Inst::Lw(Reg::T0, 0, Reg::T0));
                        } else {
                            insts.add_inst(Inst::Li(Reg::T0, offset));
                            insts.add_inst(Inst::Add(Reg::T0, Reg::Sp, Reg::T0));
                            insts.add_inst(Inst::Lw(Reg::T0, 0, Reg::T0));
                            insts.add_inst(Inst::Lw(Reg::T0, 0, Reg::T0));
                        }
                    }
                }
            }
            let offset = *context.vars.get(&value).unwrap();
            if offset <= 2047 {
                insts.add_inst(Inst::Sw(Reg::T0, offset, Reg::Sp));
            } else {
                insts.add_inst(Inst::Li(Reg::T1, offset));
                insts.add_inst(Inst::Add(Reg::T1, Reg::Sp, Reg::T1));
                insts.add_inst(Inst::Sw(Reg::T0, 0, Reg::T1));
            }
        }
        Store(store) => {
            let src = store.value();
            let dst = store.dest();
            let mut src_reg = Reg::T0;
            insts.extend(load_value(global_vars, context, func_data, src, &mut src_reg));
            if global_vars.contains_key(&dst) {
                let addr = global_vars.get(&dst).unwrap();
                insts.add_inst(Inst::La(Reg::T1, addr.clone()));
                insts.add_inst(Inst::Sw(src_reg, 0, Reg::T1));
            } else {
                match func_data.dfg().value(dst).kind() {
                    Alloc(_) => {
                        let offset = *context.vars.get(&dst).unwrap();
                        if offset <= 2047 {
                            insts.add_inst(Inst::Sw(src_reg, offset, Reg::Sp));
                        } else {
                            insts.add_inst(Inst::Li(Reg::T1, offset));
                            insts.add_inst(Inst::Add(Reg::T1, Reg::Sp, Reg::T1));
                            insts.add_inst(Inst::Sw(src_reg, 0, Reg::T1));
                        }
                    }
                    _ => {
                        let offset = *context.vars.get(&dst).unwrap();
                        if offset <= 2047 {
                            insts.add_inst(Inst::Lw(Reg::T1, offset, Reg::Sp));
                            insts.add_inst(Inst::Sw(src_reg, 0, Reg::T1));
                        } else {
                            insts.add_inst(Inst::Li(Reg::T1, offset));
                            insts.add_inst(Inst::Add(Reg::T1, Reg::Sp, Reg::T1));
                            insts.add_inst(Inst::Lw(Reg::T1, 0, Reg::T1));
                            insts.add_inst(Inst::Sw(src_reg, 0, Reg::T1));
                        }
                    }
                }
            }
        }
        GetPtr(ptr) => {
            let base = ptr.src();
            let index = ptr.index();
            let step: usize = risk!(value_type.kind(), TypeKind::Pointer(base) => base.size());
            let (inst, base_reg) = get_ptr(ir, global_vars, context, func_data, base, index, step as i32);
            insts.extend(inst);
            insts.extend(store_value(global_vars, context, func_data, value, base_reg));
        }
        GetElemPtr(ptr) => {
            let base = ptr.src();
            let index = ptr.index();
            let step = risk!(value_type.kind(), TypeKind::Pointer(base) => base.size());
            let (inst, base_reg) = get_ptr(ir, global_vars, context, func_data, base, index, step as i32);
            insts.extend(inst);
            insts.extend(store_value(global_vars, context, func_data, value, base_reg));
        }
        Binary(binary) => {
            let lhs = binary.lhs();
            let mut rs1 = Reg::T0;
            insts.extend(load_value(global_vars, context, func_data, lhs, &mut rs1));
            let rhs = binary.rhs();
            let mut rs2 = Reg::T1;
            insts.extend(load_value(global_vars, context, func_data, rhs, &mut rs2));
            let rd = Reg::T2;
            match binary.op() {
                BinaryOp::NotEq => {
                    insts.add_inst(Inst::Xor(rd, rs1, rs2));
                    insts.add_inst(Inst::Snez(rd, rd));
                }
                BinaryOp::Eq => {
                    insts.add_inst(Inst::Xor(rd, rs1, rs2));
                    insts.add_inst(Inst::Seqz(rd, rd));
                }
                BinaryOp::Gt => insts.add_inst(Inst::Sgt(rd, rs1, rs2)),
                BinaryOp::Lt => insts.add_inst(Inst::Slt(rd, rs1, rs2)),
                BinaryOp::Ge => {
                    insts.add_inst(Inst::Slt(rd, rs1, rs2));
                    insts.add_inst(Inst::Seqz(rd, rd));
                }
                BinaryOp::Le => {
                    insts.add_inst(Inst::Sgt(rd, rs1, rs2));
                    insts.add_inst(Inst::Seqz(rd, rd));
                }
                BinaryOp::Add => insts.add_inst(Inst::Add(rd, rs1, rs2)),
                BinaryOp::Sub => insts.add_inst(Inst::Sub(rd, rs1, rs2)),
                BinaryOp::Mul => insts.add_inst(Inst::Mul(rd, rs1, rs2)),
                BinaryOp::Div => insts.add_inst(Inst::Div(rd, rs1, rs2)),
                BinaryOp::Mod => insts.add_inst(Inst::Rem(rd, rs1, rs2)),
                BinaryOp::And => insts.add_inst(Inst::And(rd, rs1, rs2)),
                BinaryOp::Or => insts.add_inst(Inst::Or(rd, rs1, rs2)),
                BinaryOp::Xor => insts.add_inst(Inst::Xor(rd, rs1, rs2)),
                BinaryOp::Shl => insts.add_inst(Inst::Sll(rd, rs1, rs2)),
                BinaryOp::Shr => insts.add_inst(Inst::Srl(rd, rs1, rs2)),
                BinaryOp::Sar => insts.add_inst(Inst::Sra(rd, rs1, rs2)),
            }
            insts.extend(store_value(global_vars, context, func_data, value, rd));
        }
        Branch(branch) => {
            let cond = branch.cond();
            let mut rs = Reg::T0;
            insts.extend(load_value(global_vars, context, func_data, cond, &mut rs));
            let true_label = context.labels.get(&branch.true_bb()).unwrap().clone();
            let false_label = context.labels.get(&branch.false_bb()).unwrap().clone();
            insts.add_inst(Inst::Bnez(rs, true_label));
            insts.add_inst(Inst::J(false_label));
        }
        Jump(jump) => {
            let label = context.labels.get(&jump.target()).unwrap();
            insts.add_inst(Inst::J(label.clone()));
        }
        Call(func) => {
            let args = func.args();
            for (&value, &reg) in args.iter().zip(CALL_REGS.iter()) {
                let mut reg_ = reg;
                insts.extend(load_value(global_vars, context, func_data, value, &mut reg_));
                if reg_ != reg {
                    insts.add_inst(Inst::Mv(reg, reg_))
                }
            }
            if args.len() > 8 {
                for (i, &arg) in args[8..].iter().enumerate() {
                    let mut reg = Reg::T0;
                    insts.extend(load_value(global_vars, context, func_data, arg, &mut reg));
                    insts.add_inst(Inst::Sw(reg, i as i32 * 4, Reg::Sp));
                }
            }
            insts.add_inst(Inst::Call(ir.func(func.callee()).name()[1..].to_string()));
            if value_type.is_i32() {
                insts.extend(store_value(global_vars, context, func_data, value, Reg::A0));
            }
        }
        Return(ret) => {
            if let Some(value) = ret.value() {
                let mut rd = Reg::A0;
                insts.extend(load_value(global_vars, context, func_data, value, &mut rd));
                if rd != Reg::A0 {
                    insts.add_inst(Inst::Mv(Reg::A0, rd))
                }
            }
            if context.frame_size > 0 {
                if context.frame_size <= 2047 {
                    insts.add_inst(Inst::Addi(Reg::Sp, Reg::Sp, context.frame_size))
                } else {
                    insts.add_inst(Inst::Li(Reg::T0, context.frame_size));
                    insts.add_inst(Inst::Add(Reg::Sp, Reg::Sp, Reg::T0))
                }
            }
            if context.save_ra {
                insts.add_inst(Inst::Lw(Reg::Ra, -4, Reg::Sp))
            }
            insts.add_inst(Inst::Ret)
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
        insts.add_inst(Inst::Sw(Reg::Ra, -4, Reg::Sp));
    }
    if context.frame_size > 0 {
        if context.frame_size <= 2048 {
            insts.add_inst(Inst::Addi(Reg::Sp, Reg::Sp, -context.frame_size));
        } else {
            insts.add_inst(Inst::Li(Reg::T0, context.frame_size));
            insts.add_inst(Inst::Sub(Reg::Sp, Reg::Sp, Reg::T0));
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
    let mut global_vars = HashMap::new();
    let mut global_insts = RiscV::new();
    for (&value, value_data) in ir.borrow_values().iter().filter(|(_, value_data)| matches!(value_data.kind(), GlobalAlloc(_))) {
        global_vars.insert(value, value_data.name().as_ref().unwrap()[1..].to_string());
        global_insts.extend(global_var(&ir, value_data));
    }
    global_insts.into_iter().chain(ir.funcs().values().flat_map(|func_data| func(&ir, func_data, &global_vars))).collect()
}
