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

#![allow(warnings)]
use std::fmt::{Display, Formatter, Result};

#[derive(Clone)]
pub enum Inst {
    Beq(Reg, Reg, String),
    Bne(Reg, Reg, String),
    Blt(Reg, Reg, String),
    Bgt(Reg, Reg, String),
    Ble(Reg, Reg, String),
    Bge(Reg, Reg, String),
    J(String),
    Call(String),
    Jr(Reg),

    Lw(Reg, i32, Reg),
    Sw(Reg, i32, Reg),
    Li(Reg, i32),
    La(Reg, String),
    Mv(Reg, Reg),

    Add(Reg, Reg, Reg),
    Sub(Reg, Reg, Reg),
    Xor(Reg, Reg, Reg),
    Or(Reg, Reg, Reg),
    And(Reg, Reg, Reg),
    Sll(Reg, Reg, Reg),
    Srl(Reg, Reg, Reg),
    Sra(Reg, Reg, Reg),
    Mul(Reg, Reg, Reg),
    Div(Reg, Reg, Reg),
    Rem(Reg, Reg, Reg),

    Addi(Reg, Reg, i32),
    Xori(Reg, Reg, i32),
    Ori(Reg, Reg, i32),
    Andi(Reg, Reg, i32),
    Slli(Reg, Reg, i32),
    Srli(Reg, Reg, i32),
    Srai(Reg, Reg, i32),

    Slt(Reg, Reg, Reg),
    Sgt(Reg, Reg, Reg),
    Seqz(Reg, Reg),
    Snez(Reg, Reg),
}

#[derive(Clone, Copy, PartialEq)]
pub enum Reg {
    Zero,
    Ra,
    Sp,
    Gp,
    Tp,
    T0,
    T1,
    T2,
    S0,
    S1,
    A0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    S2,
    S3,
    S4,
    S5,
    S6,
    S7,
    S8,
    S9,
    S10,
    S11,
    T3,
    T4,
    T5,
    T6,
}

#[derive(Clone)]
pub enum Directive {
    Text,
    Global(String),
    Data,
    Zero(usize),
    Word(Vec<i32>),
}

#[derive(Clone)]
pub enum RiscVItem {
    Label(String),
    Inst(Inst),
    Directive(Directive),
}

pub type RiscV = Vec<RiscVItem>;

impl Display for Reg {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let s = match self {
            Self::Zero => "zero",
            Self::Ra => "ra",
            Self::Sp => "sp",
            Self::Gp => "gp",
            Self::Tp => "tp",
            Self::T0 => "t0",
            Self::T1 => "t1",
            Self::T2 => "t2",
            Self::S0 => "s0",
            Self::S1 => "s1",
            Self::A0 => "a0",
            Self::A1 => "a1",
            Self::A2 => "a2",
            Self::A3 => "a3",
            Self::A4 => "a4",
            Self::A5 => "a5",
            Self::A6 => "a6",
            Self::A7 => "a7",
            Self::S2 => "s2",
            Self::S3 => "s3",
            Self::S4 => "s4",
            Self::S5 => "s5",
            Self::S6 => "s6",
            Self::S7 => "s7",
            Self::S8 => "s8",
            Self::S9 => "s9",
            Self::S10 => "s10",
            Self::S11 => "s11",
            Self::T3 => "t3",
            Self::T4 => "t4",
            Self::T5 => "t5",
            Self::T6 => "t6",
        };
        write!(f, "{s}")
    }
}

pub trait RiscVTrait {
    fn add_label(&mut self, label: String);
    fn add_inst(&mut self, inst: Inst);
    fn add_directive(&mut self, directive: Directive);
}

impl RiscVTrait for RiscV {
    fn add_label(&mut self, label: String) {
        self.push(RiscVItem::Label(label));
    }
    fn add_inst(&mut self, inst: Inst) {
        self.push(RiscVItem::Inst(inst));
    }
    fn add_directive(&mut self, directive: Directive) {
        self.push(RiscVItem::Directive(directive));
    }
}

impl Display for Inst {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Self::Beq(rs_1, rs_2, label) => write!(f, "beq {rs_1}, {rs_2}, {label}"),
            Self::Bne(rs_1, rs_2, label) => write!(f, "bne {rs_1}, {rs_2}, {label}"),
            Self::Blt(rs_1, rs_2, label) => write!(f, "blt {rs_1}, {rs_2}, {label}"),
            Self::Bgt(rs_1, rs_2, label) => write!(f, "bgt {rs_1}, {rs_2}, {label}"),
            Self::Ble(rs_1, rs_2, label) => write!(f, "ble {rs_1}, {rs_2}, {label}"),
            Self::Bge(rs_1, rs_2, label) => write!(f, "bge {rs_1}, {rs_2}, {label}"),
            Self::J(label) => write!(f, "j {label}"),
            Self::Call(label) => write!(f, "call {label}"),

            Self::Lw(rd, offset, rs) => write!(f, "lw {rd}, {offset}({rs})"),
            Self::Sw(rs_1, offset, rs_2) => write!(f, "sw {rs_1}, {offset}({rs_2})"),

            Self::Seqz(rd, rs) => write!(f, "seqz {rd}, {rs}"),
            Self::Snez(rd, rs) => write!(f, "snez {rd}, {rs}"),

            Self::Addi(rd, rs, imm) => write!(f, "addi {rd}, {rs}, {imm}"),
            Self::Xori(rd, rs, imm) => write!(f, "xori {rd}, {rs}, {imm}"),
            Self::Ori(rd, rs, imm) => write!(f, "ori {rd}, {rs}, {imm}"),
            Self::Andi(rd, rs, imm) => write!(f, "andi {rd}, {rs}, {imm}"),
            Self::Slli(rd, rs, imm) => write!(f, "slli {rd}, {rs}, {imm}"),
            Self::Srli(rd, rs, imm) => write!(f, "srli {rd}, {rs}, {imm}"),
            Self::Srai(rd, rs, imm) => write!(f, "srai {rd}, {rs}, {imm}"),

            Self::Add(rd, rs_1, rs_2) => write!(f, "add {rd}, {rs_1}, {rs_2}"),
            Self::Sub(rd, rs_1, rs_2) => write!(f, "sub {rd}, {rs_1}, {rs_2}"),
            Self::Slt(rd, rs_1, rs_2) => write!(f, "slt {rd}, {rs_1}, {rs_2}"),
            Self::Sgt(rd, rs_1, rs_2) => write!(f, "sgt {rd}, {rs_1}, {rs_2}"),
            Self::Xor(rd, rs_1, rs_2) => write!(f, "xor {rd}, {rs_1}, {rs_2}"),
            Self::Or(rd, rs_1, rs_2) => write!(f, "or {rd}, {rs_1}, {rs_2}"),
            Self::And(rd, rs_1, rs_2) => write!(f, "and {rd}, {rs_1}, {rs_2}"),
            Self::Sll(rd, rs_1, rs_2) => write!(f, "sll {rd}, {rs_1}, {rs_2}"),
            Self::Srl(rd, rs_1, rs_2) => write!(f, "srl {rd}, {rs_1}, {rs_2}"),
            Self::Sra(rd, rs_1, rs_2) => write!(f, "sra {rd}, {rs_1}, {rs_2}"),
            Self::Mul(rd, rs_1, rs_2) => write!(f, "mul {rd}, {rs_1}, {rs_2}"),
            Self::Div(rd, rs_1, rs_2) => write!(f, "div {rd}, {rs_1}, {rs_2}"),
            Self::Rem(rd, rs_1, rs_2) => write!(f, "rem {rd}, {rs_1}, {rs_2}"),

            Self::Li(rd, imm) => write!(f, "li {rd}, {imm}"),
            Self::La(rd, address) => write!(f, "la {rd}, {address}"),
            Self::Mv(rd, rs) => write!(f, "mv {rd}, {rs}"),
            Self::Jr(rs) => write!(f, "jr {rs}"),
        }
    }
}

impl Display for Directive {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Self::Text => write!(f, ".text"),
            Self::Global(label) => write!(f, ".globl {label}"),
            Self::Data => write!(f, ".data"),
            Self::Zero(len) => write!(f, ".zero {len}"),
            Self::Word(nums) => {
                let data: Vec<_> = nums.iter().map(i32::to_string).collect();
                write!(f, ".word {}", data.as_slice().join(", "))
            }
        }
    }
}

impl Display for RiscVItem {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Self::Label(label) => writeln!(f, "{label}:"),
            Self::Inst(inst) => writeln!(f, "    {inst}"),
            Self::Directive(directive) => writeln!(f, "{directive}"),
        }
    }
}
