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

use super::ty::Type;
type HashMap<K, V> = rustc_hash::FxHashMap<K, V>;

pub type Handler = usize;

pub struct TranslationUnit {
    pub ast: Vec<Definition>,
    pub types: HashMap<Handler, Type>,
    pub inits: HashMap<Handler, Option<Init>>,
}

pub type Definition = (Handler, String);

/// # 初始化器
/// - [`Init::Function`] 是二元组，前者为每个形参的重整化后名字，后者为函数体.
/// - [`Init::Expr`]，表示这个变量使用一个表达式初始化，即这是一个整型变量.
/// - [`Init::Const`]，表示这个变量使用一个整型常量初始化，即这是一个整型常量.
/// - [`Init::ConstList`] 和 [`Init::List`] 同理.
pub enum Init {
    Function(Vec<Option<String>>, Block),
    Expr(Expr),
    Const(i32),
    List(InitList),
    ConstList(ConstInitList),
}

impl Default for Init {
    fn default() -> Self {
        Self::Const(0)
    }
}

#[derive(Clone)]
pub enum InitListItem {
    InitList(Box<InitList>),
    Expr(Expr),
}

pub type InitList = Vec<InitListItem>;

#[derive(Clone)]
pub enum ConstInitListItem {
    ConstInitList(Box<ConstInitList>),
    Num(i32),
}

pub type ConstInitList = Vec<ConstInitListItem>;

pub enum Statement {
    Expr(Expr),
    If(Expr, Box<Block>, Box<Block>),
    While(Expr, Box<Block>),
    Return(Option<Expr>),
    Break,
    Continue,
}

pub type Block = Vec<BlockItem>;

pub enum BlockItem {
    Def(Definition),
    Block(Block),
    Statement(Statement),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),

    ShL(Box<Expr>, Box<Expr>),
    ShR(Box<Expr>, Box<Expr>),
    Xor(Box<Expr>, Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),

    Eq(Box<Expr>, Box<Expr>),
    Neq(Box<Expr>, Box<Expr>),
    Grt(Box<Expr>, Box<Expr>),
    Geq(Box<Expr>, Box<Expr>),
    Les(Box<Expr>, Box<Expr>),
    Leq(Box<Expr>, Box<Expr>),

    LogicAnd(Box<Expr>, Box<Expr>),
    LogicOr(Box<Expr>, Box<Expr>),

    LogicNot(Box<Expr>),
    Nega(Box<Expr>),
    Not(Box<Expr>),

    PostInc(Box<Expr>),
    PostDec(Box<Expr>),
    PreInc(Box<Expr>),
    PreDec(Box<Expr>),

    Assignment(Box<Expr>, Box<Expr>),
    AddAssign(Box<Expr>, Box<Expr>),
    SubAssign(Box<Expr>, Box<Expr>),
    MulAssign(Box<Expr>, Box<Expr>),
    DivAssign(Box<Expr>, Box<Expr>),
    ModAssign(Box<Expr>, Box<Expr>),
    AndAssign(Box<Expr>, Box<Expr>),
    OrAssign(Box<Expr>, Box<Expr>),
    XorAssign(Box<Expr>, Box<Expr>),
    ShLAssign(Box<Expr>, Box<Expr>),
    SaRAssign(Box<Expr>, Box<Expr>),

    Num(i32),
    Var(String),
    Func(String, Vec<Expr>),
    Array(String, Vec<Expr>, bool),
}

#[derive(Clone, Copy)]
pub enum ExprCategory {
    LValue,
    RValue,
}

#[derive(Clone, Copy)]
pub enum ExprConst {
    ConstEval,
    NonConst,
}

impl Expr {
    pub fn get_num(&self) -> i32 {
        match self {
            Self::Num(i) => *i,
            _ => unreachable!(),
        }
    }
}

impl std::ops::BitAnd for ExprConst {
    type Output = ExprConst;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::ConstEval, Self::ConstEval) => Self::ConstEval,
            (Self::ConstEval, Self::NonConst) | (Self::NonConst, Self::ConstEval) | (Self::NonConst, Self::NonConst) => {
                Self::NonConst
            }
        }
    }
}
