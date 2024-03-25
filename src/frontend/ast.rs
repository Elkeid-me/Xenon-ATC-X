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

pub type TranslationUnit = Vec<Box<GlobalItem>>;

#[derive(Debug)]
pub enum GlobalItem {
    Def(Definition),
    FuncDef {
        return_void: bool,
        id: String,
        parameter_list: Vec<Parameter>,
        block: Block,
    },
}

#[derive(Debug)]
pub enum Parameter {
    Int(String),
    PointerTmp(String, Vec<Expr>),
    Pointer(String, Vec<usize>),
}

#[derive(Debug)]
pub enum Definition {
    ConstVariableDefTmp(String, Expr),
    ConstVariableDef(String, i32),
    ConstArrayDefTmp {
        id: String,
        lengths: Vec<Expr>,
        init_list: InitList,
    },
    ConstArrayDef {
        id: String,
        lengths: Vec<usize>,
        init_list: ConstInitList,
    },
    VariableDef(String, Option<Expr>),
    ArrayDefTmp {
        id: String,
        lengths: Vec<Expr>,
        init_list: Option<InitList>,
    },
    ArrayDef {
        id: String,
        lengths: Vec<usize>,
        init_list: Option<InitList>,
    },
}

pub type InitList = Vec<InitListItem>;

#[derive(Debug)]
pub enum InitListItem {
    InitList(Box<InitList>),
    Expr(Expr),
}

pub type ConstInitList = Vec<ConstInitListItem>;

#[derive(Debug)]
pub enum ConstInitListItem {
    InitList(Box<ConstInitList>),
    Num(i32),
}

#[derive(Debug)]
pub enum Statement {
    Expr(Expr),
    If {
        condition: Expr,
        then_block: Box<Block>,
        else_block: Box<Block>,
    },
    While {
        condition: Expr,
        block: Box<Block>,
    },
    Return(Option<Expr>),
    Break,
    Continue,
}

pub type Block = Vec<BlockItem>;

#[derive(Debug)]
pub enum BlockItem {
    Def(Box<Definition>),
    Block(Box<Block>),
    Statement(Box<Statement>),
}

#[derive(Debug)]
pub enum AssignOp {
    Assignment,
    AddAssign,
    SubtractAssign,
    MultiplyAssign,
    BitAndAssign,
    BitOrAssign,
    BitXorAssign,
    BitLeftShiftAssign,
    BitRightShiftAssign,
}

#[derive(Debug)]
pub enum ArithmeticOp {
    Multiply,
    Divide,
    Modulus,
    Add,
    Subtract,

    BitLeftShift,
    BitRightShift,
    BirXor,
    BitAnd,
    BitOr,

    Equal,
    NotEqual,
    Greater,
    GreaterOrEqual,
    Less,
    LessOrEqual,
}

#[derive(Debug)]
pub enum LogicOp {
    LogicalAnd,
    LogicalOr,
}

#[derive(Debug)]
pub enum InfixOp {
    Assign(AssignOp),
    Arith(ArithmeticOp),
    Logic(LogicOp),
}

#[derive(Debug)]
pub enum ArithmeticUnaryOp {
    LogicalNot,
    Negative,
    BitNot,
}

#[derive(Debug)]
pub enum OtherUnaryOp {
    PostfixSelfIncrease,
    PostfixSelfDecrease,
    PrefixSelfIncrease,
    PrefixSelfDecrease,
}

#[derive(Debug)]
pub enum UnaryOp {
    ArithUnary(ArithmeticUnaryOp),
    Others(OtherUnaryOp),
}

#[derive(Debug)]
pub enum ExprInner {
    InfixExpr(Box<Expr>, InfixOp, Box<Expr>),
    UnaryExpr(UnaryOp, Box<Expr>),

    Num(i32),
    Identifier(String),
    FunctionCall(String, Vec<Expr>),
    ArrayElement(String, Vec<Expr>, bool),
}

#[derive(Debug, Clone, Copy)]
pub enum SimpleType {
    Int,
    Pointer,
    Void,
}

#[derive(Debug)]
pub struct Expr {
    pub inner: ExprInner,
    pub type_: SimpleType,
}

impl From<ExprInner> for Expr {
    fn from(inner: ExprInner) -> Self {
        Self {
            inner,
            type_: SimpleType::Void,
        }
    }
}

impl Default for Expr {
    fn default() -> Self {
        Self {
            inner: ExprInner::Num(0),
            type_: SimpleType::Void,
        }
    }
}
