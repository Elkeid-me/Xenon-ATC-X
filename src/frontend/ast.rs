use super::ty::Type;

pub type TranslationUnit = Vec<Definition>;
pub type Definition = (Type, String, Option<Init>);

/// # 初始化器
/// - [`Init::Function`] 是二元组，前者为每个形参的重整化后名字，后者为函数体.
/// - [`Init::Expr`]，表示这个变量使用一个表达式初始化，即这是一个整型变量.
/// - [`Init::Const`]，表示这个变量使用一个整型常量初始化，即这是一个整型常量.
/// - [`Init::ConstInitList`] 和 [`Init::InitList`] 同理.
#[derive(Debug)]
pub enum Init {
    Function(Vec<String>, Block),
    Expr(Expr),
    Const(i32),
    InitList(InitList),
    ConstInitList(ConstInitList),
}

#[derive(Debug)]
pub enum InitListItem {
    InitList(Box<InitList>),
    Expr(Expr),
}

pub type InitList = Vec<InitListItem>;

#[derive(Debug, Clone)]
pub enum ConstInitListItem {
    ConstInitList(Box<ConstInitList>),
    Num(i32),
}

pub type ConstInitList = Vec<ConstInitListItem>;

#[derive(Debug)]
pub enum Statement {
    Expr(Expr),
    If(Expr, Box<Block>, Box<Block>),
    While(Expr, Box<Block>),
    Return(Option<Expr>),
    Break,
    Continue,
}

pub type Block = Vec<BlockItem>;

#[derive(Debug)]
pub enum BlockItem {
    Def(Definition),
    Block(Block),
    Statement(Statement),
}

#[derive(Debug)]
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
    AndAssign(Box<Expr>, Box<Expr>),
    OrAssign(Box<Expr>, Box<Expr>),
    XorAssign(Box<Expr>, Box<Expr>),
    ShLAssign(Box<Expr>, Box<Expr>),
    SaRAssign(Box<Expr>, Box<Expr>),

    Num(i32),
    Var(String),
    Func(String, Vec<Expr>),
    Array(String, Vec<Expr>),
}

#[derive(Debug, Clone, Copy)]
pub enum ExprCategory {
    LValue,
    RValue,
}

#[derive(Debug, Clone, Copy)]
pub enum ExprConst {
    ConstEval,
    NonConst,
}

impl Expr {
    pub fn get_num(&self) -> i32 {
        match self {
            Expr::Num(i) => *i,
            _ => unreachable!(),
        }
    }
}

impl std::ops::BitAnd for ExprConst {
    type Output = ExprConst;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (ExprConst::ConstEval, ExprConst::ConstEval) => ExprConst::ConstEval,
            (ExprConst::ConstEval, ExprConst::NonConst)
            | (ExprConst::NonConst, ExprConst::ConstEval)
            | (ExprConst::NonConst, ExprConst::NonConst) => ExprConst::NonConst,
        }
    }
}
