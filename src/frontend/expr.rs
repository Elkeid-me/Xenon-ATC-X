use crate::risk;

use super::ast::{Expr::*, ExprCategory::*, ExprConst::*, *};
use super::parser::{ASTBuilder, ConstInit, Rule, Scope, Symbol};
use super::ty::*;
use pest::iterators::Pair;

impl ASTBuilder {
    fn parse_expr(&self, expr: Pair<Rule>) -> Expr {
        self.expr_parser
            .map_primary(|exp| match exp.as_rule() {
                Rule::expression => self.parse_expr(exp),
                Rule::integer_bin => Num(i32::from_str_radix(&exp.as_str()[2..], 2).unwrap()),
                Rule::integer_oct => Num(i32::from_str_radix(exp.as_str(), 8).unwrap()),
                Rule::integer_dec => Num(i32::from_str_radix(exp.as_str(), 10).unwrap()),
                Rule::integer_hex => Num(i32::from_str_radix(&exp.as_str()[2..], 16).unwrap()),
                Rule::identifier => Var(exp.as_str().to_string()),
                Rule::function_call => {
                    let mut iter = exp.into_inner();
                    Func(iter.next().unwrap().as_str().to_string(), iter.map(|p| self.parse_expr(p)).collect())
                }
                Rule::array_element => {
                    let mut iter = exp.into_inner();
                    Array(
                        iter.next().unwrap().as_str().to_string(),
                        iter.next().unwrap().into_inner().map(|p| self.parse_expr(p)).collect(),
                    )
                }
                _ => unreachable!(),
            })
            .map_infix(|l, op, r| match op.as_rule() {
                Rule::custom => Func(op.into_inner().as_str().to_string(), vec![l, r]),
                Rule::mul => Mul(Box::new(l), Box::new(r)),
                Rule::div => Div(Box::new(l), Box::new(r)),
                Rule::modu => Mod(Box::new(l), Box::new(r)),
                Rule::add => Add(Box::new(l), Box::new(r)),
                Rule::sub => Sub(Box::new(l), Box::new(r)),

                Rule::logic_and => LogicAnd(Box::new(l), Box::new(r)),
                Rule::logic_or => LogicOr(Box::new(l), Box::new(r)),

                Rule::shl => ShL(Box::new(l), Box::new(r)),
                Rule::sar => ShR(Box::new(l), Box::new(r)),
                Rule::xor => Xor(Box::new(l), Box::new(r)),
                Rule::and => And(Box::new(l), Box::new(r)),
                Rule::or => Or(Box::new(l), Box::new(r)),

                Rule::eq => Eq(Box::new(l), Box::new(r)),
                Rule::neq => Neq(Box::new(l), Box::new(r)),
                Rule::grt => Grt(Box::new(l), Box::new(r)),
                Rule::geq => Geq(Box::new(l), Box::new(r)),
                Rule::les => Les(Box::new(l), Box::new(r)),
                Rule::leq => Leq(Box::new(l), Box::new(r)),

                Rule::assignment => Assignment(Box::new(l), Box::new(r)),
                Rule::add_assign => AddAssign(Box::new(l), Box::new(r)),
                Rule::sub_assign => SubAssign(Box::new(l), Box::new(r)),
                Rule::mul_assign => MulAssign(Box::new(l), Box::new(r)),
                Rule::and_assign => AndAssign(Box::new(l), Box::new(r)),
                Rule::or_assign => OrAssign(Box::new(l), Box::new(r)),
                Rule::xor_assign => XorAssign(Box::new(l), Box::new(r)),
                Rule::shl_assign => ShLAssign(Box::new(l), Box::new(r)),
                Rule::sar_assign => SaRAssign(Box::new(l), Box::new(r)),
                _ => unreachable!(),
            })
            .map_prefix(|op, expr| match op.as_rule() {
                Rule::logic_not => LogicNot(Box::new(expr)),
                Rule::negative => Nega(Box::new(expr)),
                Rule::positive => expr,
                Rule::bit_not => Not(Box::new(expr)),
                Rule::pre_inc => PreInc(Box::new(expr)),
                Rule::pre_dec => PreDec(Box::new(expr)),
                _ => unreachable!(),
            })
            .map_postfix(|expr, op| match op.as_rule() {
                Rule::post_inc => PostInc(Box::new(expr)),
                Rule::post_dec => PostDec(Box::new(expr)),
                _ => unreachable!(),
            })
            .parse(expr.into_inner())
    }

    // len 是指针长度
    fn check_pointer<'a>(&self, exprs: &[Expr], len: &'a [usize]) -> Result<(RefType<'a>, ExprCategory, ExprConst), String> {
        for expr in exprs {
            if !matches!(self.expr_type(expr)?, RefType::Int) {
                return Err(format!("{expr:?} 不是整型表达式"));
            }
        }
        match (exprs.len() - 1).cmp(&len.len()) {
            std::cmp::Ordering::Less => Ok((RefType::IntPointer(&len[exprs.len()..]), RValue, NonConst)),
            std::cmp::Ordering::Equal => Ok((RefType::Int, LValue, NonConst)),
            std::cmp::Ordering::Greater => Err(format!("下标运算符不能应用于整型对象")),
        }
    }

    // 返回值：表达式的值类型、是否为可修改左值、是否为整型常量表达式
    pub fn expr_check(&self, expr: &Expr) -> Result<(RefType, ExprCategory, ExprConst), String> {
        match expr {
            Mul(l, r)
            | Div(l, r)
            | Mod(l, r)
            | Add(l, r)
            | Sub(l, r)
            | ShL(l, r)
            | ShR(l, r)
            | Xor(l, r)
            | And(l, r)
            | Or(l, r)
            | Eq(l, r)
            | Neq(l, r)
            | Grt(l, r)
            | Geq(l, r)
            | Les(l, r)
            | Leq(l, r)
            | LogicAnd(l, r)
            | LogicOr(l, r) => match (self.expr_check(l)?, self.expr_check(r)?) {
                ((RefType::Int, _, is_l_const), (RefType::Int, _, is_r_const)) => {
                    Ok((RefType::Int, RValue, is_l_const & is_r_const))
                }
                _ => Err(format!("{l:?} 或 {r:?} 不是整型表达式")),
            },

            LogicNot(e) | Nega(e) | Not(e) => match self.expr_check(e)? {
                (RefType::Int, _, is_const) => Ok((RefType::Int, RValue, is_const)),
                _ => Err(format!("{e:?} 不是整型表达式")),
            },

            PostInc(e) | PostDec(e) => match self.expr_check(e)? {
                (RefType::Int, LValue, _) => Ok((RefType::Int, RValue, NonConst)),
                _ => Err(format!("{e:?} 不是左值表达式")),
            },

            PreInc(e) | PreDec(e) => match self.expr_check(e)? {
                (RefType::Int, LValue, _) => Ok((RefType::Int, LValue, NonConst)),
                _ => Err(format!("{e:?} 不是左值表达式")),
            },

            Assignment(l, r)
            | AddAssign(l, r)
            | SubAssign(l, r)
            | MulAssign(l, r)
            | AndAssign(l, r)
            | OrAssign(l, r)
            | XorAssign(l, r)
            | ShLAssign(l, r)
            | SaRAssign(l, r) => match (self.expr_check(l)?, self.expr_check(r)?) {
                ((RefType::Int, LValue, _), (RefType::Int, _, _)) => Ok((RefType::Int, LValue, NonConst)),
                _ => Err(format!("{l:?} 或 {r:?} 不是整型表达式, 或 {l:?} 不是左值表达式")),
            },

            Num(_) => Ok((RefType::Int, RValue, ConstEval)),
            Var(id) => match self.symbol_table.search(id) {
                Some(Symbol(Type::Int, _, Some(ConstInit::Num(_)))) => Ok((RefType::Int, RValue, ConstEval)), // const 变量
                Some(Symbol(Type::Int, _, None)) => Ok((RefType::Int, LValue, NonConst)),                     // 普通变量
                Some(Symbol(Type::IntArray(_), _, Some(_))) => Err(format!("孤立的 const 数组似乎干不了什么事...")), // const 数组
                Some(Symbol(Type::IntArray(len), _, None)) => Ok((RefType::IntPointer(&len[1..]), RValue, NonConst)), // 普通数组
                Some(Symbol(Type::IntPointer(len), _, None)) => Ok((RefType::IntPointer(len), RValue, NonConst)), // 普通指针
                _ => Err(format!("标识符 {id} 在当前作用域不存在")),
            },
            Func(id, exprs) => match self.symbol_table.search(id) {
                Some(Symbol(Type::Function(ret_type, paras_type), _, _)) => {
                    if exprs.len() != paras_type.len() {
                        return Err(format!("实参列表与函数 {id} 的签名不匹配"));
                    }
                    for (expect_type, expr) in paras_type.iter().zip(exprs.iter()) {
                        let valid = match (expect_type, self.expr_type(expr)?) {
                            (Type::Int, RefType::Int) => true,
                            (Type::IntArray(l), RefType::IntArray(r)) => l == r,
                            (Type::IntArray(l), RefType::IntPointer(r)) => &l[1..] == r,
                            _ => false,
                        };
                        if !valid {
                            return Err(format!("{expr:?} 与类型 {expect_type:?} 不兼容"));
                        }
                    }
                    Ok((ret_type.to_ref_type(), RValue, NonConst))
                }
                Some(_) => Err(format!("标识符 {id} 不是函数")),
                None => Err(format!("标识符 {id} 在当前作用域中不存在")),
            },
            Array(id, exprs) => match self.symbol_table.search(id) {
                // const 数组
                Some(Symbol(Type::IntArray(len), _, Some(ConstInit::List(_)))) => match exprs.len().cmp(&len.len()) {
                    std::cmp::Ordering::Less => Err(format!("常量数组 {id} 不能转为指针")),
                    std::cmp::Ordering::Equal => {
                        let mut const_eval = true;
                        for expr in exprs {
                            let (ty, _, is_const) = self.expr_check(expr)?;
                            if !matches!(ty, RefType::Int) {
                                return Err(format!("{expr:?} 不是整型表达式"));
                            }
                            const_eval &= matches!(is_const, ConstEval);
                        }
                        if const_eval {
                            Ok((RefType::Int, RValue, ConstEval))
                        } else {
                            Ok((RefType::Int, RValue, NonConst))
                        }
                    }
                    std::cmp::Ordering::Greater => Err(format!("下标运算符不能应用于整型对象")),
                },
                // 普通数组
                Some(Symbol(Type::IntArray(len), _, None)) => self.check_pointer(exprs, &len[1..]),
                // 普通指针
                Some(Symbol(Type::IntPointer(len), _, _)) => self.check_pointer(exprs, len),
                Some(_) => Err(format!("标识符 {id} 不是数组或指针")),
                None => Err(format!("标识符 {id} 在当前作用域中不存在")),
            },
        }
    }

    pub fn expr_type(&self, expr: &Expr) -> Result<RefType, String> {
        Ok(self.expr_check(expr)?.0)
    }

    fn simplify(&self, mut expr: Expr) -> (Expr, bool, bool) {
        let mut simplified;
        let mut side_effect;
        loop {
            (expr, simplified, side_effect) = self.simplify_impl(expr);
            if !simplified {
                return (expr, simplified, side_effect);
            }
        }
    }
    // 返回值类型：表达式、此轮是否化简、表达式有无副作用
    fn simplify_impl(&self, expr: Expr) -> (Expr, bool, bool) {
        match expr {
            Num(i) => (Num(i), false, false),
            Var(x) => match self.symbol_table.search(&x).unwrap() {
                Symbol(_, _, Some(ConstInit::Num(i))) => (Num(*i), true, false),
                Symbol(_, mangled_name, _) => (Var(mangled_name.clone()), false, false),
            },
            Add(l, r) => match (self.simplify(*l), self.simplify(*r)) {
                ((Num(a), _, _), (Num(b), _, _)) => (Num(a + b), true, false),
                ((Num(0), _, _), (e, _, e_se)) => (e, true, e_se),
                ((e, _, e_se), (Num(0), _, _)) => (e, true, e_se),
                ((l, _, l_se), (Nega(r), _, r_se)) => (Sub(Box::new(l), r), true, l_se || r_se),
                ((l, l_s, l_se), (r, r_s, r_se)) => (Add(Box::new(l), Box::new(r)), l_s || r_s, l_se || r_se),
            },
            Sub(l, r) => match (self.simplify(*l), self.simplify(*r)) {
                ((Num(a), _, _), (Num(b), _, _)) => (Num(a - b), true, false),
                ((l, _, _), (r, _, _)) if l == r => (Num(0), true, false),
                ((l, l_s, l_se), (r, r_s, r_se)) => (Sub(Box::new(l), Box::new(r)), l_s || r_s, l_se || r_se),
            },
            Mul(l, r) => match (self.simplify(*l), self.simplify(*r)) {
                ((Num(a), _, _), (Num(b), _, _)) => (Num(a * b), true, false),
                ((Num(0), _, _), (_, _, false)) | ((_, _, false), (Num(0), _, _)) => (Num(0), true, false),
                ((Num(1), _, _), (r, _, r_se)) | ((r, _, r_se), (Num(1), _, _)) => (r, true, r_se),
                ((Num(-1), _, _), (r, _, r_se)) | ((r, _, r_se), (Num(-1), _, _)) => (Nega(Box::new(r)), true, r_se),
                ((l, l_s, l_se), (r, r_s, r_se)) => (Mul(Box::new(l), Box::new(r)), l_s || r_s, l_se || r_se),
            },
            Div(l, r) => match (self.simplify(*l), self.simplify(*r)) {
                (_, (Num(0), _, _)) => {
                    println!("表达式中出现除零");
                    (Num(2147483647), true, false)
                }
                ((Num(a), _, _), (Num(b), _, _)) => (Num(a / b), true, false),
                ((Num(0), _, _), (_, _, false)) => (Num(0), true, false),
                ((r, _, r_se), (Num(1), _, _)) => (r, true, r_se),
                ((r, _, r_se), (Num(-1), _, _)) => (Nega(Box::new(r)), true, r_se),
                ((l, l_s, l_se), (r, r_s, r_se)) => (Div(Box::new(l), Box::new(r)), l_s || r_s, l_se || r_se),
            },
            Mod(l, r) => match (self.simplify(*l), self.simplify(*r)) {
                (_, (Num(0), _, _)) => {
                    println!("表达式中出现除零");
                    (Num(2147483647), true, false)
                }
                ((Num(a), _, _), (Num(b), _, _)) => (Num(a % b), true, false),
                ((l, l_s, l_se), (r, r_s, r_se)) => (Mod(Box::new(l), Box::new(r)), l_s || r_s, l_se || r_se),
            },
            ShL(l, r) => match (self.simplify(*l), self.simplify(*r)) {
                ((Num(a), _, _), (Num(b), _, _)) => (Num(a << b), true, false),
                ((l, l_s, l_se), (r, r_s, r_se)) => (ShL(Box::new(l), Box::new(r)), l_s || r_s, l_se || r_se),
            },
            ShR(l, r) => match (self.simplify(*l), self.simplify(*r)) {
                ((Num(a), _, _), (Num(b), _, _)) => (Num(a >> b), true, false),
                ((l, l_s, l_se), (r, r_s, r_se)) => (ShR(Box::new(l), Box::new(r)), l_s || r_s, l_se || r_se),
            },
            Xor(l, r) => match (self.simplify(*l), self.simplify(*r)) {
                ((Num(a), _, _), (Num(b), _, _)) => (Num(a ^ b), true, false),
                ((Num(0), _, _), (r, _, r_se)) | ((r, _, r_se), (Num(0), _, _)) => (r, true, r_se),
                ((l, _, _), (r, _, _)) if l == r => (Num(0), true, false),
                ((l, l_s, l_se), (r, r_s, r_se)) => (Xor(Box::new(l), Box::new(r)), l_s || r_s, l_se || r_se),
            },
            And(l, r) => match (self.simplify(*l), self.simplify(*r)) {
                ((Num(a), _, _), (Num(b), _, _)) => (Num(a & b), true, false),
                ((Num(0), _, _), (_, _, false)) | ((_, _, false), (Num(0), _, _)) => (Num(0), true, false),
                ((Num(1), _, _), (r, _, r_se)) | ((r, _, r_se), (Num(1), _, _)) => (r, true, r_se),
                ((l, _, _), (r, _, _)) if l == r => (l, true, false),
                ((l, l_s, l_se), (r, r_s, r_se)) => (And(Box::new(l), Box::new(r)), l_s || r_s, l_se || r_se),
            },
            Or(l, r) => match (self.simplify(*l), self.simplify(*r)) {
                ((Num(a), _, _), (Num(b), _, _)) => (Num(a | b), true, false),
                ((Num(0), _, _), (r, _, r_se)) | ((r, _, r_se), (Num(0), _, _)) => (r, true, r_se),
                ((Num(1), _, _), (_, _, false)) | ((_, _, false), (Num(1), _, _)) => (Num(1), true, false),
                ((l, _, _), (r, _, _)) if l == r => (l, true, false),
                ((l, l_s, l_se), (r, r_s, r_se)) => (Or(Box::new(l), Box::new(r)), l_s || r_s, l_se || r_se),
            },
            Eq(l, r) => match (self.simplify(*l), self.simplify(*r)) {
                ((Num(a), _, _), (Num(b), _, _)) => (Num((a == b) as i32), true, false),
                ((l, _, _), (r, _, _)) if l == r => (Num(1), true, false),
                ((l, l_s, l_se), (r, r_s, r_se)) => (Eq(Box::new(l), Box::new(r)), l_s || r_s, l_se || r_se),
            },
            Neq(l, r) => match (self.simplify(*l), self.simplify(*r)) {
                ((Num(a), _, _), (Num(b), _, _)) => (Num((a != b) as i32), true, false),
                ((l, _, _), (r, _, _)) if l == r => (Num(0), true, false),
                ((l, l_s, l_se), (r, r_s, r_se)) => (Neq(Box::new(l), Box::new(r)), l_s || r_s, l_se || r_se),
            },
            Grt(l, r) => match (self.simplify(*l), self.simplify(*r)) {
                ((Num(a), _, _), (Num(b), _, _)) => (Num((a > b) as i32), true, false),
                ((l, l_s, l_se), (r, r_s, r_se)) => (Grt(Box::new(l), Box::new(r)), l_s || r_s, l_se || r_se),
            },
            Geq(l, r) => match (self.simplify(*l), self.simplify(*r)) {
                ((Num(a), _, _), (Num(b), _, _)) => (Num((a >= b) as i32), true, false),
                ((l, l_s, l_se), (r, r_s, r_se)) => (Geq(Box::new(l), Box::new(r)), l_s || r_s, l_se || r_se),
            },
            Les(l, r) => match (self.simplify(*l), self.simplify(*r)) {
                ((Num(a), _, _), (Num(b), _, _)) => (Num((a < b) as i32), true, false),
                ((l, l_s, l_se), (r, r_s, r_se)) => (Les(Box::new(l), Box::new(r)), l_s || r_s, l_se || r_se),
            },
            Leq(l, r) => match (self.simplify(*l), self.simplify(*r)) {
                ((Num(a), _, _), (Num(b), _, _)) => (Num((a <= b) as i32), true, false),
                ((l, l_s, l_se), (r, r_s, r_se)) => (Leq(Box::new(l), Box::new(r)), l_s || r_s, l_se || r_se),
            },
            LogicAnd(l, r) => match (self.simplify(*l), self.simplify(*r)) {
                ((Num(a), _, _), (Num(b), _, _)) => (Num((a != 0 && b != 0) as i32), true, false),
                ((Num(0), _, _), (_, _, false)) => (Num(0), true, false),
                ((Num(a), _, _), (r, _, r_se)) | ((r, _, r_se), (Num(a), _, _)) if a != 0 => (r, true, r_se),
                ((l, l_s, l_se), (r, r_s, r_se)) => (LogicAnd(Box::new(l), Box::new(r)), l_s || r_s, l_se || r_se),
            },
            LogicOr(l, r) => match (self.simplify(*l), self.simplify(*r)) {
                ((Num(a), _, _), (Num(b), _, _)) => (Num((a != 0 || b != 0) as i32), true, false),
                ((Num(0), _, _), (r, _, r_se)) | ((r, _, r_se), (Num(0), _, _)) => (r, true, r_se),
                ((Num(a), _, _), (_, _, false)) if a != 0 => (Num(1), true, false),
                ((l, l_s, l_se), (r, r_s, r_se)) => (LogicOr(Box::new(l), Box::new(r)), l_s || r_s, l_se || r_se),
            },
            LogicNot(expr) => match self.simplify(*expr) {
                (Num(a), _, _) => (Num((a == 0) as i32), true, false),
                (e, e_s, e_se) => (LogicNot(Box::new(e)), e_s, e_se),
            },
            Nega(expr) => match self.simplify(*expr) {
                (Num(a), _, _) => (Num(-a), true, false),
                (Nega(e), _, e_se) => (*e, true, e_se),
                (Sub(l, r), _, e_se) => (Sub(r, l), true, e_se),
                (e, e_s, e_se) => (Nega(Box::new(e)), e_s, e_se),
            },
            Not(expr) => match self.simplify(*expr) {
                (Num(a), _, _) => (Num(!a), true, false),
                (e, e_s, e_se) => (Not(Box::new(e)), e_s, e_se),
            },
            PostInc(expr) => match self.simplify(*expr) {
                (e, e_s, _) => (PostInc(Box::new(e)), e_s, true),
            },
            PostDec(expr) => match self.simplify(*expr) {
                (e, e_s, _) => (PostDec(Box::new(e)), e_s, true),
            },
            PreInc(expr) => match self.simplify(*expr) {
                (e, e_s, _) => (PreInc(Box::new(e)), e_s, true),
            },
            PreDec(expr) => match self.simplify(*expr) {
                (e, e_s, _) => (PreDec(Box::new(e)), e_s, true),
            },
            Assignment(l, r) => match (self.simplify(*l), self.simplify(*r)) {
                ((l, l_s, _), (r, r_s, _)) => (Assignment(Box::new(l), Box::new(r)), l_s || r_s, true),
            },
            AddAssign(l, r) => match (self.simplify(*l), self.simplify(*r)) {
                ((l, l_s, _), (r, r_s, _)) => (AddAssign(Box::new(l), Box::new(r)), l_s || r_s, true),
            },
            SubAssign(l, r) => match (self.simplify(*l), self.simplify(*r)) {
                ((l, l_s, _), (r, r_s, _)) => (SubAssign(Box::new(l), Box::new(r)), l_s || r_s, true),
            },
            MulAssign(l, r) => match (self.simplify(*l), self.simplify(*r)) {
                ((l, l_s, _), (r, r_s, _)) => (MulAssign(Box::new(l), Box::new(r)), l_s || r_s, true),
            },
            AndAssign(l, r) => match (self.simplify(*l), self.simplify(*r)) {
                ((l, l_s, _), (r, r_s, _)) => (AndAssign(Box::new(l), Box::new(r)), l_s || r_s, true),
            },
            OrAssign(l, r) => match (self.simplify(*l), self.simplify(*r)) {
                ((l, l_s, _), (r, r_s, _)) => (OrAssign(Box::new(l), Box::new(r)), l_s || r_s, true),
            },
            XorAssign(l, r) => match (self.simplify(*l), self.simplify(*r)) {
                ((l, l_s, _), (r, r_s, _)) => (XorAssign(Box::new(l), Box::new(r)), l_s || r_s, true),
            },
            ShLAssign(l, r) => match (self.simplify(*l), self.simplify(*r)) {
                ((l, l_s, _), (r, r_s, _)) => (ShLAssign(Box::new(l), Box::new(r)), l_s || r_s, true),
            },
            SaRAssign(l, r) => match (self.simplify(*l), self.simplify(*r)) {
                ((l, l_s, _), (r, r_s, _)) => (SaRAssign(Box::new(l), Box::new(r)), l_s || r_s, true),
            },
            Func(id, args) => {
                let (args_simplified, s) = args.into_iter().fold((Vec::new(), false), |(mut v, mut s), expr| {
                    let (e, _s, _) = self.simplify(expr);
                    v.push(e);
                    s = s || _s;
                    (v, s)
                });
                (Func(id, args_simplified), s, true)
            }
            Array(id, subscripts) => {
                let Symbol(_, mangled_name, init) = self.symbol_table.search(&id).unwrap();
                let (subscripts_simplified, s, se) =
                    subscripts.into_iter().fold((Vec::new(), false, false), |(mut v, mut s, mut se), expr| {
                        let (e, _s, _se) = self.simplify(expr);
                        v.push(e);
                        s = s || _s;
                        se = se || _se;
                        (v, s, se)
                    });
                match (subscripts_simplified.iter().all(|expr| matches!(expr, Num(_))), init) {
                    (true, Some(ConstInit::List(l))) => {
                        let mut r_ref = l;
                        for expr in subscripts_simplified.iter().take(subscripts_simplified.len() - 1) {
                            let i = expr.get_num() as usize;
                            if i >= r_ref.len() {
                                return (Num(0), true, false);
                            }
                            r_ref = risk!(&r_ref[i], ConstInitListItem::ConstInitList(l) => l);
                        }
                        let i = subscripts_simplified.last().unwrap().get_num() as usize;
                        if i >= r_ref.len() {
                            (Num(0), true, false)
                        } else {
                            (Num(risk!(r_ref[i], ConstInitListItem::Num(i) => i)), true, false)
                        }
                    }
                    _ => (Array(mangled_name.clone(), subscripts_simplified), s, se),
                }
            }
        }
    }

    pub fn process_expr_impl(&self, expr: Pair<Rule>) -> Result<(Expr, RefType, ExprConst), String> {
        let expr = self.parse_expr(expr);
        let (ty, _, is_const) = self.expr_check(&expr)?;
        let (expr, _, _) = self.simplify(expr);
        Ok((expr, ty, is_const))
    }

    pub fn process_expr(&self, expr: Pair<Rule>) -> Result<Expr, String> {
        let (expr, _, _) = self.process_expr_impl(expr)?;
        Ok(expr)
    }
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Mul(l0, l1), Mul(r0, r1)) => (l0 == r0 && l1 == r1) || (l0 == r1 && l1 == r0),
            (Div(l0, l1), Div(r0, r1)) => l0 == r0 && l1 == r1,
            (Mod(l0, l1), Mod(r0, r1)) => l0 == r0 && l1 == r1,
            (Add(l0, l1), Add(r0, r1)) => (l0 == r0 && l1 == r1) || (l0 == r1 && l1 == r0),
            (Sub(l0, l1), Sub(r0, r1)) => l0 == r0 && l1 == r1,
            (ShL(l0, l1), ShL(r0, r1)) => l0 == r0 && l1 == r1,
            (ShR(l0, l1), ShR(r0, r1)) => l0 == r0 && l1 == r1,
            (Xor(l0, l1), Xor(r0, r1)) => (l0 == r0 && l1 == r1) || (l0 == r1 && l1 == r0),
            (And(l0, l1), And(r0, r1)) => (l0 == r0 && l1 == r1) || (l0 == r1 && l1 == r0),
            (Or(l0, l1), Or(r0, r1)) => (l0 == r0 && l1 == r1) || (l0 == r1 && l1 == r0),
            (Eq(l0, l1), Eq(r0, r1)) => (l0 == r0 && l1 == r1) || (l0 == r1 && l1 == r0),
            (Neq(l0, l1), Neq(r0, r1)) => (l0 == r0 && l1 == r1) || (l0 == r1 && l1 == r0),
            (Grt(l0, l1), Grt(r0, r1)) => l0 == r0 && l1 == r1,
            (Geq(l0, l1), Geq(r0, r1)) => l0 == r0 && l1 == r1,
            (Les(l0, l1), Les(r0, r1)) => l0 == r0 && l1 == r1,
            (Leq(l0, l1), Leq(r0, r1)) => l0 == r0 && l1 == r1,
            (LogicAnd(l0, l1), LogicAnd(r0, r1)) => l0 == r0 && l1 == r1,
            (LogicOr(l0, l1), LogicOr(r0, r1)) => l0 == r0 && l1 == r1,
            (LogicNot(l0), LogicNot(r0)) => l0 == r0,
            (Nega(l0), Nega(r0)) => l0 == r0,
            (Not(l0), Not(r0)) => l0 == r0,
            (Num(l0), Num(r0)) => l0 == r0,
            (Var(l0), Var(r0)) => l0 == r0,
            _ => false,
        }
    }
}
