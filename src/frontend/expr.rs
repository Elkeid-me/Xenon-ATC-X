use super::ast::{Expr::*, ExprCategory::*, ExprConst::*, *};
use super::parser::{ASTBuilder, ConstInit, Rule, Scope, Symbol};
use super::ty::*;
use pest::iterators::Pair;

impl ASTBuilder {
    pub fn parse_expr(&self, expr: Pair<Rule>) -> Expr {
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
    pub fn check_pointer<'a>(&self, exprs: &[Expr], len: &'a [usize]) -> Result<(RefType<'a>, ExprCategory, ExprConst), String> {
        for expr in exprs {
            if !matches!(self.expr_type(expr)?, RefType::Int) {
                return Err(format!("表达式 {expr:?} 不是整型表达式"));
            }
        }
        match exprs.len().cmp(&(len.len() + 1)) {
            std::cmp::Ordering::Less => Ok((RefType::IntPointer(&len[exprs.len() - 1..]), RValue, NonConst)),
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
                _ => Err(format!("{e:?} 不是整型表达式或左值表达式")),
            },

            PreInc(e) | PreDec(e) => match self.expr_check(e)? {
                (RefType::Int, LValue, _) => Ok((RefType::Int, LValue, NonConst)),
                _ => Err(format!("{e:?} 不是整型表达式或左值表达式")),
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
                            return Err(format!("表达式 {expr:?} 与类型 {expect_type:?} 不兼容"));
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
                                return Err(format!("表达式 {expr:?} 不是整型表达式"));
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

    pub fn simplify(&self, expr: Expr) -> (Expr, bool) {
        match expr {
            Num(i) => (Num(i), false),
            Var(x) => (Var(x), false),
            e => (e, false),
        }
    }

    pub fn process_expr_impl(&self, expr: Pair<Rule>) -> Result<(Expr, RefType, ExprConst), String> {
        let expr = self.parse_expr(expr);
        let (ty, _, is_const) = self.expr_check(&expr)?;
        let (expr, _) = self.simplify(expr);
        Ok((expr, ty, is_const))
    }

    pub fn process_expr(&self, expr: Pair<Rule>) -> Result<Expr, String> {
        let (expr, _, _) = self.process_expr_impl(expr)?;
        Ok(expr)
    }
}
