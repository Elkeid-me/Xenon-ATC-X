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

use super::Generator;
use crate::frontend::ast::Expr::{self, *};

impl Generator {
    pub fn arith_expr_helper(&mut self, l: Expr, r: Expr, op: &str) -> (String, String) {
        let id = self.counter.get();
        let (l_eval, l_id) = self.expr_rvalue(l);
        let (r_eval, r_id) = self.expr_rvalue(r);
        (format!("{l_eval}{r_eval}    {id} = {op} {l_id}, {r_id}\n"), id)
    }

    pub fn arith_unary_helper(&mut self, expr: Expr, operate: &str) -> (String, String) {
        let id = self.counter.get();
        let (expr_eval, expr_id) = self.expr_rvalue(expr);
        (format!("{expr_eval}    {id} = {operate}, {expr_id}\n"), id)
    }

    pub fn array_elem_rvalue(&mut self, id: String, subscripts: Vec<Expr>, rvalue_int: bool) -> (String, String) {
        let (expr_eval, expr_id) = self.array_elem_lvalue(id, subscripts);
        if rvalue_int {
            let tmp_id = self.counter.get();
            (format!("{expr_eval}    {tmp_id} = load {expr_id}\n"), tmp_id)
        } else {
            let tmp_id = self.counter.get();
            (format!("{expr_eval}    {tmp_id} = getelemptr {expr_id}, 0\n"), tmp_id)
        }
    }

    pub fn expr_rvalue(&mut self, expr: Expr) -> (String, String) {
        match expr {
            Mul(l, r) => self.arith_expr_helper(*l, *r, "mul"),
            Div(l, r) => self.arith_expr_helper(*l, *r, "div"),
            Mod(l, r) => self.arith_expr_helper(*l, *r, "mod"),
            Add(l, r) => self.arith_expr_helper(*l, *r, "add"),
            Sub(l, r) => self.arith_expr_helper(*l, *r, "sub"),
            ShL(l, r) => self.arith_expr_helper(*l, *r, "shl"),
            ShR(l, r) => self.arith_expr_helper(*l, *r, "shr"),
            Xor(l, r) => self.arith_expr_helper(*l, *r, "xor"),
            And(l, r) => self.arith_expr_helper(*l, *r, "and"),
            Or(l, r) => self.arith_expr_helper(*l, *r, "or"),
            Eq(l, r) => self.arith_expr_helper(*l, *r, "eq"),
            Neq(l, r) => self.arith_expr_helper(*l, *r, "ne"),
            Grt(l, r) => self.arith_expr_helper(*l, *r, "gt"),
            Geq(l, r) => self.arith_expr_helper(*l, *r, "ge"),
            Les(l, r) => self.arith_expr_helper(*l, *r, "lt"),
            Leq(l, r) => self.arith_expr_helper(*l, *r, "le"),
            LogicAnd(l, r) => {
                let id = self.counter.get();
                let (l_eval, l_id) = self.expr_rvalue(*l);
                let (r_eval, r_id) = self.expr_rvalue(*r);
                let l_ne_0_id = self.counter.get();
                let r_ne_0_id = self.counter.get();
                let eval_r_id = self.counter.get();
                let expr_eq_0_id = self.counter.get();
                let tmp_id = self.counter.get();
                let expr_store_id = self.counter.get();
                let next_id = self.counter.get();
                (
                    format!(
                        r"{l_eval}    {expr_store_id} = alloc i32
    {l_ne_0_id} = ne {l_id}, 0
    br {l_ne_0_id}, {eval_r_id}, {expr_eq_0_id}
{expr_eq_0_id}:
    store 0, {expr_store_id}
    jump {next_id}
{eval_r_id}:
{r_eval}    {r_ne_0_id} = ne {r_id}, 0
    {tmp_id} = and {l_ne_0_id}, {r_ne_0_id}
    store {tmp_id}, {expr_store_id}
    jump {next_id}
{next_id}:
    {id} = load {expr_store_id}
"
                    ),
                    id,
                )
            }
            LogicOr(l, r) => {
                let id = self.counter.get();
                let (l_eval, l_id) = self.expr_rvalue(*l);
                let (r_eval, r_id) = self.expr_rvalue(*r);
                let l_ne_0_id = self.counter.get();
                let r_ne_0_id = self.counter.get();
                let eval_r_id = self.counter.get();
                let expr_eq_1_id = self.counter.get();
                let tmp_id = self.counter.get();
                let expr_store_id = self.counter.get();
                let next_id = self.counter.get();
                (
                    format!(
                        r"{l_eval}    {expr_store_id} = alloc i32
    {l_ne_0_id} = ne {l_id}, 0
    br {l_ne_0_id}, {expr_eq_1_id}, {eval_r_id}
{expr_eq_1_id}:
    store 1, {expr_store_id}
    jump {next_id}
{eval_r_id}:
{r_eval}    {r_ne_0_id} = ne {r_id}, 0
    {tmp_id} = or {l_ne_0_id}, {r_ne_0_id}
    store {tmp_id}, {expr_store_id}
    jump {next_id}
{next_id}:
    {id} = load {expr_store_id}
"
                    ),
                    id,
                )
            }
            LogicNot(expr) => self.arith_unary_helper(*expr, "eq 0"),
            Nega(expr) => self.arith_unary_helper(*expr, "sub 0"),
            Not(expr) => self.arith_unary_helper(*expr, "xor 1"),
            PostInc(expr) => self.inc_dec_helper(*expr, "add", false, true),
            PostDec(expr) => self.inc_dec_helper(*expr, "sub", false, true),
            PreInc(expr) => self.inc_dec_helper(*expr, "add", true, true),
            PreDec(expr) => self.inc_dec_helper(*expr, "sub", true, true),
            Assignment(l, r) => {
                let (r_eval, r_id) = self.expr_rvalue(*r);
                let (l_eval, l_id) = self.expr_lvalue(*l);
                (format!("{r_eval}{l_eval}    store {r_id}, {l_id}\n"), r_id)
            }
            AddAssign(l, r) => self.assign_expr_helper(*l, *r, "add", true),
            SubAssign(l, r) => self.assign_expr_helper(*l, *r, "sub", true),
            MulAssign(l, r) => self.assign_expr_helper(*l, *r, "mul", true),
            DivAssign(l, r) => self.assign_expr_helper(*l, *r, "div", true),
            ModAssign(l, r) => self.assign_expr_helper(*l, *r, "mod", true),
            AndAssign(l, r) => self.assign_expr_helper(*l, *r, "and", true),
            OrAssign(l, r) => self.assign_expr_helper(*l, *r, "or", true),
            XorAssign(l, r) => self.assign_expr_helper(*l, *r, "xor", true),
            ShLAssign(l, r) => self.assign_expr_helper(*l, *r, "shl", true),
            SaRAssign(l, r) => self.assign_expr_helper(*l, *r, "sar", true),
            Num(i) => (String::new(), i.to_string()),
            Var(x) => {
                let id = self.counter.get();
                match &x[0..2] {
                    "_I" => (format!("    {id} = load %{x}\n"), id),
                    "_P" => (format!("    {id} = load %{x}\n"), id),
                    "_A" => (format!("    {id} = getelemptr %{x}, 0\n"), id),
                    _ => unreachable!(),
                }
            }
            Func(fun_id, args) => {
                let (arg_str, arg_ids) = args
                    .into_iter()
                    .map(|expr| self.expr_rvalue(expr))
                    .reduce(|(l_str, l_id), (r_str, r_id)| (format!("{l_str}{r_str}"), format!("{l_id}, {r_id}")))
                    .unwrap_or_default();
                let tmp_id = self.counter.get();
                (format!("{arg_str}    {tmp_id} = call @{fun_id}({arg_ids})\n"), tmp_id)
            }
            Array(id, subscripts, rvalue_int) => self.array_elem_rvalue(id, subscripts, rvalue_int),
        }
    }
}
