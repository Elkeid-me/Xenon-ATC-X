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
    pub fn array_elem_lvalue(&mut self, id: String, subscripts: Vec<Expr>) -> (String, String) {
        match &id[0..2] {
            "_P" => {
                let tmp_id_0 = self.counter.get();
                let mut id = format!("%{id}");
                let str_0 = format!("    {tmp_id_0} = load {id}\n");
                let tmp_id_1 = self.counter.get();
                let mut subscripts = subscripts.into_iter();
                let (expr_eval, expr_id) = self.expr_rvalue(subscripts.next().unwrap());
                let str_1 = format!("{expr_eval}\n    {tmp_id_1} = getptr {tmp_id_0}, {expr_id}\n");
                id = tmp_id_1;
                let str: String = subscripts
                    .map(|expr| {
                        let (expr_eval, expr_id) = self.expr_rvalue(expr);
                        let new_id = self.counter.get();
                        let str = format!("{expr_eval}\n    {new_id} = getelemptr {id}, {expr_id}\n");
                        id = new_id;
                        str
                    })
                    .collect();
                (format!("{str_0}{str_1}{str}"), id)
            }
            "_A" => {
                let mut id = format!("%{id}");
                let str = subscripts
                    .into_iter()
                    .map(|expr| {
                        let (expr_eval, expr_id) = self.expr_rvalue(expr);
                        let new_id = self.counter.get();
                        let str = format!("{expr_eval}\n    {new_id} = getelemptr {id}, {expr_id}\n");
                        id = new_id;
                        str
                    })
                    .collect();
                (str, id)
            }
            _ => unreachable!(),
        }
    }

    pub fn expr_lvalue(&mut self, expr: Expr) -> (String, String) {
        match expr {
            PreInc(expr) => self.inc_dec_helper(*expr, "add", true, false),
            PreDec(expr) => self.inc_dec_helper(*expr, "sub", true, false),
            Assignment(l, r) => {
                let r_eval = self.expr_dvalue(*r);
                let (l_eval, l_id) = self.expr_lvalue(*l);
                (format!("{r_eval}{l_eval}\n"), l_id)
            }
            AddAssign(l, r) => self.assign_expr_helper(*l, *r, "add", false),
            SubAssign(l, r) => self.assign_expr_helper(*l, *r, "sub", false),
            MulAssign(l, r) => self.assign_expr_helper(*l, *r, "mul", false),
            AndAssign(l, r) => self.assign_expr_helper(*l, *r, "and", false),
            OrAssign(l, r) => self.assign_expr_helper(*l, *r, "or", false),
            XorAssign(l, r) => self.assign_expr_helper(*l, *r, "xor", false),
            ShLAssign(l, r) => self.assign_expr_helper(*l, *r, "shl", false),
            SaRAssign(l, r) => self.assign_expr_helper(*l, *r, "sar", false),
            Var(x) => (String::new(), format!("%{x}")),
            Array(id, subscripts, _) => self.array_elem_lvalue(id, subscripts),
            _ => unreachable!(),
        }
    }
}
