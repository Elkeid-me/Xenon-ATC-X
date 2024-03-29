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
    pub fn expr_dvalue(&mut self, expr: Expr) -> String {
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
            | Leq(l, r) => format!("{}{}", self.expr_dvalue(*l), self.expr_dvalue(*r)),
            logic if matches!(logic, LogicAnd(_, _) | LogicOr(_, _)) => self.expr_rvalue(logic).0,
            LogicNot(expr) | Nega(expr) | Not(expr) => self.expr_dvalue(*expr),
            inc_dec if matches!(inc_dec, PostInc(_) | PostDec(_) | PreInc(_) | PreDec(_)) => self.expr_rvalue(inc_dec).0,
            Func(id, args) => {
                let (arg_str, arg_ids) = args
                    .into_iter()
                    .map(|expr| self.expr_rvalue(expr))
                    .reduce(|(l_str, l_id), (r_str, r_id)| (format!("{l_str}{r_str}"), format!("{l_id}, {r_id}")))
                    .unwrap_or_default();
                format!("{arg_str}    call @{id}({arg_ids})\n")
            }
            Array(_, subscripts, _) => subscripts.into_iter().map(|expr| self.expr_dvalue(expr)).collect(),
            Var(_) | Num(_) => String::new(),
            assign => self.expr_rvalue(assign).0,
        }
    }
}
