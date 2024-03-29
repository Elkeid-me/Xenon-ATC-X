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
use crate::frontend::ast::{Expr::*, *};

impl Generator {
    fn cond_expr(&mut self, cond: Expr, then_label: &str, else_label: &str) -> String {
        match cond {
            Num(0) => format!("    jump {else_label}\n"),
            Num(_) => format!("    jump {then_label}\n"),
            LogicAnd(l, r) => {
                let label = self.counter.get();
                format!("{}{label}:\n{}", self.cond_expr(*l, &label, else_label), self.cond_expr(*r, then_label, else_label))
            }
            LogicOr(l, r) => {
                let label = self.counter.get();
                format!("{}{label}:\n{}", self.cond_expr(*l, then_label, &label), self.cond_expr(*r, then_label, else_label))
            }

            Eq(l, r) => {
                let (l_eval, l_id) = self.expr_rvalue(*l);
                let (r_eval, r_id) = self.expr_rvalue(*r);
                let cond_id = format!("{}_br", self.counter.get());
                format!("{l_eval}{r_eval}    {cond_id} = eq {l_id}, {r_id}\n    br {cond_id}, {then_label}, {else_label}\n")
            }
            Neq(l, r) => {
                let (l_eval, l_id) = self.expr_rvalue(*l);
                let (r_eval, r_id) = self.expr_rvalue(*r);
                let cond_id = format!("{}_br", self.counter.get());
                format!("{l_eval}{r_eval}    {cond_id} = ne {l_id}, {r_id}\n    br {cond_id}, {then_label}, {else_label}\n")
            }
            Grt(l, r) => {
                let (l_eval, l_id) = self.expr_rvalue(*l);
                let (r_eval, r_id) = self.expr_rvalue(*r);
                let cond_id = format!("{}_br", self.counter.get());
                format!("{l_eval}{r_eval}    {cond_id} = gt {l_id}, {r_id}\n    br {cond_id}, {then_label}, {else_label}\n")
            }
            Les(l, r) => {
                let (l_eval, l_id) = self.expr_rvalue(*l);
                let (r_eval, r_id) = self.expr_rvalue(*r);
                let cond_id = format!("{}_br", self.counter.get());
                format!("{l_eval}{r_eval}    {cond_id} = lt {l_id}, {r_id}\n    br {cond_id}, {then_label}, {else_label}\n")
            }
            Geq(l, r) => {
                let (l_eval, l_id) = self.expr_rvalue(*l);
                let (r_eval, r_id) = self.expr_rvalue(*r);
                let cond_id = format!("{}_br", self.counter.get());
                format!("{l_eval}{r_eval}    {cond_id} = ge {l_id}, {r_id}\n    br {cond_id}, {then_label}, {else_label}\n")
            }
            Leq(l, r) => {
                let (l_eval, l_id) = self.expr_rvalue(*l);
                let (r_eval, r_id) = self.expr_rvalue(*r);
                let cond_id = format!("{}_br", self.counter.get());
                format!("{l_eval}{r_eval}    {cond_id} = le {l_id}, {r_id}\n    br {cond_id}, {then_label}, {else_label}\n")
            }

            _ => {
                let (cond_eval, cond_id) = self.expr_rvalue(cond);
                format!("{cond_eval}    br {cond_id}, {then_label}, {else_label}\n")
            }
        }
    }
    fn while_statement(&mut self, cond: Expr, block: Block) -> String {
        match cond {
            Num(0) => String::new(),
            Num(_) => {
                let while_id = self.counter.get();
                let while_next_id = self.counter.get();
                let (block_str, _) = self.block(block, &while_id, &while_next_id);
                format!(
                    r"    jump {while_id}
{while_id}:
{block_str}    jump {while_id}
{while_next_id}:
"
                )
            }
            _ => {
                let while_id = self.counter.get();
                let while_next_id = self.counter.get();
                let (block_str, block_id) = self.block(block, &while_id, &while_next_id);
                let cond_str = self.cond_expr(cond, &block_id, &while_next_id);
                format!(
                    r"    jump {while_id}
{while_id}:
{cond_str}
{block_id}:
{block_str}    jump {while_id}
{while_next_id}:
"
                )
            }
        }
    }
    fn if_statement(&mut self, cond: Expr, then_block: Block, else_block: Block, while_id: &str, while_next_id: &str) -> String {
        match (then_block.is_empty(), else_block.is_empty()) {
            (true, true) => {
                let next_id = self.counter.get();
                let cond_str = self.cond_expr(cond, &next_id, &next_id);
                format!("{cond_str}{next_id}:\n")
            }
            (false, true) => {
                let next_id = self.counter.get();
                let (then_block_str, then_id) = self.block(then_block, &while_id, &while_next_id);
                let cond_str = self.cond_expr(cond, &then_id, &next_id);
                format!(
                    r"{cond_str}{then_id}:
{then_block_str}    jump {next_id}
{next_id}:
"
                )
            }
            (true, false) => {
                let next_id = self.counter.get();
                let (else_block_str, else_id) = self.block(else_block, &while_id, &while_next_id);
                let cond_str = self.cond_expr(cond, &next_id, &else_id);
                format!(
                    r"{cond_str}{else_id}:
{else_block_str}    jump {next_id}
{next_id}:
"
                )
            }
            (false, false) => {
                let next_id = self.counter.get();
                let (then_block_str, then_id) = self.block(then_block, &while_id, &while_next_id);
                let (else_block_str, else_id) = self.block(else_block, &while_id, &while_next_id);
                let cond_str = self.cond_expr(cond, &then_id, &else_id);
                format!(
                    r"{cond_str}{then_id}:
{then_block_str}    jump {next_id}
{else_id}:
{else_block_str}    jump {next_id}
{next_id}:
"
                )
            }
        }
    }
    pub fn statement(&mut self, statement: Statement, while_id: &str, while_next_id: &str) -> String {
        match statement {
            Statement::Expr(expr) => self.expr_dvalue(expr),
            Statement::If(condition, then_block, else_block) => {
                self.if_statement(condition, *then_block, *else_block, while_id, while_next_id)
            }
            Statement::While(condition, block) => self.while_statement(condition, *block),
            Statement::Return(expr) => match expr {
                Some(expr) => {
                    let (expr_str, expr_id) = self.expr_rvalue(expr);
                    format!("{expr_str}    ret {expr_id}\n")
                }
                None => "    ret\n".to_string(),
            },
            Statement::Break => format!("    jump {while_next_id}\n"),
            Statement::Continue => format!("    jump {while_id}\n"),
        }
    }
    pub fn block(&mut self, block: Block, while_id: &str, while_next_id: &str) -> (String, String) {
        let id = self.counter.get();
        let body: String = block
            .into_iter()
            .map(|item| match item {
                BlockItem::Def(def) => self.def(def),
                BlockItem::Block(block) => self.block(block, while_id, while_next_id).0,
                BlockItem::Statement(stmt) => self.statement(stmt, while_id, while_next_id),
            })
            .collect();
        (body, id)
    }
}
