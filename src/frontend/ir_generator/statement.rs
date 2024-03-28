use super::super::ast::{Expr::*, *};
use super::Generator;

impl Generator {
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
                let (cond_str, cond_id) = self.expr_rvalue(cond);
                let (block_str, block_id) = self.block(block, &while_id, &while_next_id);
                format!(
                    r"    jump {while_id}
{block_id}:
{block_str}    jump {while_id}
{while_id}:
{cond_str}    br {cond_id}, {block_id}, {while_next_id}
{while_next_id}:
"
                )
            }
        }
    }
    fn if_statement(&mut self, cond: Expr, then_block: Block, else_block: Block, while_id: &str, while_next_id: &str) -> String {
        match cond {
            Num(0) => {
                if else_block.is_empty() {
                    String::new()
                } else {
                    let (else_str, _) = self.block(else_block, while_id, while_next_id);
                    else_str
                }
            }
            Num(_) => {
                let (then_str, _) = self.block(then_block, while_id, while_next_id);
                then_str
            }
            _ => {
                let next_block_id = self.counter.get();
                let (cond_eval, cond_id) = self.expr_rvalue(cond);
                let (then_str, then_id) = self.block(then_block, while_id, while_next_id);
                if else_block.is_empty() {
                    format!(
                        r"{cond_eval}    br {cond_id}, {then_id}, {next_block_id}
{then_id}:
{then_str}    jump {next_block_id}
{next_block_id}:
"
                    )
                } else {
                    let (else_str, else_id) = self.block(else_block, while_id, while_next_id);
                    format!(
                        r"{cond_eval}    br {cond_id}, {then_id}, {else_id}
{then_id}:
{then_str}    jump {next_block_id}
{else_id}:
{else_str}    jump {next_block_id}
{next_block_id}:
"
                    )
                }
            }
        }
    }
    pub(super) fn statement(&mut self, statement: Statement, while_id: &str, while_next_id: &str) -> String {
        match statement {
            Statement::Expr(expr) => self.expr_dvalue(expr),
            Statement::If(condition, then_block, else_block) => {
                self.if_statement(condition, *then_block, *else_block, while_id, while_next_id)
            }
            Statement::While(condition, block) => self.while_statement(condition, *block),
            Statement::Return(expr) => match expr {
                Some(expr) => {
                    let (expr_str, expr_id) = self.expr_rvalue(expr);
                    format!("{}    ret {}\n", expr_str, expr_id)
                }
                None => "    ret\n".to_string(),
            },
            Statement::Break => format!("    jump {}\n", while_next_id),
            Statement::Continue => format!("    jump {}\n", while_id),
        }
    }
    pub(super) fn block(&mut self, block: Block, while_id: &str, while_next_id: &str) -> (String, String) {
        let id = self.counter.get();
        let body: String = block
            .into_iter()
            .map(|item| match item {
                BlockItem::Def(def) => self.def(def),
                BlockItem::Block(block) => format!("{}\n", self.block(block, while_id, while_next_id).0),
                BlockItem::Statement(stmt) => self.statement(stmt, while_id, while_next_id),
            })
            .collect();
        (body, id)
    }
}
