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

mod def;
mod dvalue;
mod lvalue;
mod rvalue;
mod statement;

use super::{ast::*, ty::Type};
use generator::{done, Gn};
use std::{collections::LinkedList, fmt::Write, mem::take};

struct Counter {
    value: usize,
}

impl Counter {
    fn get(&mut self) -> String {
        self.value += 1;
        format!("%_L_name_{}", self.value)
    }
}

struct Generator {
    counter: Counter,
    global_const_init: Vec<String>,
    translation_unit: TranslationUnit,
}

impl Generator {
    fn new(translation_unit: TranslationUnit) -> Self {
        Self { counter: Counter { value: 0 }, global_const_init: Vec::new(), translation_unit }
    }
    /// 谨慎地使用 `take()`. `take()` 可以看作是 C++ 中 `std::move()` 与移动构造的结合.
    ///
    /// 这里已经确定被 `take()` 的变量不会被第二次使用. 但是有一个例外. 考虑下面的代码：
    /// ```c
    /// int f(int, int);
    /// int main() { return f(1, 1); }
    /// int f(int a, int b) { return a + b; }
    /// ```
    /// 在语法树中，第一个 `f` 和第二个 `f` 有相同的 `handler`，而第一个 `f` 生成 IR 之后，
    /// 其 `type` 和 `init` 都被 `take()` 走了. 又因为 `take::<&mut T>()` 会在原来的位置留下
    /// `T.default()`，实际上第二个 `f` 变成了 `const int`.
    ///
    /// 编译器依赖此 bug 运行. 不要改动.
    fn search(&mut self, def: Definition) -> (Type, String, Option<Init>) {
        let (handler, id) = def;
        let ty = take(self.translation_unit.types.get_mut(&handler).unwrap());
        let init = take(self.translation_unit.inits.get_mut(&handler).unwrap());
        (ty, id, init)
    }
    fn generate(mut self) -> String {
        let ir: LinkedList<_> =
            take(&mut self.translation_unit.ast).into_iter().map(|global_item| self.global_def(global_item)).collect();
        let ir = Gn::new_scoped(|mut s| {
            for iter in ir {
                for str in iter.split('\n').filter(|s| !s.is_empty()) {
                    s.yield_(str.to_string());
                }
            }
            done!()
        });
        let postprocess = Gn::new_scoped(move |mut s| {
            let mut flag = true;
            for i in ir {
                if flag && (i.starts_with("    jump") || i.starts_with("    ret") || i.starts_with("    br")) {
                    s.yield_(i);
                    flag = false;
                } else if i.starts_with('%') && i.ends_with(':') || i == "}" {
                    s.yield_(i);
                    flag = true;
                } else if flag {
                    s.yield_(i);
                }
            }
            done!()
        });
        let add_ret = Gn::new_scoped(|mut s| {
            let mut flag = false;
            let mut ret_int = false;
            for i in postprocess {
                if i.ends_with("): i32 {") {
                    flag = true;
                    ret_int = true;
                } else if i.ends_with(") {") {
                    flag = true;
                    ret_int = false;
                } else if i.starts_with("    jump") || i.starts_with("    ret") || i.starts_with("    br") {
                    flag = true;
                } else if i.starts_with('%') && i.ends_with(':') || i == "}" {
                    if !flag && ret_int {
                        s.yield_("    ret 0".to_string());
                    } else if !flag && !ret_int {
                        s.yield_("    ret".to_string());
                    }
                    flag = false;
                }
                s.yield_(i);
            }
            done!()
        });
        let global = self.global_const_init.join("");
        let ir: String = add_ret.into_iter().fold(String::new(), |mut output, s| {
            let _ = writeln!(output, "{s}");
            output
        });
        format!("{global}{ir}")
    }
    fn assign_expr_helper(&mut self, l: Expr, r: Expr, op: &str, rvalue: bool) -> (String, String) {
        if rvalue {
            let (r_eval, r_id) = self.expr_rvalue(r);
            let (l_eval, l_id) = self.expr_lvalue(l);
            let tmp_id_1 = self.counter.get();
            let tmp_id_2 = self.counter.get();
            (
                format!(
                    r"{r_eval}{l_eval}    {tmp_id_1} = load {l_id}
    {tmp_id_2} = {op} {tmp_id_1}, {r_id}
   store {tmp_id_2}, {l_id}
"
                ),
                tmp_id_2,
            )
        } else {
            let r_eval = self.expr_dvalue(r);
            let (l_eval, l_id) = self.expr_lvalue(l);
            (format!("{r_eval}{l_eval}\n"), l_id)
        }
    }
    fn inc_dec_helper(&mut self, expr: Expr, op: &str, prefix: bool, rvalue: bool) -> (String, String) {
        let (expr_eval, expr_id) = self.expr_lvalue(expr);
        let tmp_id_1 = self.counter.get();
        let tmp_id_2 = self.counter.get();
        match (rvalue, prefix) {
            (true, true) => (
                format!(
                    r"{expr_eval}
    {tmp_id_1} = load {expr_id}
    {tmp_id_2} = {op} {expr_id}, 1
    store {tmp_id_2}, {expr_id}
"
                ),
                tmp_id_2,
            ),
            (true, false) => (
                format!(
                    r"{expr_eval}
    {tmp_id_1} = load {expr_id}
    {tmp_id_2} = {op} {expr_id}, 1
    store {tmp_id_2}, {expr_id}
"
                ),
                tmp_id_1,
            ),
            (false, true) => (format!("{expr_eval}\n"), expr_id),
            (false, false) => unreachable!(),
        }
    }
}

pub fn generator_ir(ast: TranslationUnit) -> String {
    Generator::new(ast).generate()
}
