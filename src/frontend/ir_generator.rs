use super::ast::*;
mod def;
mod dvalue;
mod lvalue;
mod rvalue;
mod statement;

struct Counter {
    value: usize,
}

impl Counter {
    fn get(&mut self) -> String {
        self.value += 1;
        format!("%{}", self.value)
    }
}

struct Generator {
    counter: Counter,
    global_const_init: Vec<String>,
}

impl Generator {
    fn new() -> Self {
        Self {
            counter: Counter { value: 0 },
            global_const_init: Vec::new(),
        }
    }
    fn generate(&mut self, ast: TranslationUnit) -> String {
        let ir: String = ast.into_iter().map(|global_item| self.global_def(global_item)).collect();
        let ir = ir.split('\n').filter(|s| !s.is_empty());
        let mut v = Vec::new();
        let mut flag = false;
        // 删除连续的 `jump`、`ret` 和 `br`.
        for i in ir {
            if i.starts_with("    jump") || i.starts_with("    ret") || i.starts_with("    br") {
                if !flag {
                    flag = true;
                    v.push(i);
                }
            } else {
                v.push(i);
                flag = false;
            }
        }
        let mut v_2 = Vec::new();
        for i in 0..v.len() - 1 {
            v_2.push(format!("{}\n", v[i]));
            if (v[i].starts_with("    jump") || v[i].starts_with("    ret") || v[i].starts_with("    br"))
                && (v[i + 1].chars().last().unwrap() != ':' && v[i + 1] != "}")
            {
                v_2.push(format!("{}:\n", self.counter.get()));
            }
        }
        v_2.push(format!("{}\n", v.last().unwrap()));
        let mut v_3: Vec<&str> = Vec::new();
        let mut flag = false;
        for i in 0..v_2.len() - 1 {
            if v_2[i].ends_with("): i32 {\n") {
                flag = true;
            } else if v_2[i].ends_with(") {\n") {
                flag = false;
            }
            v_3.push(&v_2[i]);
            if !(v_2[i].starts_with("    jump") || v_2[i].starts_with("    ret") || v_2[i].starts_with("    br"))
                && !v_2[i].ends_with("{\n")
                && (v_2[i + 1] == "}\n" || v_2[i + 1].ends_with(":\n"))
            {
                v_3.push(if flag { "    ret 0\n" } else { "    ret\n" });
            }
        }
        v_3.push(v_2.last().unwrap());
        let ir: String = v_3.into_iter().collect();
        let global: String = self.global_const_init.iter().map(|str| str.as_str()).collect();
        format!("{global}{ir}")
    }
    fn assign_expr_helper(&mut self, l: Expr, r: Expr, op: &str, rvalue: bool) -> (String, String) {
        if rvalue {
            let (r_eval, r_id) = self.expr_rvalue(r);
            let (l_eval, l_id) = self.expr_lvalue(l);
            let tmp_id_1 = self.counter.get();
            let tmp_id_2 = self.counter.get();
            (format!("{r_eval}{l_eval}    {tmp_id_1} = load {l_id}\n    {tmp_id_2} = {op} {tmp_id_1}, {r_id}\n   store {tmp_id_2}, {l_id}\n"), tmp_id_2)
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
            (true, true) => (format!("{expr_eval}\n    {tmp_id_1} = load {expr_id}\n    {tmp_id_2} = {op} {expr_id}, 1\n    store {tmp_id_2}, {expr_id}\n"), tmp_id_2),
            (true, false) => (format!("{expr_eval}\n    {tmp_id_1} = load {expr_id}\n    {tmp_id_2} = {op} {expr_id}, 1\n    store {tmp_id_2}, {expr_id}\n"), tmp_id_1),
            (false, true) => (format!("{expr_eval}\n"), expr_id),
            (false, false) => unreachable!()
        }
    }
}

pub fn generator_ir(ast: TranslationUnit) -> String {
    Generator::new().generate(ast)
}
