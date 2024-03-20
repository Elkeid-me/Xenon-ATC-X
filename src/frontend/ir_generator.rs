use super::ast::{Expr::*, *};
use super::ty::Type;
use crate::risk;

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
            if (v[i].starts_with("    jump") || v[i].starts_with("    ret") || v[i].starts_with("    br"))
                && (v[i + 1].chars().last().unwrap() != ':' && v[i + 1] != "}")
            {
                v_2.push(format!("{}\n", v[i]));
                v_2.push(format!("{}:\n", self.counter.get()));
            } else {
                v_2.push(format!("{}\n", v[i]));
            }
        }
        v_2.push(format!("{}\n", v.last().unwrap()));
        let mut v_3: Vec<&str> = Vec::new();
        for i in 0..v_2.len() - 1 {
            if !(v_2[i].starts_with("    jump") || v_2[i].starts_with("    ret") || v_2[i].starts_with("    br"))
                && !v_2[i].ends_with("{\n")
                && (v_2[i + 1] == "}\n" || v_2[i + 1].ends_with(":\n"))
            {
                v_3.push(&v_2[i]);
                v_3.push("    ret\n");
            } else {
                v_3.push(&v_2[i]);
            }
        }
        v_3.push(v_2.last().unwrap());
        let ir: String = v_3.into_iter().collect();
        let global: String = self.global_const_init.iter().map(|str| str.as_str()).collect();
        format!("{global}{ir}")
    }
    fn array_elem_lvalue(&mut self, id: String, subscripts: Vec<Expr>) -> (String, String) {
        match &id[0..2] {
            "_P" => {
                let tmp_id_0 = self.counter.get();
                let mut id = format!("%{id}");
                let str_0 = format!("    {tmp_id_0} = load {id}\n");
                let tmp_id_1 = self.counter.get();
                let (expr_eval, expr_id) = self.expr_rvalue(subscripts[0].clone());
                let str_1 = format!("{expr_eval}\n    {tmp_id_1} = getptr {tmp_id_0}, {expr_id}\n");
                id = tmp_id_1;
                let str: String = subscripts
                    .into_iter()
                    .skip(1)
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
    fn array_elem_rvalue(&mut self, id: String, subscripts: Vec<Expr>, rvalue_int: bool) -> (String, String) {
        let (expr_eval, expr_id) = self.array_elem_lvalue(id, subscripts);
        if rvalue_int {
            let tmp_id = self.counter.get();
            (format!("{expr_eval}    {tmp_id} = load {expr_id}\n"), tmp_id)
        } else {
            let tmp_id = self.counter.get();
            (format!("{expr_eval}    {tmp_id} = getelemptr {expr_id}, 0\n"), tmp_id)
        }
    }
    fn expr_xvalue(&mut self, expr: Expr) -> String {
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
            | Leq(l, r) => format!("{}{}", self.expr_xvalue(*l), self.expr_xvalue(*r)),
            logic if matches!(logic, LogicAnd(_, _) | LogicOr(_, _)) => self.expr_rvalue(logic).0,
            LogicNot(expr) | Nega(expr) | Not(expr) => self.expr_xvalue(*expr),
            inc_dec if matches!(inc_dec, PostInc(_) | PostDec(_) | PreInc(_) | PreDec(_)) => self.expr_rvalue(inc_dec).0,
            Func(id, args) => {
                let (arg_str, arg_ids) = args
                    .into_iter()
                    .map(|expr| self.expr_rvalue(expr))
                    .reduce(|(l_str, l_id), (r_str, r_id)| (format!("{}{}", l_str, r_str), format!("{}, {}", l_id, r_id)))
                    .unwrap_or_default();
                format!("{}    call @{}({})\n", arg_str, id, arg_ids)
            }
            Array(_, subscripts, _) => subscripts.into_iter().map(|expr| self.expr_xvalue(expr)).collect(),
            Var(_) | Num(_) => String::new(),
            assign => self.expr_rvalue(assign).0,
        }
    }
    fn expr_lvalue(&mut self, expr: Expr) -> (String, String) {
        match expr {
            PreInc(expr) => self.inc_dec_helper(*expr, "add", true, true),
            PreDec(expr) => self.inc_dec_helper(*expr, "sub", true, true),
            Assignment(l, r) => {
                let (r_eval, r_id) = self.expr_rvalue(*r);
                let (l_eval, l_id) = self.expr_lvalue(*l);
                (format!("{r_eval}{l_eval}    store {r_id}, {l_id}\n"), l_id)
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
    fn assign_expr_helper(&mut self, l: Expr, r: Expr, op: &str, rvalue: bool) -> (String, String) {
        let (r_eval, r_id) = self.expr_rvalue(r);
        let (l_eval, l_id) = self.expr_lvalue(l);
        let tmp_id_1 = self.counter.get();
        let tmp_id_2 = self.counter.get();
        (format!("{r_eval}{l_eval}    {tmp_id_1} = load {l_id}\n    {tmp_id_2} = {op} {tmp_id_1}, {r_id}\n   store {tmp_id_2}, {l_id}\n"), if rvalue { tmp_id_2 } else { l_id })
    }
    fn arith_expr_helper(&mut self, l: Expr, r: Expr, op: &str) -> (String, String) {
        let id = self.counter.get();
        let (l_eval, l_id) = self.expr_rvalue(l);
        let (r_eval, r_id) = self.expr_rvalue(r);
        (format!("{l_eval}{r_eval}    {id} = {op} {l_id}, {r_id}\n"), id)
    }
    fn arith_unary_helper(&mut self, expr: Expr, operate: &str) -> (String, String) {
        let id = self.counter.get();
        let (expr_eval, expr_id) = self.expr_rvalue(expr);
        (format!("{expr_eval}    {id} = {operate}, {expr_id}\n"), id)
    }
    fn inc_dec_helper(&mut self, expr: Expr, op: &str, prefix: bool, rvalue: bool) -> (String, String) {
        let (expr_eval, expr_id) = self.expr_lvalue(expr);
        let tmp_id_1 = self.counter.get();
        let tmp_id_2 = self.counter.get();
        match (rvalue, prefix) {
            (true, true) => (format!("{expr_eval}\n    {tmp_id_1} = load {expr_id}\n    {tmp_id_2} = {op} {expr_id}, 1\n    store {tmp_id_2}, {expr_id}"), tmp_id_2),
            (true, false) => (format!("{expr_eval}\n    {tmp_id_1} = load {expr_id}\n    {tmp_id_2} = {op} {expr_id}, 1\n    store {tmp_id_2}, {expr_id}"), tmp_id_1),
            (false, true) => (format!("{expr_eval}\n    {tmp_id_1} = load {expr_id}\n    {tmp_id_2} = {op} {expr_id}, 1\n    store {tmp_id_2}, {expr_id}"), expr_id),
            (false, false) => unreachable!()
        }
    }
    fn expr_rvalue(&mut self, expr: Expr) -> (String, String) {
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
                    .reduce(|(l_str, l_id), (r_str, r_id)| (format!("{}{}", l_str, r_str), format!("{}, {}", l_id, r_id)))
                    .unwrap_or_default();
                let tmp_id = self.counter.get();
                (format!("{}    {} = call @{}({})\n", arg_str, tmp_id, fun_id, arg_ids), tmp_id)
            }
            Array(id, subscripts, rvalue_int) => self.array_elem_rvalue(id, subscripts, rvalue_int),
        }
    }
    fn statement(&mut self, statement: Statement, while_id: &str, while_next_id: &str) -> String {
        match statement {
            Statement::Expr(expr) => self.expr_xvalue(expr),
            Statement::If(condition, then_block, else_block) => match condition {
                Num(0) => {
                    if else_block.is_empty() {
                        String::new()
                    } else {
                        let (else_str, _) = self.block(*else_block, while_id, while_next_id);
                        else_str
                    }
                }
                Num(_) => {
                    let (then_str, _) = self.block(*then_block, while_id, while_next_id);
                    then_str
                }
                _ => {
                    let next_block_id = self.counter.get();
                    let (cond_eval, cond_id) = self.expr_rvalue(condition);
                    let (then_str, then_id) = self.block(*then_block, while_id, while_next_id);
                    if else_block.is_empty() {
                        format!(
                            r"{cond_eval}    br {cond_id}, {then_id}, {next_block_id}
{then_id}:
{then_str}    jump {next_block_id}
{next_block_id}:
"
                        )
                    } else {
                        let (else_str, else_id) = self.block(*else_block, while_id, while_next_id);
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
            },
            Statement::While(condition, block) => match condition {
                Num(0) => String::new(),
                Num(_) => {
                    let while_id = self.counter.get();
                    let while_next_id = self.counter.get();
                    let (block_str, _) = self.block(*block, &while_id, &while_next_id);
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
                    let (cond_str, cond_id) = self.expr_rvalue(condition);
                    let (block_str, block_id) = self.block(*block, &while_id, &while_next_id);
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
            },
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
    fn block(&mut self, block: Block, while_id: &str, while_next_id: &str) -> (String, String) {
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
    fn fun_decl(&self, id: String, ret_type: Type, para_type: Vec<Type>) -> String {
        let ret_type_str = match ret_type {
            Type::Int => ": i32",
            Type::Void => "",
            _ => unreachable!(),
        };
        let para_list_str =
            para_type.iter().map(|ty| format!("{}", ty.to_koopa_type_str())).reduce(|l, r| format!("{l}, {r}")).unwrap_or_default();
        format!("decl @{id}({para_list_str}){ret_type_str}\n")
    }
    fn fun_def(&mut self, id: String, ret_type: Type, para_type: Vec<Type>, para_id: Vec<String>, block: Block) -> String {
        let ret_type_str = match ret_type {
            Type::Int => ": i32",
            Type::Void => "",
            _ => unreachable!(),
        };
        let para_list_str = para_id
            .iter()
            .zip(para_type.iter())
            .map(|(id, ty)| format!("@{id}: {}", ty.to_koopa_type_str()))
            .reduce(|l, r| format!("{l}, {r}"))
            .unwrap_or_default();
        let entry_id = self.counter.get();
        let para_alloc: String = para_id
            .into_iter()
            .zip(para_type.into_iter())
            .map(|(id, ty)| format!("    %{} = alloc {}\n    store @{}, %{}\n", id, ty.to_koopa_type_str(), id, id))
            .collect();
        let (block, _) = self.block(block, "", "");
        format!("fun @{id}({para_list_str}){ret_type_str} {{\n{entry_id}:\n{para_alloc}\n{block}\n}}\n")
    }
    fn def(&mut self, def: Definition) -> String {
        match def {
            (Type::Int, id, None) => format!("    %{id} = alloc i32\n"),
            (Type::Int, _, Some(Init::Const(_))) => String::new(),
            (Type::Int, id, Some(Init::Expr(expr))) => {
                let (expr_eval, expr_id) = self.expr_rvalue(expr);
                format!("{expr_eval}    %{id} = alloc i32\n    store {expr_id}, %{id}\n")
            }
            (Type::IntArray(len), id, Some(Init::ConstInitList(list))) => {
                let init_str = Self::const_init_list_to_str(&len, list);
                let ty_str = Type::IntArray(len).to_koopa_type_str();
                self.global_const_init.push(format!("global %{id} = alloc {ty_str}, {init_str}\n"));
                String::new()
            }
            (Type::IntArray(len), id, Some(Init::InitList(list))) => self.local_array(Type::IntArray(len), id, list),
            (Type::IntArray(len), id, None) => {
                let ty_str = Type::IntArray(len).to_koopa_type_str();
                format!("    %{} = alloc {}\n", id, ty_str)
            }
            _ => unimplemented!(),
        }
    }
    fn init_list_to_str(len: &[usize], list: InitList) -> String {
        let content = list
            .into_iter()
            .map(|item| match item {
                InitListItem::InitList(l) => Self::init_list_to_str(&len[1..], *l),
                InitListItem::Expr(e) => e.get_num().to_string(),
            })
            .reduce(|l, r| format!("{l}, {r}"))
            .unwrap_or_default();
        format!("{{{content}}}")
    }
    fn const_init_list_to_str(len: &[usize], list: ConstInitList) -> String {
        let content = list
            .into_iter()
            .map(|item| match item {
                ConstInitListItem::ConstInitList(l) => Self::const_init_list_to_str(&len[1..], *l),
                ConstInitListItem::Num(i) => i.to_string(),
            })
            .reduce(|l, r| format!("{l}, {r}"))
            .unwrap_or_default();
        format!("{{{content}}}")
    }
    fn global_def(&mut self, def: Definition) -> String {
        match def {
            (Type::Function(ret_type, para_type), id, Some(Init::Function(para_id, block))) => {
                self.fun_def(id, *ret_type, para_type, para_id, block)
            }
            (Type::Function(ret_type, para_type), id, None) => self.fun_decl(id, *ret_type, para_type),
            (Type::Int, id, None) => format!("global %{id} = alloc i32, 0\n"),
            (Type::Int, id, Some(Init::Expr(expr))) => {
                let (expr_eval, expr_id) = self.expr_rvalue(expr);
                format!("{expr_eval}global %{id} = alloc i32, {expr_id}\n")
            }
            (Type::IntArray(len), id, None) => {
                let ty_str = Type::IntArray(len).to_koopa_type_str();
                format!("global %{id} = alloc {ty_str}, zeroinit\n")
            }
            (Type::IntArray(len), id, Some(Init::InitList(list))) => {
                let init_str = Self::init_list_to_str(&len, list);
                let ty_str = Type::IntArray(len).to_koopa_type_str();
                format!("global %{id} = alloc {ty_str}, {init_str}\n")
            }
            (Type::IntArray(len), id, Some(Init::ConstInitList(list))) => {
                let init_str = Self::const_init_list_to_str(&len, list);
                let ty_str = Type::IntArray(len).to_koopa_type_str();
                format!("global %{id} = alloc {ty_str}, {init_str}\n")
            }
            _ => String::new(),
        }
    }
    fn local_array_impl(&mut self, len: &[usize], id: &str, list: Vec<InitListItem>) -> String {
        match len.len() {
            1 => list
                .into_iter()
                .enumerate()
                .map(|(i, item)| {
                    let (expr_eval, expr_id) = self.expr_rvalue(risk!(item, InitListItem::Expr(expr) => expr));
                    let tmp_id = self.counter.get();
                    format!("{expr_eval}    {tmp_id} = getelemptr {id}, {i}\n    store {expr_id}, {tmp_id}\n")
                })
                .collect(),
            _ => list
                .into_iter()
                .enumerate()
                .map(|(i, item)| {
                    let tmp_id = self.counter.get();
                    let str = self.local_array_impl(&len[1..], &tmp_id, risk!(item, InitListItem::InitList(list) => *list));
                    format!("    {tmp_id} = getelemptr {id}, {i}\n{str}")
                })
                .collect(),
        }
    }
    fn local_array(&mut self, ty: Type, id: String, list: Vec<InitListItem>) -> String {
        let local_id = format!("%{}", id);
        let alloc = format!("    {} = alloc {}\n", &local_id, ty.to_koopa_type_str());
        let len = risk!(ty, Type::IntArray(len) => len);
        format!("{}{}", alloc, self.local_array_impl(&len, &local_id, list))
    }
}

pub fn generator_ir(ast: TranslationUnit) -> String {
    Generator::new().generate(ast)
}
