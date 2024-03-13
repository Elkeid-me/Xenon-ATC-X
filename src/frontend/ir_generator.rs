use std::io::Empty;

use super::ast::{Expr::*, *};
use super::ty::Type;
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
}

impl Generator {
    fn new() -> Self {
        Self {
            counter: Counter { value: 0 },
        }
    }
    fn generate(&mut self, ast: TranslationUnit) -> String {
        let prelude = r"decl @getint(): i32
decl @getch(): i32
decl @getarray(*i32): i32
decl @putint(i32): i32
decl @putch(i32): i32
decl @putarray(i32, *i32): i32
decl @starttime(): i32
decl @stoptime(): i32";
        let ir: String = ast.into_iter().map(|global_item| self.global_def(global_item)).collect();
        let ir = ir.split('\n').filter(|s| !s.is_empty());
        let mut v = Vec::new();
        let mut flag = false;
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
                && (v[i + 1].chars().last().unwrap() != ':' && v[i + 1].chars().last().unwrap() != '}')
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
                && (v_2[i + 1].ends_with("}\n") || v_2[i + 1].ends_with(":\n"))
            {
                v_3.push(&v_2[i]);
                v_3.push("    ret\n");
            } else {
                v_3.push(&v_2[i]);
            }
        }
        v_3.push(v_2.last().unwrap());
        let ir: String = v_3.into_iter().collect();
        format!("{}\n{}", prelude, ir)
    }
    fn array_elem_lvalue(id: String, subscripts: Vec<Expr>) -> (String, String) {
        todo!()
    }
    fn array_elem_rvalue(id: String, subscripts: Vec<Expr>) -> (String, String) {
        todo!()
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
            PostInc(_) => todo!(),
            PostDec(_) => todo!(),
            PreInc(_) => todo!(),
            PreDec(_) => todo!(),
            Func(id, args) => {
                let (arg_str, arg_ids) = args
                    .into_iter()
                    .map(|expr| self.expr_rvalue(expr))
                    .reduce(|(l_str, l_id), (r_str, r_id)| (format!("{}{}", l_str, r_str), format!("{}, {}", l_id, r_id)))
                    .unwrap_or_default();
                format!("{}    call @{}({})\n", arg_str, id, arg_ids)
            }
            Array(_, subscripts) => subscripts.into_iter().map(|expr| self.expr_xvalue(expr)).collect(),
            Var(_) | Num(_) => String::new(),
            assign => self.expr_rvalue(assign).0,
        }
    }
    fn expr_lvalue(&mut self, expr: Expr) -> (String, String) {
        match expr {
            PostInc(_) => todo!(),
            PostDec(_) => todo!(),
            PreInc(_) => todo!(),
            PreDec(_) => todo!(),
            Assignment(_, _) => todo!(),
            AddAssign(_, _) => todo!(),
            SubAssign(_, _) => todo!(),
            MulAssign(_, _) => todo!(),
            AndAssign(_, _) => todo!(),
            OrAssign(_, _) => todo!(),
            XorAssign(_, _) => todo!(),
            ShLAssign(_, _) => todo!(),
            SaRAssign(_, _) => todo!(),
            Var(x) => (String::new(), format!("%{x}")),
            Array(id, subscripts) => todo!(),
            _ => unreachable!(),
        }
    }
    fn expr_rvalue(&mut self, expr: Expr) -> (String, String) {
        let id = self.counter.get();
        match expr {
            Mul(l, r) => {
                let (l_eval, l_id) = self.expr_rvalue(*l);
                let (r_eval, r_id) = self.expr_rvalue(*r);
                (format!("{l_eval}{r_eval}    {id} = mul {l_id}, {r_id}\n"), id)
            }
            Div(l, r) => {
                let (l_eval, l_id) = self.expr_rvalue(*l);
                let (r_eval, r_id) = self.expr_rvalue(*r);
                (format!("{l_eval}{r_eval}    {id} = div {l_id}, {r_id}\n"), id)
            }
            Mod(l, r) => {
                let (l_eval, l_id) = self.expr_rvalue(*l);
                let (r_eval, r_id) = self.expr_rvalue(*r);
                (format!("{l_eval}{r_eval}    {id} = mod {l_id}, {r_id}\n"), id)
            }
            Add(l, r) => {
                let (l_eval, l_id) = self.expr_rvalue(*l);
                let (r_eval, r_id) = self.expr_rvalue(*r);
                (format!("{l_eval}{r_eval}    {id} = add {l_id}, {r_id}\n"), id)
            }
            Sub(l, r) => {
                let (l_eval, l_id) = self.expr_rvalue(*l);
                let (r_eval, r_id) = self.expr_rvalue(*r);
                (format!("{l_eval}{r_eval}    {id} = sub {l_id}, {r_id}\n"), id)
            }
            ShL(l, r) => {
                let (l_eval, l_id) = self.expr_rvalue(*l);
                let (r_eval, r_id) = self.expr_rvalue(*r);
                (format!("{l_eval}{r_eval}    {id} = shl {l_id}, {r_id}\n"), id)
            }
            ShR(l, r) => {
                let (l_eval, l_id) = self.expr_rvalue(*l);
                let (r_eval, r_id) = self.expr_rvalue(*r);
                (format!("{l_eval}{r_eval}    {id} = asr {l_id}, {r_id}\n"), id)
            }
            Xor(l, r) => {
                let (l_eval, l_id) = self.expr_rvalue(*l);
                let (r_eval, r_id) = self.expr_rvalue(*r);
                (format!("{l_eval}{r_eval}    {id} = xor {l_id}, {r_id}\n"), id)
            }
            And(l, r) => {
                let (l_eval, l_id) = self.expr_rvalue(*l);
                let (r_eval, r_id) = self.expr_rvalue(*r);
                (format!("{l_eval}{r_eval}    {id} = and {l_id}, {r_id}\n"), id)
            }
            Or(l, r) => {
                let (l_eval, l_id) = self.expr_rvalue(*l);
                let (r_eval, r_id) = self.expr_rvalue(*r);
                (format!("{l_eval}{r_eval}    {id} = or {l_id}, {r_id}\n"), id)
            }
            Eq(l, r) => {
                let (l_eval, l_id) = self.expr_rvalue(*l);
                let (r_eval, r_id) = self.expr_rvalue(*r);
                (format!("{l_eval}{r_eval}    {id} = eq {l_id}, {r_id}\n"), id)
            }
            Neq(l, r) => {
                let (l_eval, l_id) = self.expr_rvalue(*l);
                let (r_eval, r_id) = self.expr_rvalue(*r);
                (format!("{l_eval}{r_eval}    {id} = ne {l_id}, {r_id}\n"), id)
            }
            Grt(l, r) => {
                let (l_eval, l_id) = self.expr_rvalue(*l);
                let (r_eval, r_id) = self.expr_rvalue(*r);
                (format!("{l_eval}{r_eval}    {id} = gt {l_id}, {r_id}\n"), id)
            }
            Geq(l, r) => {
                let (l_eval, l_id) = self.expr_rvalue(*l);
                let (r_eval, r_id) = self.expr_rvalue(*r);
                (format!("{l_eval}{r_eval}    {id} = ge {l_id}, {r_id}\n"), id)
            }
            Les(l, r) => {
                let (l_eval, l_id) = self.expr_rvalue(*l);
                let (r_eval, r_id) = self.expr_rvalue(*r);
                (format!("{l_eval}{r_eval}    {id} = lt {l_id}, {r_id}\n"), id)
            }
            Leq(l, r) => {
                let (l_eval, l_id) = self.expr_rvalue(*l);
                let (r_eval, r_id) = self.expr_rvalue(*r);
                (format!("{l_eval}{r_eval}    {id} = le {l_id}, {r_id}\n"), id)
            }
            LogicAnd(l, r) => {
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
            LogicNot(expr) => {
                let (expr_eval, expr_id) = self.expr_rvalue(*expr);
                (format!("{expr_eval}    {id} = eq 0, {expr_id}\n"), id)
            }
            Nega(expr) => {
                let (expr_eval, expr_id) = self.expr_rvalue(*expr);
                (format!("{expr_eval}    {id} = sub 0, {expr_id}\n"), id)
            }
            Not(expr) => {
                let (expr_eval, expr_id) = self.expr_rvalue(*expr);
                (format!("{expr_eval}    {id} = xor 1, {expr_id}\n"), id)
            }
            PostInc(expr) => todo!(),
            PostDec(expr) => todo!(),
            PreInc(expr) => todo!(),
            PreDec(expr) => todo!(),
            Assignment(l, r) => {
                let (r_eval, r_id) = self.expr_rvalue(*r);
                let (l_eval, l_id) = self.expr_lvalue(*l);
                (format!("{r_eval}{l_eval}    store {r_id}, {l_id}\n"), r_id)
            }
            AddAssign(l, r) => todo!(),
            SubAssign(l, r) => todo!(),
            MulAssign(l, r) => todo!(),
            AndAssign(l, r) => todo!(),
            OrAssign(l, r) => todo!(),
            XorAssign(l, r) => todo!(),
            ShLAssign(l, r) => todo!(),
            SaRAssign(l, r) => todo!(),
            Num(i) => (String::new(), i.to_string()),
            Var(x) => match &x[0..2] {
                "_I" => {
                    let tmp_id = self.counter.get();
                    (format!("    {tmp_id} = load %{x}\n"), tmp_id)
                }
                "_P" => (String::new(), format!("%{x}")),
                _ => unreachable!(),
            },
            Func(fun_id, args) => {
                let (arg_str, arg_ids) = args
                    .into_iter()
                    .map(|expr| self.expr_rvalue(expr))
                    .reduce(|(l_str, l_id), (r_str, r_id)| (format!("{}{}", l_str, r_str), format!("{}, {}", l_id, r_id)))
                    .unwrap_or_default();
                let tmp_id = self.counter.get();
                (format!("{}    {} = call @{}({})\n", arg_str, tmp_id, fun_id, arg_ids), tmp_id)
            }
            Array(array_id, subscripts) => todo!(),
        }
    }
    fn statement(&mut self, statement: Statement, while_id: &str, while_next_id: &str) -> String {
        match statement {
            Statement::Expr(expr) => self.expr_xvalue(expr),
            Statement::If(condition, then_block, else_block) => {
                let next_block_id = self.counter.get();
                let (cond_str, cond_id) = self.expr_rvalue(condition);
                let (then_str, then_id) = self.block(*then_block, while_id, while_next_id);
                if else_block.is_empty() {
                    format!(
                        r"{cond_str}    br {cond_id}, {then_id}, {next_block_id}
{then_id}:
{then_str}    jump {next_block_id}
{next_block_id}:
"
                    )
                } else {
                    let (else_str, else_id) = self.block(*else_block, while_id, while_next_id);
                    format!(
                        r"{cond_str}    br {cond_id}, {then_id}, {else_id}
{then_id}:
{then_str}    jump {next_block_id}
{else_id}:
{else_str}    jump {next_block_id}
{next_block_id}:
"
                    )
                }
            }
            Statement::While(condition, block) => {
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
        format!(
            r"fun @{id}({para_list_str}){ret_type_str} {{
{entry_id}:
{para_alloc}
{block}
}}
"
        )
    }
    fn def(&mut self, def: Definition) -> String {
        match def {
            (Type::Int, id, None) => format!("    %{id} = alloc i32\n"),
            (Type::Int, _, Some(Init::Const(_))) => String::new(),
            (Type::Int, id, Some(Init::Expr(expr))) => {
                let (expr_eval, expr_id) = self.expr_rvalue(expr);
                format!(
                    r"{expr_eval}    %{id} = alloc i32
    store {expr_id}, %{id}
"
                )
            }
            _ => unimplemented!(),
        }
    }
    fn global_def(&mut self, def: Definition) -> String {
        match def {
            (Type::Function(ret_type, para_type), id, Some(Init::Function(para_id, block))) => {
                self.fun_def(id, *ret_type, para_type, para_id, block)
            }
            (Type::Int, id, None) => format!("global    %{id} = alloc i32, 0\n"),
            (Type::Int, id, Some(Init::Expr(expr))) => {
                let (expr_eval, expr_id) = self.expr_rvalue(expr);
                format!("{expr_eval}global %{id} = alloc i32, {expr_id}\n")
            }
            (Type::Int, _, Some(Init::Const(_))) => String::new(),
            _ => unimplemented!(),
        }
    }
}

pub fn generator_ir_eval(ast: TranslationUnit) -> String {
    Generator::new().generate(ast)
}

// use koopa::ir::entities::{FunctionData, Program, BasicBlockData};

// pub fn generate_ir(ast: TranslationUnit) -> Program {
//     let mut program = Program::new();
//     let mut counter = 0usize;
//     for ele in ast {
//         match ele {
//             (Type::Function(ret_ty, para_ty), id, Some(_))
//             | (Type::Function(ret_ty, para_ty), id, None) => {
//                 let new_func_decl =
//                     FunctionData::new_decl(format!("@{id}"), para_ty.into_iter().map(|ty| ty.to_koopa_type()).collect(), ret_ty.to_koopa_type());
//                 let new_bb = BasicBlockData::
//                 new_func_decl.layout_mut().bbs_mut().push_back(key, node)
//                 program.new_func(new_func_decl);
//             }
//             _ => (),
//         }
//     }
//     program
// }
