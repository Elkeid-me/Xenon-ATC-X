use super::ast::{Expr::*, ExprConst::*, *};
use super::ty::*;
use pest::pratt_parser::Assoc::{Left, Right};
use pest::pratt_parser::{Op, PrattParser};
use pest::{iterators::Pair, Parser};
use pest_derive::Parser;
use std::collections::{HashMap, HashSet};

#[derive(Parser)]
#[grammar = "frontend/sysy.pest"]
struct SysYParser;

// 符号的类型、重整化前的名字、常量初始化器
#[derive(Debug)]
pub enum ConstInit {
    Num(i32),
    List(ConstInitList),
}

#[derive(Debug)]
pub struct Symbol(pub Type, pub String, pub Option<ConstInit>);

pub struct SymbolTable {
    table: Vec<HashMap<String, Symbol>>,
    global_name_table: HashSet<String>,
    local_name_table: Option<HashSet<String>>,
}

pub trait Scope {
    fn search(&self, identifier: &str) -> Option<&Symbol>;
    fn insert_definition(&mut self, id: String, ty: Type, init: Option<ConstInit>) -> Result<String, String>;
    fn enter_scope(&mut self);
    fn exit_scope(&mut self);
    fn enter_function(&mut self);
    fn exit_function(&mut self);
    fn name_mangling(&self, id: String, ty: &Type) -> (String, usize);
}

fn demangling<'a>(mut id: &'a str) -> &'a str {
    if id.len() < 2 {
        return id;
    }
    let prefix = match &id[0..2] {
        "_I" => "_I",
        "_A" => "_A",
        "_P" => "_P",
        _ => return id,
    };
    loop {
        id = &id[2..];
        if !id.starts_with(prefix) {
            return id;
        }
    }
}

impl Scope for SymbolTable {
    fn search(&self, mut id: &str) -> Option<&Symbol> {
        // 去重整化
        id = demangling(id);
        for map in self.table.iter().rev() {
            if let Some(info) = map.get(id) {
                return Some(info);
            }
        }
        None
    }
    fn insert_definition(&mut self, id: String, ty: Type, init: Option<ConstInit>) -> Result<String, String> {
        let (mangled_id, prefix_len) = match ty {
            Type::Int | Type::IntPointer(_) | Type::IntArray(_) => self.name_mangling(id, &ty),
            _ => (id, 0),
        };
        let original_id = &mangled_id[prefix_len..];
        match self.table.last_mut().unwrap().insert(original_id.to_string(), Symbol(ty, mangled_id.clone(), init)) {
            Some(Symbol(Type::Keyword, _, _)) => Err(format!("标识符 {original_id} 是关键字，不能重定义")),
            Some(_) => Err(format!("标识符 {original_id} 在当前作用域中已存在")),
            None => {
                match &mut self.local_name_table {
                    Some(m) => m.insert(mangled_id.clone()),
                    None => self.global_name_table.insert(mangled_id.clone()),
                };
                Ok(mangled_id)
            }
        }
    }
    fn enter_scope(&mut self) {
        self.table.push(HashMap::new());
    }
    fn exit_scope(&mut self) {
        self.table.pop();
    }
    fn name_mangling(&self, mut id: String, ty: &Type) -> (String, usize) {
        let origin_len = id.len();
        loop {
            id = match ty {
                Type::Int => format!("_I{id}"),
                Type::IntPointer(_) => format!("_P{id}"),
                Type::IntArray(_) => format!("_A{id}"),
                _ => unreachable!(),
            };
            if !(match &self.local_name_table {
                Some(m) => m.contains(id.as_str()),
                None => false,
            }) && !(self.global_name_table.contains(id.as_str()))
            {
                let len = id.len() - origin_len;
                return (id, len);
            }
        }
    }
    fn enter_function(&mut self) {
        self.local_name_table = Some(HashSet::new());
        self.enter_scope();
    }
    fn exit_function(&mut self) {
        self.exit_scope();
        self.local_name_table = None;
    }
}
pub struct ASTBuilder {
    pub expr_parser: PrattParser<Rule>,
    pub symbol_table: SymbolTable,
}

impl ASTBuilder {
    pub(in super::parser) fn new() -> Self {
        let expr_parser = PrattParser::new()
            .op(Op::infix(Rule::assignment, Right)
                | Op::infix(Rule::add_assign, Right)
                | Op::infix(Rule::sub_assign, Right)
                | Op::infix(Rule::mul_assign, Right)
                | Op::infix(Rule::div_assign, Right)
                | Op::infix(Rule::mod_assign, Right)
                | Op::infix(Rule::and_assign, Right)
                | Op::infix(Rule::or_assign, Right)
                | Op::infix(Rule::xor_assign, Right)
                | Op::infix(Rule::shl_assign, Right)
                | Op::infix(Rule::sar_assign, Right))
            .op(Op::infix(Rule::logic_or, Left))
            .op(Op::infix(Rule::logic_and, Left))
            .op(Op::infix(Rule::xor, Left))
            .op(Op::infix(Rule::xor, Left))
            .op(Op::infix(Rule::and, Left))
            .op(Op::infix(Rule::eq, Left) | Op::infix(Rule::neq, Left))
            .op(Op::infix(Rule::grt, Left) | Op::infix(Rule::geq, Left) | Op::infix(Rule::les, Left) | Op::infix(Rule::leq, Left))
            .op(Op::infix(Rule::shl, Left) | Op::infix(Rule::sar, Left))
            .op(Op::infix(Rule::add, Left) | Op::infix(Rule::sub, Left))
            .op(Op::infix(Rule::mul, Left) | Op::infix(Rule::div, Left) | Op::infix(Rule::modu, Left))
            .op(Op::infix(Rule::custom, Left))
            .op(Op::prefix(Rule::logic_not)
                | Op::prefix(Rule::negative)
                | Op::prefix(Rule::positive)
                | Op::prefix(Rule::bit_not)
                | Op::prefix(Rule::pre_inc)
                | Op::prefix(Rule::pre_dec))
            .op(Op::postfix(Rule::post_inc) | Op::postfix(Rule::post_dec));
        let table = vec![HashMap::from([
            ("getint".to_string(), Symbol(Type::Function(Box::new(Type::Int), Vec::new()), String::new(), None)),
            ("getch".to_string(), Symbol(Type::Function(Box::new(Type::Int), Vec::new()), String::new(), None)),
            (
                "getarray".to_string(),
                Symbol(Type::Function(Box::new(Type::Int), vec![Type::IntPointer(Vec::new())]), String::new(), None),
            ),
            ("putint".to_string(), Symbol(Type::Function(Box::new(Type::Void), vec![Type::Int]), String::new(), None)),
            ("putch".to_string(), Symbol(Type::Function(Box::new(Type::Void), vec![Type::Int]), String::new(), None)),
            (
                "putarray".to_string(),
                Symbol(Type::Function(Box::new(Type::Int), vec![Type::Int, Type::IntPointer(Vec::new())]), String::new(), None),
            ),
            ("starttime".to_string(), Symbol(Type::Function(Box::new(Type::Void), Vec::new()), String::new(), None)),
            ("stoptime".to_string(), Symbol(Type::Function(Box::new(Type::Void), Vec::new()), String::new(), None)),
            ("if".to_string(), Symbol(Type::Keyword, String::new(), None)),
            ("while".to_string(), Symbol(Type::Keyword, String::new(), None)),
            ("break".to_string(), Symbol(Type::Keyword, String::new(), None)),
            ("continue".to_string(), Symbol(Type::Keyword, String::new(), None)),
            ("return".to_string(), Symbol(Type::Keyword, String::new(), None)),
            ("int".to_string(), Symbol(Type::Keyword, String::new(), None)),
            ("const".to_string(), Symbol(Type::Keyword, String::new(), None)),
            ("void".to_string(), Symbol(Type::Keyword, String::new(), None)),
        ])];
        let global_name_table = HashSet::from([
            "getint".to_string(),
            "getch".to_string(),
            "getarray".to_string(),
            "putint".to_string(),
            "putarray".to_string(),
            "starttime".to_string(),
            "stoptime".to_string(),
        ]);
        let symbol_table = SymbolTable {
            table,
            global_name_table,
            local_name_table: None,
        };
        Self {
            expr_parser,
            symbol_table,
        }
    }

    fn iter_to_vec(&self, iter: Option<Pair<Rule>>) -> Result<Vec<usize>, String> {
        match iter {
            Some(i) => i
                .into_inner()
                .map(|expr| {
                    let (expr, _, is_const) = self.process_expr_impl(expr)?;
                    match is_const {
                        ConstEval => {
                            let i = expr.get_num();
                            if i > 0 {
                                Ok(i as usize)
                            } else {
                                Err(format!("{expr:?} 的值不是正整数"))
                            }
                        }
                        NonConst => Err(format!("{expr:?} 不是整型常量表达式")),
                    }
                })
                .collect::<Result<Vec<_>, _>>(),
            None => Ok(Vec::new()),
        }
    }

    fn parse_definition(&mut self, pair: Pair<Rule>) -> Result<Definition, String> {
        match pair.as_rule() {
            Rule::const_variable_definition => {
                let mut iter = pair.into_inner();
                let id = iter.next().unwrap().as_str().to_string();
                match self.process_expr_impl(iter.next().unwrap())? {
                    (Num(i), _, ConstEval) => {
                        let new_id = self.symbol_table.insert_definition(id, Type::Int, Some(ConstInit::Num(i)))?;
                        Ok((Type::Int, new_id, Some(Init::Num(i))))
                    }
                    (e, _, _) => Err(format!("{e:?} 不是整型常量表达式")),
                }
            }
            Rule::variable_definition => {
                let mut iter = pair.into_inner();
                let id = iter.next().unwrap().as_str().to_string();
                match iter.next().map(|expr| self.process_expr_impl(expr)) {
                    Some(Ok((expr, RefType::Int, _))) => {
                        let new_id = self.symbol_table.insert_definition(id, Type::Int, None)?;
                        Ok((Type::Int, new_id, Some(Init::Expr(expr))))
                    }
                    Some(Ok((expr, _, _))) => Err(format!("{expr:?} 不是整型表达式")),
                    Some(Err(s)) => Err(s),
                    None => {
                        let new_id = self.symbol_table.insert_definition(id, Type::Int, None)?;
                        Ok((Type::Int, new_id, None))
                    }
                }
            }
            Rule::const_array_definition => {
                let mut iter = pair.into_inner();
                let id = iter.next().unwrap().as_str().to_string();
                let len = self.iter_to_vec(iter.next())?;
                let init_list = Vec::new();
                let mangled_id = self.symbol_table.insert_definition(id, Type::IntArray(len.clone()), None)?;
                Ok((Type::IntArray(len), mangled_id, Some(Init::ConstInitList(init_list))))
            }
            Rule::array_definition => {
                let mut iter = pair.into_inner();
                let id = iter.next().unwrap().as_str().to_string();
                let len = self.iter_to_vec(iter.next())?;
                let init_list = Vec::new();
                let mangled_id = self.symbol_table.insert_definition(id, Type::IntArray(len.clone()), None)?;
                Ok((Type::IntArray(len), mangled_id, Some(Init::InitList(init_list))))
            }
            _ => unreachable!(),
        }
    }

    fn parse_if_while_helper(&mut self, pair: Pair<Rule>, in_while: bool, ret_type: RefType) -> Result<Block, String> {
        match pair.as_rule() {
            Rule::block => self.parse_block(pair, in_while, ret_type),
            Rule::statement => Ok(vec![BlockItem::Statement(self.parse_statement(pair, in_while, ret_type)?)]),
            Rule::empty_statement => Ok(Vec::new()),
            Rule::definitions_in_if_or_while_non_block => {
                pair.into_inner().skip(1).map(|pair| Ok(BlockItem::Def(self.parse_definition(pair)?))).collect::<Result<_, _>>()
            }
            _ => unreachable!(),
        }
    }

    fn parse_if(&mut self, pair: Pair<Rule>, in_while: bool, ret_type: RefType) -> Result<Statement, String> {
        let mut iter = pair.into_inner();
        Ok(Statement::If(
            self.process_expr(iter.next().unwrap())?,
            Box::new(self.parse_if_while_helper(iter.next().unwrap(), in_while, ret_type)?),
            match iter.next() {
                Some(block) => Box::new(self.parse_if_while_helper(block, in_while, ret_type)?),
                None => Box::new(Vec::new()),
            },
        ))
    }

    fn parse_while(&mut self, pair: Pair<Rule>, ret_type: RefType) -> Result<Statement, String> {
        let mut iter = pair.into_inner();
        Ok(Statement::While(
            self.process_expr(iter.next().unwrap())?,
            Box::new(self.parse_if_while_helper(iter.next().unwrap(), true, ret_type)?),
        ))
    }

    fn parse_statement(&mut self, pair: Pair<Rule>, in_while: bool, ret_type: RefType) -> Result<Statement, String> {
        let iter = pair.into_inner().next().unwrap();
        match iter.as_rule() {
            Rule::expression => Ok(Statement::Expr(self.process_expr(iter)?)),
            Rule::return_statement => match (iter.into_inner().skip(1).next().map(|expr| self.process_expr_impl(expr)), ret_type) {
                (None, RefType::Void) => Ok(Statement::Return(None)),
                (Some(Ok((e, RefType::Int, _))), RefType::Int) => Ok(Statement::Return(Some(e))),
                (Some(Err(s)), _) => Err(s),
                _ => Err("return 语句的返回值与函数签名不匹配".to_string()),
            },
            Rule::if_statement => self.parse_if(iter, in_while, ret_type),
            Rule::while_statement => self.parse_while(iter, ret_type),
            Rule::break_keyword => {
                if in_while {
                    Ok(Statement::Break)
                } else {
                    Err("在 while 循环外使用了 break".to_string())
                }
            }
            Rule::continue_keyword => {
                if in_while {
                    Ok(Statement::Continue)
                } else {
                    Err("在 while 循环外使用了 continue".to_string())
                }
            }
            _ => unreachable!(),
        }
    }

    fn parse_block(&mut self, block: Pair<Rule>, in_while: bool, ret_type: RefType) -> Result<Block, String> {
        self.symbol_table.enter_scope();
        let block = block
            .into_inner()
            .filter(|pair| !matches!(pair.as_rule(), Rule::int_keyword | Rule::const_keyword))
            .map(|pair| match pair.as_rule() {
                Rule::block => Ok(BlockItem::Block(self.parse_block(pair, in_while, ret_type)?)),
                Rule::statement => Ok(BlockItem::Statement(self.parse_statement(pair, in_while, ret_type)?)),
                Rule::variable_definition
                | Rule::array_definition
                | Rule::const_variable_definition
                | Rule::const_array_definition => Ok(BlockItem::Def(self.parse_definition(pair)?)),
                _ => unreachable!(),
            })
            .collect::<Result<_, _>>();
        self.symbol_table.exit_scope();
        block
    }

    fn parse_signature(&self, pair: Pair<Rule>) -> Result<(String, Type, Vec<Type>, Vec<String>), String> {
        let mut iter = pair.into_inner();
        let return_type = match iter.next().unwrap().as_rule() {
            Rule::void_keyword => Type::Void,
            _ => Type::Int,
        };
        let id = iter.next().unwrap().as_str().to_string();
        let paras = iter.next().unwrap().into_inner().map(|para| match para.as_rule() {
            Rule::var_parameter_def => Ok((para.into_inner().skip(1).next().unwrap().as_str().to_string(), Type::Int)),
            Rule::ptr_parameter_def => {
                let mut iter = para.into_inner().skip(1);
                let id = iter.next().unwrap().as_str().to_string();
                let lengths = self.iter_to_vec(iter.next())?;
                Ok((id, Type::IntPointer(lengths)))
            }
            _ => unreachable!(),
        });
        let mut para_id = Vec::new();
        let mut para_type = Vec::new();
        for item in paras {
            match item {
                Ok((id, ty)) => {
                    para_id.push(id);
                    para_type.push(ty);
                }
                Err(s) => return Err(s),
            }
        }
        Ok((id, return_type, para_type, para_id))
    }

    fn parse_function(&mut self, func: Pair<Rule>) -> Result<Definition, String> {
        let mut iter = func.into_inner();
        let (id, ret_ty, para_type, mut para_id) = self.parse_signature(iter.next().unwrap())?;

        let mangled_id =
            self.symbol_table.insert_definition(id, Type::Function(Box::new(ret_ty.clone()), para_type.clone()), None)?;
        self.symbol_table.enter_function();
        for (ty, id) in para_type.iter().zip(para_id.iter_mut()) {
            *id = self.symbol_table.insert_definition(id.clone(), ty.clone(), None)?;
        }
        let block = self.parse_block(iter.next().unwrap(), false, ret_ty.to_ref_type())?;
        self.symbol_table.exit_function();

        Ok((Type::Function(Box::new(ret_ty), para_type), mangled_id, Some(Init::Function(para_id, block))))
    }

    fn parse_global_item(&mut self, pair: Pair<Rule>) -> Result<Definition, String> {
        match pair.as_rule() {
            Rule::variable_definition | Rule::array_definition | Rule::const_variable_definition | Rule::const_array_definition => {
                self.parse_definition(pair)
            }
            Rule::func_def => self.parse_function(pair),
            _ => unreachable!(),
        }
    }

    fn parse(mut self, code: &str) -> Result<TranslationUnit, String> {
        let ast = SysYParser::parse(Rule::translation_unit, code)
            .unwrap()
            .filter(|pair| !matches!(pair.as_rule(), Rule::EOI | Rule::int_keyword | Rule::const_keyword))
            .map(|pair| self.parse_global_item(pair))
            .collect::<Result<TranslationUnit, _>>()?;
        match self.symbol_table.search("main") {
            Some(Symbol(Type::Function(ret_type, para_ty), _, None))
                if matches!(ret_type.as_ref(), Type::Int) && para_ty.len() == 0 =>
            {
                Ok(ast)
            }
            _ => Err("没有 main 函数，或 main 函数不符合要求".to_string()),
        }
    }
}

pub fn parse(code: &str) -> Result<TranslationUnit, String> {
    ASTBuilder::new().parse(code)
}
