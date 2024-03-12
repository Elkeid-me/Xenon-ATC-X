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

pub enum SymbolTableItem<'a> {
    Symbol(RefType<'a>, Option<&'a Init>),
    Keyword,
}

use SymbolTableItem::*;

pub struct SymbolTable<'a> {
    table: Vec<HashMap<&'a str, SymbolTableItem<'a>>>,
    global_name_table: HashSet<&'a str>,
    local_name_table: Option<HashSet<&'a str>>,
}

pub trait Scope<'a> {
    fn search(&self, identifier: &str) -> Option<&SymbolTableItem>;
    fn insert_definition(&mut self, id: &'a str, symbol: SymbolTableItem<'a>) -> Result<(), String>;
    fn enter_scope(&mut self);
    fn exit_scope(&mut self);
    fn enter_function(&mut self);
    fn exit_function(&mut self);
    fn name_mangling(&self, id: String, ty: RefType) -> (String, usize);
}

impl<'a> Scope<'a> for SymbolTable<'a> {
    fn search(&self, identifier: &str) -> Option<&SymbolTableItem> {
        for map in self.table.iter().rev() {
            if let Some(info) = map.get(identifier) {
                return Some(info);
            }
        }
        None
    }
    fn insert_definition(&mut self, id: &'a str, symbol: SymbolTableItem<'a>) -> Result<(), String> {
        match self.table.last_mut().unwrap().insert(id, symbol) {
            Some(SymbolTableItem::Keyword) => Err(format!("标识符 {} 是关键字，不能重定义", id)),
            Some(_) => Err(format!("标识符 {} 在当前作用域中已存在", id)),
            None => Ok(()),
        }
    }
    fn enter_scope(&mut self) {
        self.table.push(HashMap::new());
    }
    fn exit_scope(&mut self) {
        self.table.pop();
    }
    fn name_mangling(&self, mut id: String, ty: RefType) -> (String, usize) {
        let origin_len = id.len();
        loop {
            id = match ty {
                RefType::Int => format!("_I_{id}"),
                RefType::IntPointer(_) => format!("_P_{id}"),
                RefType::IntArray(_) => format!("_A_{id}"),
                _ => unreachable!(),
            };
            if !(match &self.local_name_table {
                Some(m) => m.contains(id.as_str()),
                None => false,
            } || self.global_name_table.contains(id.as_str()))
            {
                return (id, origin_len);
            }
        }
    }
    fn enter_function(&mut self) {
        self.local_name_table = Some(HashSet::new());
    }
    fn exit_function(&mut self) {
        self.local_name_table = None;
    }
}
pub struct ASTBuilder<'a> {
    pub expr_parser: PrattParser<Rule>,
    pub symbol_table: SymbolTable<'a>,
}

impl ASTBuilder<'_> {
    pub(in super::parser) fn new() -> Self {
        static L_1: [Type; 1] = [Type::IntPointer(Vec::new())];
        static L_2: [Type; 2] = [Type::Int, Type::IntPointer(Vec::new())];
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
            ("getint", Symbol(RefType::Function(&Type::Int, &[]), None)),
            ("getch", Symbol(RefType::Function(&Type::Int, &[]), None)),
            ("getarray", Symbol(RefType::Function(&Type::Int, &L_1), None)),
            ("putint", Symbol(RefType::Function(&Type::Void, &[Type::Int]), None)),
            ("putch", Symbol(RefType::Function(&Type::Void, &[Type::Int]), None)),
            ("putarray", Symbol(RefType::Function(&Type::Int, &L_2), None)),
            ("starttime", Symbol(RefType::Function(&Type::Void, &[]), None)),
            ("stoptime", Symbol(RefType::Function(&Type::Void, &[]), None)),
            ("if", SymbolTableItem::Keyword),
            ("while", SymbolTableItem::Keyword),
            ("break", SymbolTableItem::Keyword),
            ("continue", SymbolTableItem::Keyword),
            ("return", SymbolTableItem::Keyword),
            ("int", SymbolTableItem::Keyword),
            ("const", SymbolTableItem::Keyword),
            ("void", SymbolTableItem::Keyword),
        ])];
        let global_name_table = HashSet::from(["getint", "getch", "getarray", "putint", "putarray", "starttime", "stoptime"]);
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
    // fn iter_to_usize_vec(&self, iter: Pairs<Rule>) -> Result<> {
    //     Pairs.
    // }

    fn parse_definition(&mut self, pair: Pair<Rule>) -> Result<Definition, String> {
        match pair.as_rule() {
            Rule::const_variable_definition => {
                let mut iter = pair.into_inner();
                let id = iter.next().unwrap().as_str().to_string();
                match self.process_expr_impl(iter.next().unwrap())? {
                    (Num(i), _, ConstEval) => Ok((Type::Int, id, Some(Init::Num(i)))),
                    (e, _, _) => Err(format!("表达式 {e:?} 不是整型常量表达式")),
                }
            }
            Rule::variable_definition => {
                let mut iter = pair.into_inner();
                let id = iter.next().unwrap().as_str().to_string();
                match iter.next().map(|expr| self.process_expr_impl(expr)) {
                    Some(Ok((expr, RefType::Int, _))) => Ok((Type::Int, id, Some(Init::Expr(expr)))),
                    Some(Ok((expr, _, _))) => Err(format!("表达式 {expr:?} 不是整型表达式")),
                    Some(Err(s)) => Err(s),
                    None => Ok((Type::Int, id, None)),
                }
            }
            Rule::const_array_definition => {
                let mut iter = pair.into_inner();
                let id = iter.next().unwrap().as_str().to_string();
                let len = iter
                    .next()
                    .unwrap()
                    .into_inner()
                    .map(|expr| Ok(self.process_expr(expr)?.get_num() as usize))
                    .collect::<Result<Vec<_>, String>>()?;
                let init_list = Vec::new();
                Ok((Type::IntArray(len), id, Some(Init::ConstInitList(init_list))))
            }
            Rule::array_definition => {
                let mut iter = pair.into_inner();
                let id = iter.next().unwrap().as_str().to_string();
                let len = iter
                    .next()
                    .unwrap()
                    .into_inner()
                    .map(|expr| Ok(self.process_expr(expr)?.get_num() as usize))
                    .collect::<Result<Vec<_>, String>>()?;
                let init_list = Vec::new();
                Ok((Type::IntArray(len), id, Some(Init::InitList(init_list))))
            }
            _ => unreachable!(),
        }
    }

    fn parse_if_while_helper(&mut self, pair: Pair<Rule>, in_while: bool, ret_type: RefType) -> Result<Block, String> {
        match pair.as_rule() {
            Rule::block => self.parse_block(pair, in_while, ret_type),
            Rule::statement => Ok(vec![BlockItem::Statement(self.parse_statement(pair, in_while, ret_type)?)]),
            Rule::empty_statement => Ok(Vec::new()),
            Rule::definitions_in_if_or_while_non_block => pair
                .into_inner()
                .skip(1)
                .map(|pair| Ok(BlockItem::Def(self.parse_definition(pair)?)))
                .collect::<Result<_, _>>(),
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
                let lengths = match iter.next() {
                    Some(i) => i
                        .into_inner()
                        .map(|expr| {
                            let (expr, _, is_const) = self.process_expr_impl(expr)?;
                            match is_const {
                                ConstEval => Ok(expr.get_num() as usize),
                                NonConst => Err(format!("{expr:?} 不是整型常量表达式")),
                            }
                        })
                        .collect::<Result<Vec<usize>, _>>()?,
                    None => Vec::new(),
                };
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
        let (id, ret_ty, para_type, para_id) = self.parse_signature(iter.next().unwrap())?;

        self.symbol_table.enter_function();
        let block = self.parse_block(iter.next().unwrap(), false, ret_ty.to_ref_type())?;
        self.symbol_table.exit_function();

        Ok((Type::Function(Box::new(ret_ty), para_type), id, Some(Init::Function(para_id, block))))
    }

    fn parse_global_item(&mut self, pair: Pair<Rule>) -> Result<Definition, String> {
        match pair.as_rule() {
            Rule::variable_definition | Rule::array_definition | Rule::const_variable_definition | Rule::const_array_definition => {
                todo!()
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
            Some(Symbol(RefType::Function(&Type::Int, &[]), None)) => Ok(ast),
            _ => Err("没有 main 函数，或 main 函数不符合要求".to_string()),
        }
    }
}

pub fn parse(code: &str) -> Result<TranslationUnit, String> {
    ASTBuilder::new().parse(code)
}
