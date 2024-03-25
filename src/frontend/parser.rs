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

use super::ast::{ArithmeticOp::*, ArithmeticUnaryOp::*, AssignOp::*, Expr, ExprInner::*};
use super::ast::{InfixOp::*, LogicOp::*, OtherUnaryOp::*, UnaryOp::*, *};
use pest::pratt_parser::Assoc::{Left, Right};
use pest::pratt_parser::{Op, PrattParser};
use pest::{iterators::Pair, Parser};
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "frontend/sysy.pest"]
struct SysYParser;

fn new_expr_parser() -> PrattParser<Rule> {
    PrattParser::new()
        .op(Op::infix(Rule::assignment, Right)
            | Op::infix(Rule::add_assignment, Right)
            | Op::infix(Rule::subtract_assignment, Right)
            | Op::infix(Rule::multiply_assignment, Right)
            | Op::infix(Rule::divide_assignment, Right)
            | Op::infix(Rule::modulus_assignment, Right)
            | Op::infix(Rule::bit_and_assignment, Right)
            | Op::infix(Rule::bit_or_assignment, Right)
            | Op::infix(Rule::bit_xor_assignment, Right)
            | Op::infix(Rule::bit_left_shift_assignment, Right)
            | Op::infix(Rule::bit_right_shift_assignment, Right))
        .op(Op::infix(Rule::logical_or, Left))
        .op(Op::infix(Rule::logical_and, Left))
        .op(Op::infix(Rule::bit_xor, Left))
        .op(Op::infix(Rule::bit_xor, Left))
        .op(Op::infix(Rule::bit_and, Left))
        .op(Op::infix(Rule::equal, Left) | Op::infix(Rule::not_equal, Left))
        .op(Op::infix(Rule::greater, Left)
            | Op::infix(Rule::greater_or_equal, Left)
            | Op::infix(Rule::less, Left)
            | Op::infix(Rule::less_or_equal, Left))
        .op(Op::infix(Rule::bit_left_shift, Left) | Op::infix(Rule::bit_right_shift, Left))
        .op(Op::infix(Rule::add, Left) | Op::infix(Rule::subtract, Left))
        .op(Op::infix(Rule::multiply, Left) | Op::infix(Rule::divide, Left) | Op::infix(Rule::modulus, Left))
        .op(Op::infix(Rule::custom_operator, Left))
        .op(Op::prefix(Rule::prefix_self_decrease)
            | Op::prefix(Rule::prefix_self_increase)
            | Op::prefix(Rule::logical_not)
            | Op::prefix(Rule::negative)
            | Op::prefix(Rule::positive)
            | Op::prefix(Rule::bit_not))
        .op(Op::postfix(Rule::postfix_self_increase) | Op::postfix(Rule::postfix_self_decrease))
}

fn parse_expr(expr_parser: &PrattParser<Rule>, pair: Pair<Rule>) -> Expr {
    expr_parser
        .map_primary(|pair| match pair.as_rule() {
            Rule::expression => parse_expr(expr_parser, pair),
            Rule::integer_bin => Num(i32::from_str_radix(&pair.as_str()[2..], 2).unwrap()).into(),
            Rule::integer_oct => Num(i32::from_str_radix(pair.as_str(), 8).unwrap()).into(),
            Rule::integer_dec => Num(i32::from_str_radix(pair.as_str(), 10).unwrap()).into(),
            Rule::integer_hex => Num(i32::from_str_radix(&pair.as_str()[2..], 16).unwrap()).into(),
            Rule::identifier => Identifier(pair.as_str().to_string()).into(),
            Rule::function_call => {
                let mut iter = pair.into_inner();
                let id = iter.next().unwrap().as_str().to_string();
                let arg_list = iter.map(|p| parse_expr(expr_parser, p)).collect();
                FunctionCall(id, arg_list).into()
            }
            Rule::array_element => {
                let mut iter = pair.into_inner();
                let id = iter.next().unwrap().as_str().to_string();
                let subscripts = iter
                    .next()
                    .unwrap()
                    .into_inner()
                    .map(|p| parse_expr(expr_parser, p))
                    .collect();
                ArrayElement(id, subscripts, false).into()
            }
            _ => unreachable!(),
        })
        .map_infix(|lhs, op, rhs| match op.as_rule() {
            Rule::custom_operator => FunctionCall(op.into_inner().as_str().to_string(), vec![lhs, rhs]).into(),
            Rule::multiply => InfixExpr(Box::new(lhs), Arith(Multiply), Box::new(rhs)).into(),
            Rule::divide => InfixExpr(Box::new(lhs), Arith(Divide), Box::new(rhs)).into(),
            Rule::modulus => InfixExpr(Box::new(lhs), Arith(Modulus), Box::new(rhs)).into(),
            Rule::add => InfixExpr(Box::new(lhs), Arith(Add), Box::new(rhs)).into(),
            Rule::subtract => InfixExpr(Box::new(lhs), Arith(Subtract), Box::new(rhs)).into(),

            Rule::logical_and => InfixExpr(Box::new(lhs), Logic(LogicalAnd), Box::new(rhs)).into(),
            Rule::logical_or => InfixExpr(Box::new(lhs), Logic(LogicalOr), Box::new(rhs)).into(),

            Rule::bit_left_shift => InfixExpr(Box::new(lhs), Arith(BitLeftShift), Box::new(rhs)).into(),
            Rule::bit_right_shift => InfixExpr(Box::new(lhs), Arith(BitRightShift), Box::new(rhs)).into(),
            Rule::bit_xor => InfixExpr(Box::new(lhs), Arith(BirXor), Box::new(rhs)).into(),
            Rule::bit_and => InfixExpr(Box::new(lhs), Arith(BitAnd), Box::new(rhs)).into(),
            Rule::bit_or => InfixExpr(Box::new(lhs), Arith(BitOr), Box::new(rhs)).into(),

            Rule::equal => InfixExpr(Box::new(lhs), Arith(Equal), Box::new(rhs)).into(),
            Rule::not_equal => InfixExpr(Box::new(lhs), Arith(NotEqual), Box::new(rhs)).into(),
            Rule::greater => InfixExpr(Box::new(lhs), Arith(Greater), Box::new(rhs)).into(),
            Rule::greater_or_equal => InfixExpr(Box::new(lhs), Arith(GreaterOrEqual), Box::new(rhs)).into(),
            Rule::less => InfixExpr(Box::new(lhs), Arith(Less), Box::new(rhs)).into(),
            Rule::less_or_equal => InfixExpr(Box::new(lhs), Arith(LessOrEqual), Box::new(rhs)).into(),

            Rule::assignment => InfixExpr(Box::new(lhs), Assign(Assignment), Box::new(rhs)).into(),
            Rule::add_assignment => InfixExpr(Box::new(lhs), Assign(AddAssign), Box::new(rhs)).into(),
            Rule::subtract_assignment => InfixExpr(Box::new(lhs), Assign(SubtractAssign), Box::new(rhs)).into(),
            Rule::multiply_assignment => InfixExpr(Box::new(lhs), Assign(MultiplyAssign), Box::new(rhs)).into(),
            Rule::bit_and_assignment => InfixExpr(Box::new(lhs), Assign(BitAndAssign), Box::new(rhs)).into(),
            Rule::bit_or_assignment => InfixExpr(Box::new(lhs), Assign(BitOrAssign), Box::new(rhs)).into(),
            Rule::bit_xor_assignment => InfixExpr(Box::new(lhs), Assign(BitXorAssign), Box::new(rhs)).into(),
            Rule::bit_left_shift_assignment => InfixExpr(Box::new(lhs), Assign(BitLeftShiftAssign), Box::new(rhs)).into(),
            Rule::bit_right_shift_assignment => InfixExpr(Box::new(lhs), Assign(BitRightShiftAssign), Box::new(rhs)).into(),
            _ => unreachable!(),
        })
        .map_prefix(|op, rhs| match op.as_rule() {
            Rule::prefix_self_increase => UnaryExpr(Others(PrefixSelfIncrease), Box::new(rhs)).into(),
            Rule::prefix_self_decrease => UnaryExpr(Others(PrefixSelfDecrease), Box::new(rhs)).into(),
            Rule::logical_not => UnaryExpr(ArithUnary(LogicalNot), Box::new(rhs)).into(),
            Rule::negative => UnaryExpr(ArithUnary(Negative), Box::new(rhs)).into(),
            Rule::positive => rhs.into(),
            Rule::bit_not => UnaryExpr(ArithUnary(BitNot), Box::new(rhs)).into(),
            _ => unreachable!(),
        })
        .map_postfix(|lhs, op| match op.as_rule() {
            Rule::postfix_self_increase => UnaryExpr(Others(PostfixSelfIncrease), Box::new(lhs)).into(),
            Rule::postfix_self_decrease => UnaryExpr(Others(PostfixSelfDecrease), Box::new(lhs)).into(),
            _ => unreachable!(),
        })
        .parse(pair.into_inner())
}

fn parse_init_list_item(expr_parser: &PrattParser<Rule>, pair: Pair<Rule>) -> InitListItem {
    match pair.as_rule() {
        Rule::initializer_list => InitListItem::InitList(Box::new(parse_init_list(expr_parser, pair))),
        Rule::expression => InitListItem::Expr(parse_expr(expr_parser, pair)),
        _ => unreachable!(),
    }
}

fn parse_init_list(expr_parser: &PrattParser<Rule>, pair: Pair<Rule>) -> InitList {
    pair.into_inner()
        .map(|pair| parse_init_list_item(expr_parser, pair))
        .collect()
}

fn parse_definition(expr_parser: &PrattParser<Rule>, pair: Pair<Rule>) -> Definition {
    match pair.as_rule() {
        Rule::const_variable_definition => {
            let mut iter = pair.into_inner();
            Definition::ConstVariableDefTmp(iter.next().unwrap().as_str().to_string(), parse_expr(expr_parser, iter.next().unwrap()))
        }
        Rule::variable_definition => {
            let mut iter = pair.into_inner();
            Definition::VariableDef(iter.next().unwrap().as_str().to_string(), iter.next().map(|expr| parse_expr(expr_parser, expr)))
        }
        Rule::const_array_definition => {
            let mut iter = pair.into_inner();
            Definition::ConstArrayDefTmp {
                id: iter.next().unwrap().as_str().to_string(),
                lengths: iter
                    .next()
                    .unwrap()
                    .into_inner()
                    .map(|expr| parse_expr(expr_parser, expr))
                    .collect(),
                init_list: parse_init_list(expr_parser, iter.next().unwrap()),
            }
        }
        Rule::array_definition => {
            let mut iter = pair.into_inner();
            Definition::ArrayDefTmp {
                id: iter.next().unwrap().as_str().to_string(),
                lengths: iter
                    .next()
                    .unwrap()
                    .into_inner()
                    .map(|expr| parse_expr(expr_parser, expr))
                    .collect(),
                init_list: iter.next().map(|init_list| parse_init_list(expr_parser, init_list)),
            }
        }
        _ => unreachable!(),
    }
}

fn parse_if_while_helper(expr_parser: &PrattParser<Rule>, pair: Pair<Rule>) -> Block {
    match pair.as_rule() {
        Rule::block => parse_block(expr_parser, pair),
        Rule::expression
        | Rule::return_statement
        | Rule::if_statement
        | Rule::while_statement
        | Rule::break_keyword
        | Rule::continue_keyword => vec![BlockItem::Statement(Box::new(parse_statement(expr_parser, pair)))],
        Rule::empty_statement => Vec::new(),
        Rule::definitions_in_if_or_while_non_block => pair
            .into_inner()
            .skip(1)
            .map(|pair| BlockItem::Def(Box::new(parse_definition(expr_parser, pair))))
            .collect(),
        _ => unreachable!(),
    }
}

fn parse_if(expr_parser: &PrattParser<Rule>, pair: Pair<Rule>) -> Statement {
    let mut iter = pair.into_inner();
    Statement::If {
        condition: parse_expr(expr_parser, iter.next().unwrap()),
        then_block: Box::new(parse_if_while_helper(expr_parser, iter.next().unwrap())),
        else_block: iter
            .next()
            .map(|block| Box::new(parse_if_while_helper(expr_parser, block)))
            .unwrap_or_default(),
    }
}

fn parse_while(expr_parser: &PrattParser<Rule>, pair: Pair<Rule>) -> Statement {
    let mut iter = pair.into_inner();
    Statement::While {
        condition: parse_expr(expr_parser, iter.next().unwrap()),
        block: Box::new(parse_if_while_helper(expr_parser, iter.next().unwrap())),
    }
}

fn parse_statement(expr_parser: &PrattParser<Rule>, iter: Pair<Rule>) -> Statement {
    match iter.as_rule() {
        Rule::expression => Statement::Expr(parse_expr(expr_parser, iter)),
        Rule::return_statement => iter
            .into_inner()
            .skip(1)
            .next()
            .map(|expr| Statement::Return(Some(parse_expr(expr_parser, expr))))
            .unwrap_or(Statement::Return(None)),
        Rule::if_statement => parse_if(expr_parser, iter),
        Rule::while_statement => parse_while(expr_parser, iter),
        Rule::break_keyword => Statement::Break,
        Rule::continue_keyword => Statement::Continue,
        _ => unreachable!(),
    }
}

fn parse_block(expr_parser: &PrattParser<Rule>, pair: Pair<Rule>) -> Block {
    pair.into_inner()
        .filter(|pair| !matches!(pair.as_rule(), Rule::int_keyword | Rule::const_keyword))
        .map(|pair| match pair.as_rule() {
            Rule::block => BlockItem::Block(Box::new(parse_block(expr_parser, pair))),
            Rule::expression
            | Rule::return_statement
            | Rule::if_statement
            | Rule::while_statement
            | Rule::break_keyword
            | Rule::continue_keyword => BlockItem::Statement(Box::new(parse_statement(expr_parser, pair))),
            Rule::variable_definition | Rule::array_definition | Rule::const_variable_definition | Rule::const_array_definition => {
                BlockItem::Def(Box::new(parse_definition(expr_parser, pair)))
            }
            _ => unreachable!(),
        })
        .collect()
}

fn parse_signature(expr_parser: &PrattParser<Rule>, pair: Pair<Rule>) -> (bool, String, Vec<Parameter>) {
    let mut iter = pair.into_inner();
    let return_void = matches!(iter.next().unwrap().as_rule(), Rule::void_keyword);
    let id = iter.next().unwrap().as_str().to_string();
    let parameter_list = iter
        .next()
        .unwrap()
        .into_inner()
        .map(|pair| match pair.as_rule() {
            Rule::variable_parameter_definition => Parameter::Int(pair.into_inner().skip(1).next().unwrap().as_str().to_string()),
            Rule::pointer_parameter_definition => {
                let mut iter = pair.into_inner().skip(1);
                Parameter::PointerTmp(
                    iter.next().unwrap().as_str().to_string(),
                    iter.next()
                        .map(|iter| iter.into_inner().map(|expr| parse_expr(expr_parser, expr)).collect())
                        .unwrap_or_default(),
                )
            }
            _ => unreachable!(),
        })
        .collect();
    (return_void, id, parameter_list)
}

fn parse_function_definition(expr_parser: &PrattParser<Rule>, pair: Pair<Rule>) -> GlobalItem {
    let mut iter = pair.into_inner();
    let (return_void, id, parameter_list) = parse_signature(expr_parser, iter.next().unwrap());
    GlobalItem::FuncDef {
        return_void,
        id,
        parameter_list,
        block: parse_block(expr_parser, iter.next().unwrap()),
    }
}

fn parse_global_item(expr_parser: &PrattParser<Rule>, pair: Pair<Rule>) -> GlobalItem {
    match pair.as_rule() {
        Rule::variable_definition | Rule::array_definition | Rule::const_variable_definition | Rule::const_array_definition => {
            GlobalItem::Def(parse_definition(expr_parser, pair))
        }
        Rule::function_definition => parse_function_definition(expr_parser, pair),
        _ => unreachable!(),
    }
}

pub fn build_ast(code: &str) -> TranslationUnit {
    let expr_parser = new_expr_parser();
    let translation_unit = SysYParser::parse(Rule::translation_unit, code).unwrap();
    translation_unit
        .filter(|pair| !matches!(pair.as_rule(), Rule::EOI | Rule::int_keyword | Rule::const_keyword))
        .map(|p| Box::new(parse_global_item(&expr_parser, p)))
        .collect()
}
