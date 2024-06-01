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
use crate::frontend::{ast::*, ty::Type};
use crate::risk;
use std::fmt::Write;

impl Generator {
    pub fn fun_decl(&self, id: String, ret_type: Type, para_type: Vec<Type>) -> String {
        let ret_type_str = match ret_type {
            Type::Int => ": i32",
            Type::Void => "",
            _ => unreachable!(),
        };
        let para_list_str = para_type.iter().map(|ty| ty.to_koopa_type_str().to_string()).collect::<Vec<_>>().join(", ");
        format!("decl @{id}({para_list_str}){ret_type_str}\n")
    }
    pub fn fun_def(
        &mut self,
        id: String,
        ret_type: Type,
        para_type: Vec<Type>,
        para_id: Vec<Option<String>>,
        block: Block,
    ) -> String {
        let ret_type_str = match ret_type {
            Type::Int => ": i32",
            Type::Void => "",
            _ => unreachable!(),
        };
        let para_list_str = para_id
            .iter()
            .zip(para_type.iter())
            .map(|(id, ty)| if let Some(id) = id { format!("@{id}: {}", ty.to_koopa_type_str()) } else { ty.to_koopa_type_str() })
            .collect::<Vec<_>>()
            .join(", ");
        let entry_id = self.counter.get();
        let para_alloc: String = para_id
            .into_iter()
            .zip(para_type)
            .map(|(id, ty)| {
                if let Some(id) = id {
                    format!("    %{} = alloc {}\n    store @{}, %{}\n", id, ty.to_koopa_type_str(), id, id)
                } else {
                    String::new()
                }
            })
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
    pub fn def(&mut self, def: Definition) -> String {
        match self.search(def) {
            (Type::Int, id, None) => format!("    %{id} = alloc i32\n"),
            (Type::Int, _, Some(Init::Const(_))) => String::new(),
            (Type::Int, id, Some(Init::Expr(expr))) => {
                let (expr_eval, expr_id) = self.expr_rvalue(expr);
                format!("{expr_eval}    %{id} = alloc i32\n    store {expr_id}, %{id}\n")
            }
            (Type::IntArray(len), id, Some(Init::ConstList(list))) => {
                let init_str = Self::const_init_list_to_str(&len, list);
                let ty_str = Type::IntArray(len).to_koopa_type_str();
                self.global_const_init.push(format!("global %{id} = alloc {ty_str}, {init_str}\n"));
                String::new()
            }
            (Type::IntArray(len), id, Some(Init::List(list))) => self.local_array(Type::IntArray(len), id, list),
            (Type::IntArray(len), id, None) => {
                let ty_str = Type::IntArray(len).to_koopa_type_str();
                format!("    %{} = alloc {}\n", id, ty_str)
            }
            _ => unreachable!(),
        }
    }
    pub fn init_list_to_str(len: &[usize], list: InitList) -> String {
        let content = list
            .into_iter()
            .map(|item| match item {
                InitListItem::InitList(l) => Self::init_list_to_str(&len[1..], *l),
                InitListItem::Expr(e) => e.get_num().to_string(),
            })
            .collect::<Vec<_>>()
            .join(", ");
        format!("{{{content}}}")
    }
    pub fn const_init_list_to_str(len: &[usize], list: ConstInitList) -> String {
        let content = list
            .into_iter()
            .map(|item| match item {
                ConstInitListItem::ConstInitList(l) => Self::const_init_list_to_str(&len[1..], *l),
                ConstInitListItem::Num(i) => i.to_string(),
            })
            .collect::<Vec<_>>()
            .join(", ");
        format!("{{{content}}}")
    }
    pub fn global_def(&mut self, def: Definition) -> String {
        match self.search(def) {
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
            (Type::IntArray(len), id, Some(Init::List(list))) => {
                let init_str = Self::init_list_to_str(&len, list);
                let ty_str = Type::IntArray(len).to_koopa_type_str();
                format!("global %{id} = alloc {ty_str}, {init_str}\n")
            }
            (Type::IntArray(len), id, Some(Init::ConstList(list))) => {
                let init_str = Self::const_init_list_to_str(&len, list);
                let ty_str = Type::IntArray(len).to_koopa_type_str();
                format!("global %{id} = alloc {ty_str}, {init_str}\n")
            }
            _ => String::new(),
        }
    }
    pub fn local_array_impl(&mut self, len: &[usize], id: &str, list: Vec<InitListItem>) -> String {
        match len.len() {
            1 => list.into_iter().enumerate().fold(String::new(), |mut output, (i, item)| {
                let (expr_eval, expr_id) = self.expr_rvalue(risk!(item, InitListItem::Expr(expr) => expr));
                let tmp_id = self.counter.get();
                let _ = write!(output, "{expr_eval}    {tmp_id} = getelemptr {id}, {i}\n    store {expr_id}, {tmp_id}\n");
                output
            }),
            _ => list.into_iter().enumerate().fold(String::new(), |mut output, (i, item)| {
                let tmp_id = self.counter.get();
                let str = self.local_array_impl(&len[1..], &tmp_id, risk!(item, InitListItem::InitList(list) => *list));
                let _ = write!(output, "    {tmp_id} = getelemptr {id}, {i}\n{str}");
                output
            }),
        }
    }
    pub fn local_array(&mut self, ty: Type, id: String, list: Vec<InitListItem>) -> String {
        let local_id = format!("%{id}");
        let alloc = format!("    {} = alloc {}\n", &local_id, ty.to_koopa_type_str());
        let len = risk!(ty, Type::IntArray(len) => len);
        format!("{}{}", alloc, self.local_array_impl(&len, &local_id, list))
    }
}
