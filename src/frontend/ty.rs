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

use std::fmt::{Display, Formatter, Result};

#[derive(Clone, Copy)]
pub enum RefType<'a> {
    Int,
    Void,
    IntPointer(&'a [usize]),
    IntArray(&'a [usize]),
}

#[derive(Clone, Default, PartialEq)]
pub enum Type {
    Int,
    #[default]
    Void,
    IntPointer(Vec<usize>),
    IntArray(Vec<usize>),
    Function(Box<Type>, Vec<Type>),
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Self::Int => write!(f, "int"),
            Self::Void => write!(f, "void"),
            Self::IntPointer(len) => {
                write!(f, "*int")?;
                for i in len {
                    write!(f, "[{i}]")?;
                }
                Ok(())
            }
            Self::IntArray(len) => {
                write!(f, "int")?;
                for i in len {
                    write!(f, "[{i}]")?;
                }
                Ok(())
            }
            Self::Function(ret_type, para_types) => {
                write!(f, "{ret_type}(")?;
                let mut flag = false;
                for i in para_types {
                    if flag {
                        write!(f, ", ")?;
                    }
                    write!(f, "{i}")?;
                    flag = true;
                }
                write!(f, ")")
            }
        }
    }
}

impl<'a> Type {
    pub fn to_ref_type(&'a self) -> RefType<'a> {
        match self {
            Self::Int => RefType::Int,
            Self::Void => RefType::Void,
            Self::IntPointer(l) => RefType::IntPointer(l),
            Self::IntArray(l) => RefType::IntArray(l),
            _ => unreachable!(),
        }
    }

    pub fn to_koopa_type_str(&self) -> String {
        match self {
            Self::Int => "i32".to_string(),
            Self::IntPointer(len) => {
                let base = len.iter().rev().fold("i32".to_string(), |ty, len| format!("[{ty}, {len}]"));
                format!("*{base}")
            }
            Self::IntArray(len) => len.iter().rev().fold("i32".to_string(), |ty, len| format!("[{ty}, {len}]")),
            _ => unreachable!(),
        }
    }
}
