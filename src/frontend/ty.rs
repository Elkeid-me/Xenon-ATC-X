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

#[derive(Debug, Clone, Copy)]
pub enum Type<'a> {
    Int,
    Void,
    Pointer(&'a [usize]),
}

impl Type<'_> {
    pub fn can_convert_to(&self, rhs: &Self) -> bool {
        match (*self, *rhs) {
            (Type::Int, Type::Int) | (Type::Void, Type::Void) => true,
            (Type::Pointer(l_1), Type::Pointer(l_2)) => l_1 == l_2,
            _ => false,
        }
    }
}
