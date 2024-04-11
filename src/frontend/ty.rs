#[derive(Debug, Clone, Copy)]
pub enum RefType<'a> {
    Int,
    Void,
    IntPointer(&'a [usize]),
    IntArray(&'a [usize]),
}

#[derive(Debug, Clone)]
pub enum Type {
    Int,
    Void,
    IntPointer(Vec<usize>),
    IntArray(Vec<usize>),
    Function(Box<Type>, Vec<Type>),
}

impl Default for Type {
    fn default() -> Self {
        Self::Void
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::IntPointer(l0), Self::IntPointer(r0)) => l0 == r0,
            (Self::IntArray(l0), Self::IntArray(r0)) => l0 == r0,
            (Self::Function(l0, l1), Self::Function(r0, r1)) => l0 == r0 && l1 == r1,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
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
