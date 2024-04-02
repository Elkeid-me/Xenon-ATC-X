#[derive(Debug, Clone, Copy)]
pub enum RefType<'a> {
    Int,
    Void,
    Keyword,
    IntPointer(&'a [usize]),
    IntArray(&'a [usize]),
}

#[derive(Debug, Clone)]
pub enum Type {
    Int,
    Void,
    Keyword,
    IntPointer(Vec<usize>),
    IntArray(Vec<usize>),
    Function(Box<Type>, Vec<Type>),
}

impl<'a> Type {
    pub fn to_ref_type(&'a self) -> RefType<'a> {
        match self {
            Type::Int => RefType::Int,
            Type::Void => RefType::Void,
            Type::Keyword => RefType::Keyword,
            Type::IntPointer(l) => RefType::IntPointer(l),
            Type::IntArray(l) => RefType::IntArray(l),
            _ => unreachable!(),
        }
    }

    pub fn to_koopa_type_str(&self) -> String {
        match self {
            Type::Int => "i32".to_string(),
            Type::IntPointer(len) => {
                let base = len.iter().rev().fold("i32".to_string(), |ty, len| format!("[{ty}, {len}]"));
                format!("*{base}")
            }
            Type::IntArray(len) => len.iter().rev().fold("i32".to_string(), |ty, len| format!("[{ty}, {len}]")),
            _ => unreachable!(),
        }
    }
}
