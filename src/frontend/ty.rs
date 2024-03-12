#[derive(Debug, Clone, Copy)]
pub enum RefType<'a> {
    Int,
    Void,
    Keyword,
    IntPointer(&'a [usize]),
    IntArray(&'a [usize]),
    Function(&'a Type, &'a [Type]),
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
            Type::Function(ret, para) => RefType::Function(ret, para),
        }
    }
}
