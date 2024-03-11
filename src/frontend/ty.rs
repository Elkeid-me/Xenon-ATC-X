#[derive(Debug, Clone, Copy)]
pub enum RefType<'a> {
    Int,
    Void,
    IntPointer(&'a [usize]),
    IntArray(&'a [usize]),
    Function(&'a Type, &'a [Type]),
}

#[derive(Debug)]
pub enum Type {
    Int,
    Void,
    IntPointer(Vec<usize>),
    IntArray(Vec<usize>),
    Function(Box<Type>, Vec<Type>),
}

impl<'a> Type {
    pub fn to_ref_type(&'a self) -> RefType<'a> {
        match self {
            Type::Int => RefType::Int,
            Type::Void => RefType::Void,
            Type::IntPointer(l) => RefType::IntPointer(l),
            Type::IntArray(l) => RefType::IntArray(l),
            Type::Function(ret, para) => RefType::Function(ret, para),
        }
    }
}
