use koopa::ir::types::Type as KType;
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

    pub fn to_koopa_type(&self) -> KType {
        match self {
            Type::Int => KType::get_i32(),
            Type::Void => KType::get_unit(),
            Type::IntPointer(len) => {
                KType::get_pointer(len.iter().fold(Type::Int.to_koopa_type(), |ty, &len| KType::get_array(ty, len)))
            }
            Type::IntArray(len) => len.iter().fold(Type::Int.to_koopa_type(), |ty, &len| KType::get_array(ty, len)),
            Type::Function(ret_type, para_types) => {
                KType::get_function(para_types.iter().map(|ty| ty.to_koopa_type()).collect(), ret_type.to_koopa_type())
            }
            Type::Keyword => unreachable!(),
        }
    }

    pub fn to_koopa_type_str(&self) -> String {
        match self {
            Type::Int => "i32".to_string(),
            Type::IntPointer(len) => {
                let base = len.iter().fold("i32".to_string(), |ty, len| format!("[{ty}, {len}]"));
                format!("*{base}")
            }
            Type::IntArray(len) => len.iter().fold("i32".to_string(), |ty, len| format!("[{ty}, {len}]")),
            _ => unreachable!(),
        }
    }
}
