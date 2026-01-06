#[derive(Debug, Clone, PartialEq)]
pub enum NormType {
    Int,                                          // data         | int
    Void,                                         // void type    | void
    Addr(Box<NormType>),                          // address      | *Type
    Array(usize, Box<NormType>),                  // array        | Type[10]
    Struct(Vec<(String, NormType)>),              // struct       | {a: int, b: Type}
    Func(Vec<(String, NormType)>, Box<NormType>), // function     | (a: int, b: Type) -> Type
}

impl NormType {
    pub fn sizeof(&self) -> usize {
        match self {
            NormType::Int => 1,
            NormType::Void => 0,
            NormType::Addr(_) => 1,
            NormType::Array(len, elem) => len * elem.sizeof(),
            NormType::Struct(fields) => fields.iter().map(|(_, elem)| elem.sizeof()).sum(),
            NormType::Func(_, _) => 0,
        }
    }

    /// Get field offset from a struct type
    pub fn get_field_offset(&self, field: &str) -> Option<usize> {
        if let NormType::Struct(fields) = self {
            let mut offset = 0;
            for (name, ty) in fields {
                if name == field {
                    return Some(offset);
                }
                offset += ty.sizeof();
            }
        }
        None
    }

    /// Get array element offset for a given index
    pub fn get_array_offset(&self, index: usize) -> Option<usize> {
        match self {
            NormType::Array(_, elem) => Some(index * elem.sizeof()),
            _ => None,
        }
    }
}

impl NormType {
    pub fn fmt(&self) -> String {
        match self {
            NormType::Int => "int".to_string(),
            NormType::Void => "void".to_string(),
            NormType::Addr(inner) => format!("*{}", inner.fmt()),
            NormType::Array(len, elem) => format!("{}[{}]", elem.fmt(), len),
            NormType::Struct(fields) => {
                let mut strs: Vec<String> = Vec::new();
                for (name, ty) in fields {
                    strs.push(format!("{}: {}", name, ty.fmt()));
                }
                format!("{{{}}}", strs.join(", "))
            }
            NormType::Func(args, ret) => {
                let mut strs: Vec<String> = Vec::new();
                for (name, ty) in args {
                    strs.push(format!("{}: {}", name, ty.fmt()));
                }
                format!("fn({}) -> {}", strs.join(", "), ret.fmt())
            }
        }
    }
}
