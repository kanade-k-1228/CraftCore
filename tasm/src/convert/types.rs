use crate::link::structs::AsmLine;

/// Unified code representation for both functions and assembly blocks
#[derive(Debug, Clone)]
pub struct Code {
    /// Name of the code block (function or asm block name)
    pub name: String,
    /// Instruction sequence with unresolved symbols
    pub instructions: Vec<AsmLine>,
    /// Whether this is a function (true) or asm block (false)
    pub is_function: bool,
}

impl Code {
    pub fn new_function(name: String, instructions: Vec<AsmLine>) -> Self {
        Code {
            name,
            instructions,
            is_function: true,
        }
    }

    pub fn new_asm_block(name: String, instructions: Vec<AsmLine>) -> Self {
        Code {
            name,
            instructions,
            is_function: false,
        }
    }
}
