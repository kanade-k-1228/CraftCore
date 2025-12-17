use std::collections::HashMap;

/// Represents an assembly instruction that may contain unresolved symbols
#[derive(Debug, Clone, PartialEq)]
pub enum AsmInst {
    /// Raw instruction with all operands resolved
    Inst(arch::inst::Inst),
    /// Label definition (creates a symbol at current address)
    Label(String),
    /// Directive to set current address
    Org(u16),
}

/// A line of assembly code with possible symbol references
#[derive(Debug, Clone)]
pub struct AsmLine {
    pub inst: AsmInst,
    /// Symbols referenced in this instruction (for later resolution)
    pub symbols: Vec<String>,
}

/// Items before address allocation
#[derive(Debug, Clone)]
pub enum Item {
    /// Static variable: name, size in words, optional fixed address
    Static(String, usize, Option<usize>),
    /// Constant data: name, size in words, optional fixed address
    Const(String, usize, Option<usize>),
    /// Code section: name, instructions
    Code(String, Vec<AsmLine>),
}

/// Items after address allocation
#[derive(Debug, Clone)]
pub struct Allocated {
    /// Symbol table mapping names to addresses
    pub symbols: HashMap<String, usize>,
    /// Memory sections with allocated addresses
    pub sections: Vec<Section>,
}

#[derive(Debug, Clone)]
pub struct Section {
    pub name: String,
    pub addr: usize,
    pub data: SectionData,
}

#[derive(Debug, Clone)]
pub enum SectionData {
    /// Static/const data (size in words)
    Data(usize),
    /// Code with instructions
    Code(Vec<AsmLine>),
}
