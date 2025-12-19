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
