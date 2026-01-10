use arch::inst::Inst;
use arch::reg::Reg;

#[derive(Debug, Clone)]
pub struct Code(pub Vec<Inst<Reg, Imm>>);

/// Unresolved immidiate value
#[derive(Debug, Clone)]
pub enum Imm {
    Lit(usize),            // Literal value (value)
    Label(String),         // Address of code (name)
    Symbol(String, usize), // Address of data (name, offset)
}

impl std::fmt::Display for Imm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Imm::Lit(val) => write!(f, "{}", val),
            Imm::Label(name) => write!(f, "{}", name),
            Imm::Symbol(name, offset) => write!(f, "{} + {}", name, offset),
        }
    }
}

impl std::fmt::LowerHex for Imm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Imm::Lit(val) => write!(f, "0x{:04x}", val),
            Imm::Label(name) => write!(f, "{}", name),
            Imm::Symbol(name, offset) => write!(f, "{} + 0x{:04x}", name, offset),
        }
    }
}

impl std::fmt::UpperHex for Imm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Imm::Lit(val) => write!(f, "0x{:04X}", val),
            Imm::Label(name) => write!(f, "{}", name),
            Imm::Symbol(name, offset) => write!(f, "{} + 0x{:04X}", name, offset),
        }
    }
}
