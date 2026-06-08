use arch::op::Op;
use arch::reg::Reg;

#[derive(Debug, Clone)]
pub struct Code(pub Vec<Op<Reg, Imm>>);

/// Unresolved immidiate value
#[derive(Debug, Clone)]
pub enum Imm {
    Lit(usize),            // Literal value (value)
    Label(String),         // Address of code (name)
    Const(String, usize),  // Named constant (name, value)
    Symbol(String, usize), // Address of data (name, offset)
    ScopeExit(usize),      // Placeholder: jump past labeled scope with given id (break)
    ScopeEntry(usize),     // Placeholder: jump to start of labeled scope with given id (continue)
}

impl Imm {
    /// SP-relative negative offset (= SP - n). Encoded as 16bit two's complement.
    pub fn neg(n: usize) -> Self {
        Imm::Lit(((-(n as i32)) as u16) as usize)
    }
}

impl From<u16> for Imm {
    fn from(v: u16) -> Self {
        Imm::Lit(v as usize)
    }
}
