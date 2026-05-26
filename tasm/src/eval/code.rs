use arch::inst::Inst;
use arch::reg::Reg;

#[derive(Debug, Clone)]
pub struct Code(pub Vec<Inst<Reg, Imm>>);

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
