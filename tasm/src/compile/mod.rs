pub mod asm;
pub mod func;

use arch::inst::Inst;

#[derive(Debug, Clone)]
pub struct Code(pub Vec<(Inst, Option<Imm>)>);

#[derive(Debug, Clone)]
pub enum Imm {
    Literal(u16),
    Symbol(String, usize),
}
