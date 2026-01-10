pub mod asm;
pub mod func;

use arch::inst::Inst;
use arch::reg::Reg;
use std::fmt;

#[derive(Debug, Clone)]
pub struct Code(pub Vec<Inst<Reg, Imm>>);

#[derive(Debug, Clone)]
pub enum Imm {
    Literal(u16),
    Symbol(String, usize),
}

impl fmt::Display for Imm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Imm::Literal(val) => write!(f, "{}", val),
            Imm::Symbol(name, offset) => {
                if *offset == 0 {
                    write!(f, "{}", name)
                } else {
                    write!(f, "{}+{}", name, offset)
                }
            }
        }
    }
}

impl fmt::LowerHex for Imm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Imm::Literal(val) => write!(f, "{:04x}", val),
            Imm::Symbol(name, offset) => {
                if *offset == 0 {
                    write!(f, "{}", name)
                } else {
                    write!(f, "{}+0x{:x}", name, offset)
                }
            }
        }
    }
}

impl fmt::UpperHex for Imm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Imm::Literal(val) => write!(f, "{:04X}", val),
            Imm::Symbol(name, offset) => {
                if *offset == 0 {
                    write!(f, "{}", name)
                } else {
                    write!(f, "{}+0x{:X}", name, offset)
                }
            }
        }
    }
}
