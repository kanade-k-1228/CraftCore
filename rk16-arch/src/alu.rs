use num_enum::{FromPrimitive, IntoPrimitive};
use serde::{Deserialize, Serialize};

#[derive(
    Debug, Clone, Copy, PartialEq, Serialize, Deserialize, Default, FromPrimitive, IntoPrimitive,
)]
#[repr(u8)]
pub enum Alu {
    #[default]
    Add,
    Sub,
    And,
    Or,
    Xor,
    Eq,
    Neq,
    Lt,
    Lts,
    Sr,
    Srs,
    Srr,
    Sl,
    Slr,
}

macro_rules! bol {
    ($cond:expr) => {
        if $cond {
            0xFFFF
        } else {
            0x0000
        }
    };
}

pub fn valu<T: Into<Alu>>(op: T, a: u16, b: u16) -> u16 {
    use Alu::*;
    match op.into() {
        Add => a + b,
        Sub => a - b,
        And => a & b,
        Or => a | b,
        Xor => a ^ b,
        Eq => bol!(a == b),
        Neq => bol!(a != b),
        Lt => bol!(a < b),
        Lts => bol!((a as i16) < (b as i16)),
        Sr => a >> 1,
        Srs => (a as i16 >> 1) as u16,
        Srr => a >> 1 | a << 15,
        Sl => a << 1,
        Slr => a << 1 | a >> 15,
    }
}
