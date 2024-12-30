use num_enum::{FromPrimitive, IntoPrimitive};
use serde::{Deserialize, Serialize};

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default, FromPrimitive, IntoPrimitive,
)]
#[repr(u8)]
pub enum ALU {
    #[default]
    ADD,
    SUB,
    AND,
    OR,
    XOR,
    EQ,
    NEQ,
    LT,
    LTS,
    SR,
    SRS,
    SRR,
    SL,
    SLR,
}

macro_rules! boo {
    ($cond:expr) => {
        if $cond {
            0xFFFF
        } else {
            0x0000
        }
    };
}

pub fn valu<T: Into<ALU>>(op: T, a: u16, b: u16) -> u16 {
    use ALU::*;
    match op.into() {
        ADD => a + b,
        SUB => a - b,
        AND => a & b,
        OR => a | b,
        XOR => a ^ b,
        EQ => boo!(a == b),
        NEQ => boo!(a != b),
        LT => boo!(a < b),
        LTS => boo!((a as i16) < (b as i16)),
        SR => a >> 1,
        SRS => (a as i16 >> 1) as u16,
        SRR => a >> 1 | a << 15,
        SL => a << 1,
        SLR => a << 1 | a >> 15,
    }
}
