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
            0xFFFF // True
        } else {
            0x0000 // False
        }
    };
}

impl ALU {
    pub fn calc(&self, a: u16, b: u16) -> u16 {
        match self {
            Self::ADD => a.wrapping_add(b),
            Self::SUB => a.wrapping_sub(b),
            Self::AND => a & b,
            Self::OR => a | b,
            Self::XOR => a ^ b,
            Self::EQ => boo!(a == b),
            Self::NEQ => boo!(a != b),
            Self::LT => boo!(a < b),
            Self::LTS => boo!((a as i16) < (b as i16)),
            Self::SR => a >> 1,
            Self::SRS => (a as i16 >> 1) as u16,
            Self::SRR => a >> 1 | a << 15,
            Self::SL => a << 1,
            Self::SLR => a << 1 | a >> 15,
        }
    }
}
