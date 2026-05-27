use num_enum::{FromPrimitive, IntoPrimitive};
use serde::{Deserialize, Serialize};
use strum::{Display, EnumString};

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Serialize,
    Deserialize,
    Default,
    FromPrimitive,
    IntoPrimitive,
    EnumString,
    Display,
    Eq,
)]
#[repr(u8)]
pub enum Reg {
    #[default]
    Z,
    IRA,
    PC,
    CSR,
    RA,
    FP,
    T0,
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
}

impl Reg {
    pub fn parse(s: &str) -> Option<Self> {
        s.to_ascii_uppercase().parse::<Self>().ok()
    }
}

impl Into<u16> for Reg {
    fn into(self) -> u16 {
        self as u16
    }
}

#[test]
fn test() {
    assert_eq!(Some(Reg::Z), Reg::parse("z"));
    assert_eq!(Some(Reg::Z), Reg::parse("Z"));
    assert_eq!(Some(Reg::IRA), Reg::parse("ira"));
    assert_eq!(Some(Reg::PC), Reg::parse("pc"));
    assert_eq!(Some(Reg::CSR), Reg::parse("csr"));
    assert_eq!(None, Reg::parse("sp"));
    assert_eq!(Some(Reg::RA), Reg::parse("ra"));
    assert_eq!(Some(Reg::FP), Reg::parse("fp"));
    assert_eq!(Some(Reg::T0), Reg::parse("t0"));
    assert_eq!(Some(Reg::T1), Reg::parse("t1"));
    assert_eq!(Some(Reg::T2), Reg::parse("t2"));
    assert_eq!(Some(Reg::T3), Reg::parse("t3"));
    assert_eq!(Some(Reg::T4), Reg::parse("t4"));
    assert_eq!(Some(Reg::T5), Reg::parse("t5"));
    assert_eq!(Some(Reg::T6), Reg::parse("t6"));
    assert_eq!(Some(Reg::T7), Reg::parse("t7"));
    assert_eq!(Some(Reg::T8), Reg::parse("t8"));
    assert_eq!(Some(Reg::T9), Reg::parse("t9"));
    assert_eq!(None, Reg::parse("a0"));
    assert_eq!(None, Reg::parse("s0"));
    assert_eq!(None, Reg::parse("hoge"));
}
