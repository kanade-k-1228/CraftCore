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
    SP,
    RA,
    FP,
    A0,
    A1,
    T0,
    T1,
    T2,
    T3,
    S0,
    S1,
    S2,
    S3,
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
    assert_eq!(Some(Reg::SP), Reg::parse("sp"));
    assert_eq!(Some(Reg::RA), Reg::parse("ra"));
    assert_eq!(Some(Reg::FP), Reg::parse("fp"));
    assert_eq!(Some(Reg::A0), Reg::parse("a0"));
    assert_eq!(Some(Reg::A1), Reg::parse("a1"));
    assert_eq!(Some(Reg::T0), Reg::parse("t0"));
    assert_eq!(Some(Reg::T1), Reg::parse("t1"));
    assert_eq!(Some(Reg::T2), Reg::parse("t2"));
    assert_eq!(Some(Reg::T3), Reg::parse("t3"));
    assert_eq!(Some(Reg::S0), Reg::parse("s0"));
    assert_eq!(Some(Reg::S1), Reg::parse("s1"));
    assert_eq!(Some(Reg::S2), Reg::parse("s2"));
    assert_eq!(Some(Reg::S3), Reg::parse("s3"));
    assert_eq!(None, Reg::parse("hoge"));
}
