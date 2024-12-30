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
    ZERO,
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
    pub fn parse(s: &str) -> Result<Self, String> {
        match s.to_ascii_uppercase().parse::<Self>() {
            Ok(a) => Ok(a),
            Err(_) => Err(format!("Unknown reg name: {s}")),
        }
    }
}

impl Into<u16> for Reg {
    fn into(self) -> u16 {
        self as u16
    }
}

#[test]
fn test() {
    println!("{}", Reg::ZERO);
    println!("{:?}", Reg::parse("s2"));
    println!("{:?}", Reg::parse("zero"));
    println!("{:?}", Reg::parse("hoge"));
}
