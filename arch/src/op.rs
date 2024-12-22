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
)]
#[repr(u8)]
pub enum OpKind {
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
    #[default]
    NOP,
    MOV,
    ADDI,
    SUBI,
    ANDI,
    ORI,
    XORI,
    EQI,
    NEQI,
    LTI,
    LTSI,
    NOT,
    LOADI,
    LOAD,
    STORE,
    IF,
    IFR,
    JUMP,
    JUMPR,
    CALL,
    RET,
    IRET,
}

impl OpKind {
    pub fn parse(s: &str) -> Result<Self, String> {
        match s.to_uppercase().parse::<Self>() {
            Ok(a) => Ok(a),
            Err(_) => Err(format!("Undefined Op: {s}")),
        }
    }
}

pub enum Arg {
    RD,
    RS1,
    RS2,
    IMM,
}

impl OpKind {
    pub fn arg_field(&self) -> Vec<Arg> {
        use OpKind::*;
        match self {
            ADD => vec![Arg::RD, Arg::RS1, Arg::RS2],
            SUB => vec![Arg::RD, Arg::RS1, Arg::RS2],
            AND => vec![Arg::RD, Arg::RS1, Arg::RS2],
            OR => vec![Arg::RD, Arg::RS1, Arg::RS2],
            XOR => vec![Arg::RD, Arg::RS1, Arg::RS2],
            EQ => vec![Arg::RD, Arg::RS1, Arg::RS2],
            NEQ => vec![Arg::RD, Arg::RS1, Arg::RS2],
            LT => vec![Arg::RD, Arg::RS1, Arg::RS2],
            LTS => vec![Arg::RD, Arg::RS1, Arg::RS2],
            SR => vec![Arg::RD, Arg::RS1],
            SRS => vec![Arg::RD, Arg::RS1],
            SRR => vec![Arg::RD, Arg::RS1],
            SL => vec![Arg::RD, Arg::RS1],
            SLR => vec![Arg::RD, Arg::RS1],
            NOP => vec![],
            MOV => vec![Arg::RD, Arg::RS1],
            ADDI => vec![Arg::RD, Arg::RS1, Arg::IMM],
            SUBI => vec![Arg::RD, Arg::RS1, Arg::IMM],
            ANDI => vec![Arg::RD, Arg::RS1, Arg::IMM],
            ORI => vec![Arg::RD, Arg::RS1, Arg::IMM],
            XORI => vec![Arg::RD, Arg::RS1, Arg::IMM],
            EQI => vec![Arg::RD, Arg::RS1, Arg::IMM],
            NEQI => vec![Arg::RD, Arg::RS1, Arg::IMM],
            LTI => vec![Arg::RD, Arg::RS1, Arg::IMM],
            LTSI => vec![Arg::RD, Arg::RS1, Arg::IMM],
            NOT => vec![Arg::RD, Arg::RS1],
            LOADI => vec![Arg::RD, Arg::IMM],
            LOAD => vec![Arg::RD, Arg::RS1, Arg::IMM],
            STORE => vec![Arg::RS2, Arg::RS1, Arg::IMM],
            IF => vec![Arg::RS2, Arg::IMM],
            IFR => vec![Arg::RS2, Arg::IMM],
            JUMP => vec![Arg::IMM],
            JUMPR => vec![Arg::IMM],
            CALL => vec![Arg::IMM],
            RET => vec![],
            IRET => vec![],
        }
    }
}

#[test]
fn test() {
    println!("{}", OpKind::ADD);
    println!("{:?}", OpKind::parse("add"));
    println!("{:?}", OpKind::parse("iret"));
    println!("{:?}", OpKind::parse("hoge"));
}
