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
    #[default]
    Nop,
    Mov,
    Addi,
    Subi,
    Andi,
    Ori,
    Xori,
    Eqi,
    Neqi,
    Lti,
    Ltsi,
    Not,
    Loadi,
    Load,
    Store,
    If,
    Ifr,
    Jump,
    Jumpr,
    Call,
    Ret,
    Iret,
}

impl OpKind {
    pub fn parse(s: &str) -> Result<Self, String> {
        match s.to_ascii_uppercase().parse::<Self>() {
            Ok(a) => Ok(a),
            Err(_) => Err(format!("Undefined Op: {s}")),
        }
    }
}

pub enum Arg {
    Rd,
    Rs1,
    Rs2,
    Imm,
}

impl OpKind {
    pub fn arg_field(&self) -> Vec<Arg> {
        use OpKind::*;
        match self {
            Add => vec![Arg::Rd, Arg::Rs1, Arg::Rs2],
            Sub => vec![Arg::Rd, Arg::Rs1, Arg::Rs2],
            And => vec![Arg::Rd, Arg::Rs1, Arg::Rs2],
            Or => vec![Arg::Rd, Arg::Rs1, Arg::Rs2],
            Xor => vec![Arg::Rd, Arg::Rs1, Arg::Rs2],
            Eq => vec![Arg::Rd, Arg::Rs1, Arg::Rs2],
            Neq => vec![Arg::Rd, Arg::Rs1, Arg::Rs2],
            Lt => vec![Arg::Rd, Arg::Rs1, Arg::Rs2],
            Lts => vec![Arg::Rd, Arg::Rs1, Arg::Rs2],
            Sr => vec![Arg::Rd, Arg::Rs1],
            Srs => vec![Arg::Rd, Arg::Rs1],
            Srr => vec![Arg::Rd, Arg::Rs1],
            Sl => vec![Arg::Rd, Arg::Rs1],
            Slr => vec![Arg::Rd, Arg::Rs1],
            Nop => vec![],
            Mov => vec![Arg::Rd, Arg::Rs1],
            Addi => vec![Arg::Rd, Arg::Rs1, Arg::Imm],
            Subi => vec![Arg::Rd, Arg::Rs1, Arg::Imm],
            Andi => vec![Arg::Rd, Arg::Rs1, Arg::Imm],
            Ori => vec![Arg::Rd, Arg::Rs1, Arg::Imm],
            Xori => vec![Arg::Rd, Arg::Rs1, Arg::Imm],
            Eqi => vec![Arg::Rd, Arg::Rs1, Arg::Imm],
            Neqi => vec![Arg::Rd, Arg::Rs1, Arg::Imm],
            Lti => vec![Arg::Rd, Arg::Rs1, Arg::Imm],
            Ltsi => vec![Arg::Rd, Arg::Rs1, Arg::Imm],
            Not => vec![Arg::Rd, Arg::Rs1],
            Loadi => vec![Arg::Rd, Arg::Imm],
            Load => vec![Arg::Rd, Arg::Rs1, Arg::Imm],
            Store => vec![Arg::Rs2, Arg::Rs1, Arg::Imm],
            If => vec![Arg::Rs2, Arg::Imm],
            Ifr => vec![Arg::Rs2, Arg::Imm],
            Jump => vec![Arg::Imm],
            Jumpr => vec![Arg::Imm],
            Call => vec![Arg::Imm],
            Ret => vec![],
            Iret => vec![],
        }
    }
}
