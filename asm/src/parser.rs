use arch::{inst::Inst, reg::Reg};
use color_print::cformat;
use std::num::ParseIntError;

use crate::{error, error::Error, label::Labels};

// ----------------------------------------------------------------------------
// Statement

#[derive(Debug, Clone)]
pub enum Stmt {
    Asm(Asm, Option<u16>),
    CodeLabel(String),
    StaticLabel(String, u16),
    ConstLabel(String, u16),
}

impl Stmt {
    pub fn parse(code: &str) -> (Option<Stmt>, Vec<Error>) {
        let words: Vec<&str> = code.split_whitespace().collect();

        if words.len() == 0 {
            return (None, vec![]);
        }

        // Check for label patterns
        if let Some(key) = words.get(0) {
            if let Some(head) = key.chars().nth(0) {
                // @0x0123 hoge
                if head == '@' {
                    if let Some(label) = words.get(1) {
                        if let Some(value) = key.get(1..) {
                            let key = label.to_string();
                            let val = parse_with_prefix(value).unwrap();
                            return (Some(Stmt::StaticLabel(key, val)), vec![]);
                        }
                    }
                }
                // #0x0123 const
                if head == '#' {
                    if let Some(label) = words.get(1) {
                        if let Some(value) = key.get(1..) {
                            let key = label.to_string();
                            let val = parse_with_prefix(value).unwrap();
                            return (Some(Stmt::ConstLabel(key, val)), vec![]);
                        }
                    }
                }
            }

            if let Some(tail) = key.chars().last() {
                // main:
                if tail == ':' {
                    let label = key.to_string();
                    if let Some(label) = label.get(0..label.len() - 1) {
                        let key = label.to_string();
                        return (Some(Stmt::CodeLabel(key)), vec![]);
                    }
                }
            }
        }

        // Operation
        match Asm::parse(&code) {
            Ok(op) => (Some(Stmt::Asm(op, None)), vec![]),
            Err(err) => (None, vec![err]),
        }
    }
}

// ----------------------------------------------------------------------------
// Operation

#[derive(Debug, Clone)]
pub enum Asm {
    ADD(Reg, Reg, Reg),
    SUB(Reg, Reg, Reg),
    AND(Reg, Reg, Reg),
    OR(Reg, Reg, Reg),
    XOR(Reg, Reg, Reg),
    EQ(Reg, Reg, Reg),
    NEQ(Reg, Reg, Reg),
    LT(Reg, Reg, Reg),
    LTS(Reg, Reg, Reg),
    SR(Reg, Reg),
    SRS(Reg, Reg),
    SRR(Reg, Reg),
    SL(Reg, Reg),
    SLR(Reg, Reg),
    NOP(),
    MOV(Reg, Reg),
    ADDI(Reg, Reg, Imm),
    SUBI(Reg, Reg, Imm),
    ANDI(Reg, Reg, Imm),
    ORI(Reg, Reg, Imm),
    XORI(Reg, Reg, Imm),
    EQI(Reg, Reg, Imm),
    NEQI(Reg, Reg, Imm),
    LTI(Reg, Reg, Imm),
    LTSI(Reg, Reg, Imm),
    NOT(Reg, Reg),
    LOADI(Reg, Imm),
    LOAD(Reg, Reg, Imm),
    STORE(Reg, Reg, Imm),
    IF(Reg, Imm),
    IFR(Reg, Imm),
    JUMP(Imm),
    JUMPR(Imm),
    CALL(Imm),
    RET(),
    IRET(),
}

impl Asm {
    fn parse(code: &str) -> Result<Asm, Error> {
        match code.split_whitespace().collect::<Vec<_>>().split_first() {
            Some((op, args)) => {
                // Get argument by index and parse as Type
                // Example: arg!(0, Reg) -> Reg
                // Meaning: Get 0th argument and parse as Reg
                macro_rules! arg {
                    ($index:expr, $Type:ident) => {{
                        let arg = args.get($index).ok_or(error::Error::MissingArgument)?;
                        let arg = $Type::parse(arg).ok_or(error::Error::ParseArgument(
                            arg.to_string(),
                            stringify!($Type).to_string(),
                        ))?;
                        arg
                    }};
                }

                let op: &str = op.to_owned();
                match op {
                    "nop" => Ok(Asm::NOP()),
                    "add" => Ok(Asm::ADD(arg!(0, Reg), arg!(1, Reg), arg!(2, Reg))),
                    "sub" => Ok(Asm::SUB(arg!(0, Reg), arg!(1, Reg), arg!(2, Reg))),
                    "and" => Ok(Asm::AND(arg!(0, Reg), arg!(1, Reg), arg!(2, Reg))),
                    "or" => Ok(Asm::OR(arg!(0, Reg), arg!(1, Reg), arg!(2, Reg))),
                    "xor" => Ok(Asm::XOR(arg!(0, Reg), arg!(1, Reg), arg!(2, Reg))),
                    "eq" => Ok(Asm::EQ(arg!(0, Reg), arg!(1, Reg), arg!(2, Reg))),
                    "neq" => Ok(Asm::NEQ(arg!(0, Reg), arg!(1, Reg), arg!(2, Reg))),
                    "lt" => Ok(Asm::LT(arg!(0, Reg), arg!(1, Reg), arg!(2, Reg))),
                    "lts" => Ok(Asm::LTS(arg!(0, Reg), arg!(1, Reg), arg!(2, Reg))),
                    "sr" => Ok(Asm::SR(arg!(0, Reg), arg!(1, Reg))),
                    "srs" => Ok(Asm::SRS(arg!(0, Reg), arg!(1, Reg))),
                    "srr" => Ok(Asm::SRR(arg!(0, Reg), arg!(1, Reg))),
                    "sl" => Ok(Asm::SL(arg!(0, Reg), arg!(1, Reg))),
                    "slr" => Ok(Asm::SLR(arg!(0, Reg), arg!(1, Reg))),
                    "mov" => Ok(Asm::MOV(arg!(0, Reg), arg!(1, Reg))),
                    "addi" => Ok(Asm::ADDI(arg!(0, Reg), arg!(1, Reg), arg!(2, Imm))),
                    "subi" => Ok(Asm::SUBI(arg!(0, Reg), arg!(1, Reg), arg!(2, Imm))),
                    "andi" => Ok(Asm::ANDI(arg!(0, Reg), arg!(1, Reg), arg!(2, Imm))),
                    "ori" => Ok(Asm::ORI(arg!(0, Reg), arg!(1, Reg), arg!(2, Imm))),
                    "xori" => Ok(Asm::XORI(arg!(0, Reg), arg!(1, Reg), arg!(2, Imm))),
                    "eqi" => Ok(Asm::EQI(arg!(0, Reg), arg!(1, Reg), arg!(2, Imm))),
                    "neqi" => Ok(Asm::NEQI(arg!(0, Reg), arg!(1, Reg), arg!(2, Imm))),
                    "lti" => Ok(Asm::LTI(arg!(0, Reg), arg!(1, Reg), arg!(2, Imm))),
                    "ltsi" => Ok(Asm::LTSI(arg!(0, Reg), arg!(1, Reg), arg!(2, Imm))),
                    "not" => Ok(Asm::NOT(arg!(0, Reg), arg!(1, Reg))),
                    "loadi" => Ok(Asm::LOADI(arg!(0, Reg), arg!(1, Imm))),
                    "load" => Ok(Asm::LOAD(arg!(0, Reg), arg!(1, Reg), arg!(2, Imm))),
                    "store" => Ok(Asm::STORE(arg!(0, Reg), arg!(1, Reg), arg!(2, Imm))),
                    "if" => Ok(Asm::IF(arg!(0, Reg), arg!(1, Imm))),
                    "ifr" => Ok(Asm::IFR(arg!(0, Reg), arg!(1, Imm))),
                    "jump" => Ok(Asm::JUMP(arg!(0, Imm))),
                    "jumpr" => Ok(Asm::JUMPR(arg!(0, Imm))),
                    "call" => Ok(Asm::CALL(arg!(0, Imm))),
                    "ret" => Ok(Asm::RET()),
                    "iret" => Ok(Asm::IRET()),
                    _ => Err(Error::UnknownOperation(op.to_string())),
                }
            }
            None => Err(Error::SyntaxError),
        }
    }
}

impl Asm {
    pub fn cformat(&self, labels: &Labels) -> String {
        macro_rules! opfmt {
            ($name:expr, $rd:expr, $rs1:expr, $rs2:expr) => {
                cformat!(
                    "<red>{:<6}</><blue>{:<2} {:<2} {:<6}</>",
                    $name,
                    $rd,
                    $rs1,
                    $rs2
                )
            };
        }
        match self {
            Asm::ADD(rd, rs1, rs2) => opfmt!("add", rd, rs1, rs2),
            Asm::SUB(rd, rs1, rs2) => opfmt!("sub", rd, rs1, rs2),
            Asm::AND(rd, rs1, rs2) => opfmt!("and", rd, rs1, rs2),
            Asm::OR(rd, rs1, rs2) => opfmt!("or", rd, rs1, rs2),
            Asm::XOR(rd, rs1, rs2) => opfmt!("xor", rd, rs1, rs2),
            Asm::EQ(rd, rs1, rs2) => opfmt!("eq", rd, rs1, rs2),
            Asm::NEQ(rd, rs1, rs2) => opfmt!("neq", rd, rs1, rs2),
            Asm::LT(rd, rs1, rs2) => opfmt!("lt", rd, rs1, rs2),
            Asm::LTS(rd, rs1, rs2) => opfmt!("lts", rd, rs1, rs2),
            Asm::SR(rd, rs1) => opfmt!("sr", rd, rs1, ""),
            Asm::SRS(rd, rs1) => opfmt!("srs", rd, rs1, ""),
            Asm::SRR(rd, rs1) => opfmt!("srr", rd, rs1, ""),
            Asm::SL(rd, rs1) => opfmt!("sl", rd, rs1, ""),
            Asm::SLR(rd, rs1) => opfmt!("slr", rd, rs1, ""),
            Asm::NOP() => opfmt!("nop", "", "", ""),
            Asm::MOV(rd, rs1) => opfmt!("mov", rd, rs1, ""),
            Asm::ADDI(rd, rs1, imm) => opfmt!("addi", rd, rs1, imm.cformat(labels)),
            Asm::SUBI(rd, rs1, imm) => opfmt!("subi", rd, rs1, imm.cformat(labels)),
            Asm::ANDI(rd, rs1, imm) => opfmt!("andi", rd, rs1, imm.cformat(labels)),
            Asm::ORI(rd, rs1, imm) => opfmt!("ori", rd, rs1, imm.cformat(labels)),
            Asm::XORI(rd, rs1, imm) => opfmt!("xori", rd, rs1, imm.cformat(labels)),
            Asm::EQI(rd, rs1, imm) => opfmt!("eqi", rd, rs1, imm.cformat(labels)),
            Asm::NEQI(rd, rs1, imm) => opfmt!("neqi", rd, rs1, imm.cformat(labels)),
            Asm::LTI(rd, rs1, imm) => opfmt!("lti", rd, rs1, imm.cformat(labels)),
            Asm::LTSI(rd, rs1, imm) => opfmt!("ltsi", rd, rs1, imm.cformat(labels)),
            Asm::NOT(rd, rs1) => opfmt!("not", rd, rs1, ""),
            Asm::LOADI(rd, imm) => opfmt!("loadi", rd, "", imm.cformat(labels)),
            Asm::LOAD(rd, rs1, imm) => opfmt!("load", rd, rs1, imm.cformat(labels)),
            Asm::STORE(rs2, rs1, imm) => opfmt!("store", rs2, rs1, imm.cformat(labels)),
            Asm::IF(rs2, imm) => opfmt!("if", rs2, "", imm.cformat(labels)),
            Asm::IFR(rs2, imm) => opfmt!("ifr", rs2, "", imm.cformat(labels)),
            Asm::JUMP(imm) => opfmt!("jump", "", "", imm.cformat(labels)),
            Asm::JUMPR(imm) => opfmt!("jumpr", "", "", imm.cformat(labels)),
            Asm::CALL(imm) => opfmt!("call", "", "", imm.cformat(labels)),
            Asm::RET() => opfmt!("ret", "", "", ""),
            Asm::IRET() => opfmt!("iret", "", "", ""),
        }
    }
}

impl Asm {
    pub fn resolve(&self, labels: &Labels) -> Result<Inst, error::Error> {
        match self {
            Asm::ADD(rd, rs1, rs2) => Ok(Inst::ADD(*rd, *rs1, *rs2)),
            Asm::SUB(rd, rs1, rs2) => Ok(Inst::SUB(*rd, *rs1, *rs2)),
            Asm::AND(rd, rs1, rs2) => Ok(Inst::AND(*rd, *rs1, *rs2)),
            Asm::OR(rd, rs1, rs2) => Ok(Inst::OR(*rd, *rs1, *rs2)),
            Asm::XOR(rd, rs1, rs2) => Ok(Inst::XOR(*rd, *rs1, *rs2)),
            Asm::EQ(rd, rs1, rs2) => Ok(Inst::EQ(*rd, *rs1, *rs2)),
            Asm::NEQ(rd, rs1, rs2) => Ok(Inst::NEQ(*rd, *rs1, *rs2)),
            Asm::LT(rd, rs1, rs2) => Ok(Inst::LT(*rd, *rs1, *rs2)),
            Asm::LTS(rd, rs1, rs2) => Ok(Inst::LTS(*rd, *rs1, *rs2)),
            Asm::SR(rd, rs1) => Ok(Inst::SR(*rd, *rs1)),
            Asm::SRS(rd, rs1) => Ok(Inst::SRS(*rd, *rs1)),
            Asm::SRR(rd, rs1) => Ok(Inst::SRR(*rd, *rs1)),
            Asm::SL(rd, rs1) => Ok(Inst::SL(*rd, *rs1)),
            Asm::SLR(rd, rs1) => Ok(Inst::SLR(*rd, *rs1)),
            Asm::MOV(rd, rs1) => Ok(Inst::MOV(*rd, *rs1)),
            Asm::ADDI(rd, rs1, imm) => Ok(Inst::ADDI(*rd, *rs1, imm.resolve(&labels)?)),
            Asm::SUBI(rd, rs1, imm) => Ok(Inst::SUBI(*rd, *rs1, imm.resolve(&labels)?)),
            Asm::ANDI(rd, rs1, imm) => Ok(Inst::ANDI(*rd, *rs1, imm.resolve(&labels)?)),
            Asm::ORI(rd, rs1, imm) => Ok(Inst::ORI(*rd, *rs1, imm.resolve(&labels)?)),
            Asm::XORI(rd, rs1, imm) => Ok(Inst::XORI(*rd, *rs1, imm.resolve(&labels)?)),
            Asm::EQI(rd, rs1, imm) => Ok(Inst::EQI(*rd, *rs1, imm.resolve(&labels)?)),
            Asm::NEQI(rd, rs1, imm) => Ok(Inst::NEQI(*rd, *rs1, imm.resolve(&labels)?)),
            Asm::LTI(rd, rs1, imm) => Ok(Inst::LTI(*rd, *rs1, imm.resolve(&labels)?)),
            Asm::LTSI(rd, rs1, imm) => Ok(Inst::LTSI(*rd, *rs1, imm.resolve(&labels)?)),
            Asm::NOT(rd, rs1) => Ok(Inst::NOT(*rd, *rs1)),
            Asm::LOADI(rd, imm) => Ok(Inst::LOADI(*rd, imm.resolve(&labels)?)),
            Asm::LOAD(rd, rs1, imm) => Ok(Inst::LOAD(*rd, *rs1, imm.resolve(&labels)?)),
            Asm::STORE(rs2, rs1, imm) => Ok(Inst::STORE(*rs2, *rs1, imm.resolve(&labels)?)),
            Asm::NOP() => Ok(Inst::NOP()),
            Asm::IF(rs2, imm) => Ok(Inst::IF(*rs2, imm.resolve(&labels)?)),
            Asm::IFR(rs2, imm) => Ok(Inst::IFR(*rs2, imm.resolve(&labels)?)),
            Asm::JUMP(imm) => Ok(Inst::JUMP(imm.resolve(&labels)?)),
            Asm::JUMPR(imm) => Ok(Inst::JUMPR(imm.resolve(&labels)?)),
            Asm::CALL(imm) => Ok(Inst::CALL(imm.resolve(&labels)?)),
            Asm::RET() => Ok(Inst::RET()),
            Asm::IRET() => Ok(Inst::IRET()),
        }
    }
}

// ----------------------------------------------------------------------------
// Immidiate

#[derive(Debug, Clone)]
pub enum Imm {
    Literal(u16),
    Label(String),
}

impl Imm {
    fn parse(s: &str) -> Option<Imm> {
        match parse_with_prefix(s) {
            Ok(value) => Some(Imm::Literal(value)),
            Err(_) => Some(Imm::Label(s.to_string())),
        }
    }
    fn cformat(&self, labels: &Labels) -> String {
        match self {
            Imm::Label(str) => {
                // Get label information directly from Labels
                if let Some(((_, _), label_type, pc_val)) = labels.get(str) {
                    match label_type {
                        crate::label::LabelType::Code => {
                            if let Some(val) = pc_val {
                                cformat!("<g>0x{:0>4X}({})</>", val, str)
                            } else {
                                cformat!("<g>({})</>", str)
                            }
                        }
                        crate::label::LabelType::Static(val) => {
                            cformat!("<c>0x{:0>4X}({})</>", val, str)
                        }
                        crate::label::LabelType::Const(val) => {
                            cformat!("<y>0x{:0>4X}({})</>", val, str)
                        }
                    }
                } else {
                    cformat!("<r,u>{}</>", str)
                }
            }
            Imm::Literal(val) => cformat!("<y>0x{:0>4X}</>", val),
        }
    }
}

fn parse_with_prefix(s: &str) -> Result<u16, ParseIntError> {
    if s.len() < 2 {
        u16::from_str_radix(s, 10)
    } else {
        let (prefix, num) = s.split_at(2);
        let radix = match prefix {
            "0b" => 2,
            "0o" => 8,
            "0x" => 16,
            _ => 10,
        };
        u16::from_str_radix(num, radix)
    }
}

impl Imm {
    pub fn resolve(&self, labels: &Labels) -> Result<u16, error::Error> {
        match self {
            Imm::Literal(val) => Ok(*val),
            Imm::Label(label) => {
                if let Some(val) = labels.get_val(label) {
                    Ok(val)
                } else {
                    Err(error::Error::UndefinedLabel(label.clone()))
                }
            }
        }
    }
}
