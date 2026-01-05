use arch::{inst::Inst, reg::Reg};
use color_print::cformat;
use std::num::ParseIntError;

use crate::{error, error::Error, ident::Idents};

// ----------------------------------------------------------------------------
// Statement

#[derive(Debug, Clone)]
pub enum Stmt {
    Code(Code, Option<u16>),
    Label(String),
    Static(String, u16),
    Const(String, u16),
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
                            return (Some(Stmt::Static(key, val)), vec![]);
                        }
                    }
                }
                // #0x0123 const
                if head == '#' {
                    if let Some(label) = words.get(1) {
                        if let Some(value) = key.get(1..) {
                            let key = label.to_string();
                            let val = parse_with_prefix(value).unwrap();
                            return (Some(Stmt::Const(key, val)), vec![]);
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
                        return (Some(Stmt::Label(key)), vec![]);
                    }
                }
            }
        }

        // Operation
        match Code::parse(&code) {
            Ok(op) => (Some(Stmt::Code(op, None)), vec![]),
            Err(err) => (None, vec![err]),
        }
    }
}

// ----------------------------------------------------------------------------
// Operation

#[derive(Debug, Clone)]
pub enum Code {
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
    JUMPIF(Reg, Imm),
    JUMPIFR(Reg, Imm),
    JUMP(Imm),
    JUMPR(Imm),
    CALL(Imm),
    RET(),
    IRET(),
}

impl Code {
    fn parse(code: &str) -> Result<Code, Error> {
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
                    "nop" => Ok(Code::NOP()),
                    "add" => Ok(Code::ADD(arg!(0, Reg), arg!(1, Reg), arg!(2, Reg))),
                    "sub" => Ok(Code::SUB(arg!(0, Reg), arg!(1, Reg), arg!(2, Reg))),
                    "and" => Ok(Code::AND(arg!(0, Reg), arg!(1, Reg), arg!(2, Reg))),
                    "or" => Ok(Code::OR(arg!(0, Reg), arg!(1, Reg), arg!(2, Reg))),
                    "xor" => Ok(Code::XOR(arg!(0, Reg), arg!(1, Reg), arg!(2, Reg))),
                    "eq" => Ok(Code::EQ(arg!(0, Reg), arg!(1, Reg), arg!(2, Reg))),
                    "neq" => Ok(Code::NEQ(arg!(0, Reg), arg!(1, Reg), arg!(2, Reg))),
                    "lt" => Ok(Code::LT(arg!(0, Reg), arg!(1, Reg), arg!(2, Reg))),
                    "lts" => Ok(Code::LTS(arg!(0, Reg), arg!(1, Reg), arg!(2, Reg))),
                    "sr" => Ok(Code::SR(arg!(0, Reg), arg!(1, Reg))),
                    "srs" => Ok(Code::SRS(arg!(0, Reg), arg!(1, Reg))),
                    "srr" => Ok(Code::SRR(arg!(0, Reg), arg!(1, Reg))),
                    "sl" => Ok(Code::SL(arg!(0, Reg), arg!(1, Reg))),
                    "slr" => Ok(Code::SLR(arg!(0, Reg), arg!(1, Reg))),
                    "mov" => Ok(Code::MOV(arg!(0, Reg), arg!(1, Reg))),
                    "addi" => Ok(Code::ADDI(arg!(0, Reg), arg!(1, Reg), arg!(2, Imm))),
                    "subi" => Ok(Code::SUBI(arg!(0, Reg), arg!(1, Reg), arg!(2, Imm))),
                    "andi" => Ok(Code::ANDI(arg!(0, Reg), arg!(1, Reg), arg!(2, Imm))),
                    "ori" => Ok(Code::ORI(arg!(0, Reg), arg!(1, Reg), arg!(2, Imm))),
                    "xori" => Ok(Code::XORI(arg!(0, Reg), arg!(1, Reg), arg!(2, Imm))),
                    "eqi" => Ok(Code::EQI(arg!(0, Reg), arg!(1, Reg), arg!(2, Imm))),
                    "neqi" => Ok(Code::NEQI(arg!(0, Reg), arg!(1, Reg), arg!(2, Imm))),
                    "lti" => Ok(Code::LTI(arg!(0, Reg), arg!(1, Reg), arg!(2, Imm))),
                    "ltsi" => Ok(Code::LTSI(arg!(0, Reg), arg!(1, Reg), arg!(2, Imm))),
                    "not" => Ok(Code::NOT(arg!(0, Reg), arg!(1, Reg))),
                    "loadi" => Ok(Code::LOADI(arg!(0, Reg), arg!(1, Imm))),
                    "load" => Ok(Code::LOAD(arg!(0, Reg), arg!(1, Reg), arg!(2, Imm))),
                    "store" => Ok(Code::STORE(arg!(0, Reg), arg!(1, Reg), arg!(2, Imm))),
                    "jumpif" => Ok(Code::JUMPIF(arg!(0, Reg), arg!(1, Imm))),
                    "jumpifr" => Ok(Code::JUMPIFR(arg!(0, Reg), arg!(1, Imm))),
                    "jump" => Ok(Code::JUMP(arg!(0, Imm))),
                    "jumpr" => Ok(Code::JUMPR(arg!(0, Imm))),
                    "call" => Ok(Code::CALL(arg!(0, Imm))),
                    "ret" => Ok(Code::RET()),
                    "iret" => Ok(Code::IRET()),
                    _ => Err(Error::UnknownOperation(op.to_string())),
                }
            }
            None => Err(Error::SyntaxError),
        }
    }
}

impl Code {
    pub fn cformat(&self) -> String {
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
            Code::ADD(rd, rs1, rs2) => opfmt!("add", rd, rs1, rs2),
            Code::SUB(rd, rs1, rs2) => opfmt!("sub", rd, rs1, rs2),
            Code::AND(rd, rs1, rs2) => opfmt!("and", rd, rs1, rs2),
            Code::OR(rd, rs1, rs2) => opfmt!("or", rd, rs1, rs2),
            Code::XOR(rd, rs1, rs2) => opfmt!("xor", rd, rs1, rs2),
            Code::EQ(rd, rs1, rs2) => opfmt!("eq", rd, rs1, rs2),
            Code::NEQ(rd, rs1, rs2) => opfmt!("neq", rd, rs1, rs2),
            Code::LT(rd, rs1, rs2) => opfmt!("lt", rd, rs1, rs2),
            Code::LTS(rd, rs1, rs2) => opfmt!("lts", rd, rs1, rs2),
            Code::SR(rd, rs1) => opfmt!("sr", rd, rs1, ""),
            Code::SRS(rd, rs1) => opfmt!("srs", rd, rs1, ""),
            Code::SRR(rd, rs1) => opfmt!("srr", rd, rs1, ""),
            Code::SL(rd, rs1) => opfmt!("sl", rd, rs1, ""),
            Code::SLR(rd, rs1) => opfmt!("slr", rd, rs1, ""),
            Code::NOP() => opfmt!("nop", "", "", ""),
            Code::MOV(rd, rs1) => opfmt!("mov", rd, rs1, ""),
            Code::ADDI(rd, rs1, imm) => opfmt!("addi", rd, rs1, imm.cfmt()),
            Code::SUBI(rd, rs1, imm) => opfmt!("subi", rd, rs1, imm.cfmt()),
            Code::ANDI(rd, rs1, imm) => opfmt!("andi", rd, rs1, imm.cfmt()),
            Code::ORI(rd, rs1, imm) => opfmt!("ori", rd, rs1, imm.cfmt()),
            Code::XORI(rd, rs1, imm) => opfmt!("xori", rd, rs1, imm.cfmt()),
            Code::EQI(rd, rs1, imm) => opfmt!("eqi", rd, rs1, imm.cfmt()),
            Code::NEQI(rd, rs1, imm) => opfmt!("neqi", rd, rs1, imm.cfmt()),
            Code::LTI(rd, rs1, imm) => opfmt!("lti", rd, rs1, imm.cfmt()),
            Code::LTSI(rd, rs1, imm) => opfmt!("ltsi", rd, rs1, imm.cfmt()),
            Code::NOT(rd, rs1) => opfmt!("not", rd, rs1, ""),
            Code::LOADI(rd, imm) => opfmt!("loadi", rd, "", imm.cfmt()),
            Code::LOAD(rd, rs1, imm) => opfmt!("load", rd, rs1, imm.cfmt()),
            Code::STORE(rs2, rs1, imm) => opfmt!("store", rs2, rs1, imm.cfmt()),
            Code::JUMPIF(rs2, imm) => opfmt!("jumpif", rs2, "", imm.cfmt()),
            Code::JUMPIFR(rs2, imm) => opfmt!("jumpifr", rs2, "", imm.cfmt()),
            Code::JUMP(imm) => opfmt!("jump", "", "", imm.cfmt()),
            Code::JUMPR(imm) => opfmt!("jumpr", "", "", imm.cfmt()),
            Code::CALL(imm) => opfmt!("call", "", "", imm.cfmt()),
            Code::RET() => opfmt!("ret", "", "", ""),
            Code::IRET() => opfmt!("iret", "", "", ""),
        }
    }
}

impl Code {
    pub fn resolve(&self, idents: &Idents) -> Result<Inst, error::Error> {
        match self {
            Code::ADD(rd, rs1, rs2) => Ok(Inst::ADD(*rd, *rs1, *rs2)),
            Code::SUB(rd, rs1, rs2) => Ok(Inst::SUB(*rd, *rs1, *rs2)),
            Code::AND(rd, rs1, rs2) => Ok(Inst::AND(*rd, *rs1, *rs2)),
            Code::OR(rd, rs1, rs2) => Ok(Inst::OR(*rd, *rs1, *rs2)),
            Code::XOR(rd, rs1, rs2) => Ok(Inst::XOR(*rd, *rs1, *rs2)),
            Code::EQ(rd, rs1, rs2) => Ok(Inst::EQ(*rd, *rs1, *rs2)),
            Code::NEQ(rd, rs1, rs2) => Ok(Inst::NEQ(*rd, *rs1, *rs2)),
            Code::LT(rd, rs1, rs2) => Ok(Inst::LT(*rd, *rs1, *rs2)),
            Code::LTS(rd, rs1, rs2) => Ok(Inst::LTS(*rd, *rs1, *rs2)),
            Code::SR(rd, rs1) => Ok(Inst::SR(*rd, *rs1)),
            Code::SRS(rd, rs1) => Ok(Inst::SRS(*rd, *rs1)),
            Code::SRR(rd, rs1) => Ok(Inst::SRR(*rd, *rs1)),
            Code::SL(rd, rs1) => Ok(Inst::SL(*rd, *rs1)),
            Code::SLR(rd, rs1) => Ok(Inst::SLR(*rd, *rs1)),
            Code::MOV(rd, rs1) => Ok(Inst::MOV(*rd, *rs1)),
            Code::ADDI(rd, rs1, imm) => Ok(Inst::ADDI(*rd, *rs1, imm.resolve(&idents)?)),
            Code::SUBI(rd, rs1, imm) => Ok(Inst::SUBI(*rd, *rs1, imm.resolve(&idents)?)),
            Code::ANDI(rd, rs1, imm) => Ok(Inst::ANDI(*rd, *rs1, imm.resolve(&idents)?)),
            Code::ORI(rd, rs1, imm) => Ok(Inst::ORI(*rd, *rs1, imm.resolve(&idents)?)),
            Code::XORI(rd, rs1, imm) => Ok(Inst::XORI(*rd, *rs1, imm.resolve(&idents)?)),
            Code::EQI(rd, rs1, imm) => Ok(Inst::EQI(*rd, *rs1, imm.resolve(&idents)?)),
            Code::NEQI(rd, rs1, imm) => Ok(Inst::NEQI(*rd, *rs1, imm.resolve(&idents)?)),
            Code::LTI(rd, rs1, imm) => Ok(Inst::LTI(*rd, *rs1, imm.resolve(&idents)?)),
            Code::LTSI(rd, rs1, imm) => Ok(Inst::LTSI(*rd, *rs1, imm.resolve(&idents)?)),
            Code::NOT(rd, rs1) => Ok(Inst::NOT(*rd, *rs1)),
            Code::LOADI(rd, imm) => Ok(Inst::LOADI(*rd, imm.resolve(&idents)?)),
            Code::LOAD(rd, rs1, imm) => Ok(Inst::LOAD(*rd, *rs1, imm.resolve(&idents)?)),
            Code::STORE(rs2, rs1, imm) => Ok(Inst::STORE(*rs2, *rs1, imm.resolve(&idents)?)),
            Code::NOP() => Ok(Inst::NOP()),
            Code::JUMPIF(rs2, imm) => Ok(Inst::JUMPIF(*rs2, imm.resolve(&idents)?)),
            Code::JUMPIFR(rs2, imm) => Ok(Inst::JUMPIFR(*rs2, imm.resolve(&idents)?)),
            Code::JUMP(imm) => Ok(Inst::JUMP(imm.resolve(&idents)?)),
            Code::JUMPR(imm) => Ok(Inst::JUMPR(imm.resolve(&idents)?)),
            Code::CALL(imm) => Ok(Inst::CALL(imm.resolve(&idents)?)),
            Code::RET() => Ok(Inst::RET()),
            Code::IRET() => Ok(Inst::IRET()),
        }
    }
}

// ----------------------------------------------------------------------------
// Immidiate

#[derive(Debug, Clone)]
pub enum Imm {
    Literal(u16),
    Ident(String, Option<u16>),
}

impl Imm {
    fn parse(s: &str) -> Option<Imm> {
        match parse_with_prefix(s) {
            Ok(v) => Some(Imm::Literal(v)),
            Err(_) => Some(Imm::Ident(s.to_string(), None)),
        }
    }

    pub fn resolve(&self, idents: &Idents) -> Result<u16, error::Error> {
        match self {
            Imm::Literal(v) => Ok(*v),
            Imm::Ident(s, _) => match idents.get_val(s) {
                Some(v) => Ok(v),
                None => Err(error::Error::UndefinedLabel(s.clone())),
            },
        }
    }

    fn cfmt(&self) -> String {
        match self {
            Imm::Ident(str, val) => match val {
                Some(v) => cformat!("<g>0x{:0>4X}({})</>", v, str),
                _ => cformat!("<r,u>{}</>", str),
            },
            Imm::Literal(v) => cformat!("<y>0x{:0>4X}</>", v),
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
