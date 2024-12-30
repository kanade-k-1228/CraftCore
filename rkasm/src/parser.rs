use arch::{inst::Inst, reg::Reg};
use color_print::cformat;
use std::num::ParseIntError;

use crate::{label::Labels, msg::Msg};

// ----------------------------------------------------------------------------
// Line

#[derive(Debug, Clone)]
pub struct Line {
    file: String,
    idx: usize,
    raw: String,
    content: Content,
    comment: Option<String>,
}

impl Line {
    pub fn parse(path: &str, idx: usize, str: &str, pc: u16) -> (Self, Vec<Msg>) {
        let (code, comment) = match str.split_once(";") {
            Some((code, comment)) => (code.to_string(), Some(comment.to_string())),
            None => (str.to_string(), None),
        };
        let (content, msgs) = Content::parse(&code, pc);
        (
            Self {
                file: path.to_string(),
                idx: idx + 1,
                raw: str.to_string(),
                comment,
                content,
            },
            msgs,
        )
    }

    pub fn get_info(&self) -> (&str, usize, &str) {
        (&self.file, self.idx, &self.raw)
    }

    pub fn get_op(&self) -> Option<&Asm> {
        match &self.content {
            Content::Asm { asm, .. } => Some(asm),
            _ => None,
        }
    }

    pub fn get_label(&self) -> Option<&Label> {
        match &self.content {
            Content::Label(label) => Some(label),
            _ => None,
        }
    }
}

pub const HL: &str = "+------+------+-------------+-----------------------------+";

impl Line {
    pub fn cformat(&self, labels: &Labels) -> String {
        let file = if self.idx == 1 {
            format!("{}\n| {:<55} |\n{}\n", HL, self.file, HL)
        } else {
            "".to_string()
        };

        let binary = {
            let mut binary = " ".repeat(11);
            if let Content::Asm { pc: _, asm } = &self.content {
                if let Some(ok) = asm.resolve(&labels) {
                    let bin = ok.to_op().clone().to_bin();
                    binary = format!(
                        "{:02X} {:02X} {:02X} {:02X}",
                        (bin >> 24) & 0xFF,
                        (bin >> 16) & 0xFF,
                        (bin >> 8) & 0xFF,
                        bin & 0xFF
                    );
                }
            }
            binary
        };

        let pc = {
            if let Content::Asm { pc, .. } = self.content {
                cformat!("<green>{:0>4X}</>", pc)
            } else {
                " ".repeat(4).to_string()
            }
        };

        let stmt = self.content.cformat(labels);

        let comment = match &self.comment {
            Some(s) => format!(" ;{}", s),
            None => format!(""),
        };

        format!(
            "{}| {:>4} | {} | {} | {}{}",
            file, self.idx, pc, binary, stmt, comment
        )
    }
}

// ----------------------------------------------------------------------------
// Statement

#[derive(Debug, Clone)]
pub enum Content {
    None,
    Err,
    Asm { pc: u16, asm: Asm },
    Label(Label),
}

impl Content {
    fn parse(code: &str, pc: u16) -> (Content, Vec<Msg>) {
        let msgs: Vec<Msg> = vec![];

        let words: Vec<&str> = code.split_whitespace().collect();

        if words.len() == 0 {
            return (Content::None, msgs);
        }

        match Label::parse(&code, pc) {
            Some(lab) => return (Content::Label(lab), msgs),
            None => {}
        };

        // Operation
        match Asm::parse(&code) {
            Ok(op) => return (Content::Asm { pc, asm: op }, msgs),
            Err(msg) => {
                return (Content::Err, vec![Msg::Error(msg)]);
            }
        };
    }
}

impl Content {
    fn cformat(&self, labels: &Labels) -> String {
        match self {
            Content::None => "".to_string(),
            Content::Err => cformat!("<red,bold>! ERROR</>"),
            Content::Asm { pc: _, asm } => asm.cformat(labels),
            Content::Label(label) => label.cformat(),
        }
    }
}

// ----------------------------------------------------------------------------
// Label

#[derive(Debug, Clone)]
pub enum Label {
    Code { key: String, val: u16 },
    Addr { key: String, val: u16 },
    Const { key: String, val: u16 },
}

impl Label {
    pub fn get_key(&self) -> String {
        match self {
            Label::Code { key, .. } => key,
            Label::Addr { key, .. } => key,
            Label::Const { key, .. } => key,
        }
        .to_string()
    }
    pub fn get_val(&self) -> u16 {
        match self {
            Label::Code { val, .. } => *val,
            Label::Addr { val, .. } => *val,
            Label::Const { val, .. } => *val,
        }
    }
}

impl Label {
    fn parse(code: &str, pc: u16) -> Option<Label> {
        let words: Vec<&str> = code.split_whitespace().collect();
        if let Some(key) = words.get(0) {
            if let Some(head) = key.chars().nth(0) {
                // @0x0123 hoge
                if head == '@' {
                    if let Some(label) = words.get(1) {
                        if let Some(value) = key.get(1..) {
                            let key = label.to_string();
                            let val = parse_with_prefix(value).unwrap();
                            return Some(Label::Addr { key, val });
                        }
                    } else {
                    }
                }
                // #0x0123 const
                if head == '#' {
                    if let Some(label) = words.get(1) {
                        if let Some(value) = key.get(1..) {
                            let key = label.to_string();
                            let val = parse_with_prefix(value).unwrap();
                            return Some(Label::Const { key, val });
                        }
                    }
                }
            }

            if let Some(tail) = key.chars().last() {
                // main:
                if tail == ':' {
                    if let Some(label) = words.get(0) {
                        let label = label.to_string();
                        if let Some(label) = label.get(0..label.len() - 1) {
                            let key = label.to_string();
                            return Some(Label::Code { key, val: pc });
                        }
                    }
                }
            }
        };

        None
    }
}

impl Label {
    fn cformat(&self) -> String {
        match self {
            Label::Code { key, val } => cformat!("<green>{}:{:04X}</>", key, val),
            Label::Addr { key, val } => cformat!("<blue>{:04X} = {}</>", val, key),
            Label::Const { key, val } => cformat!("<yellow>{:04X} = {}</>", val, key),
        }
    }
}

// ----------------------------------------------------------------------------
// Operation

#[derive(Debug, Clone)]
pub enum Asm {
    ADD { rd: Reg, rs1: Reg, rs2: Reg },
    SUB { rd: Reg, rs1: Reg, rs2: Reg },
    AND { rd: Reg, rs1: Reg, rs2: Reg },
    OR { rd: Reg, rs1: Reg, rs2: Reg },
    XOR { rd: Reg, rs1: Reg, rs2: Reg },
    EQ { rd: Reg, rs1: Reg, rs2: Reg },
    NEQ { rd: Reg, rs1: Reg, rs2: Reg },
    LT { rd: Reg, rs1: Reg, rs2: Reg },
    LTS { rd: Reg, rs1: Reg, rs2: Reg },
    SR { rd: Reg, rs1: Reg },
    SRS { rd: Reg, rs1: Reg },
    SRR { rd: Reg, rs1: Reg },
    SL { rd: Reg, rs1: Reg },
    SLR { rd: Reg, rs1: Reg },
    NOP,
    MOV { rd: Reg, rs1: Reg },
    ADDI { rd: Reg, rs1: Reg, imm: Imm },
    SUBI { rd: Reg, rs1: Reg, imm: Imm },
    ANDI { rd: Reg, rs1: Reg, imm: Imm },
    ORI { rd: Reg, rs1: Reg, imm: Imm },
    XORI { rd: Reg, rs1: Reg, imm: Imm },
    EQI { rd: Reg, rs1: Reg, imm: Imm },
    NEQI { rd: Reg, rs1: Reg, imm: Imm },
    LTI { rd: Reg, rs1: Reg, imm: Imm },
    LTSI { rd: Reg, rs1: Reg, imm: Imm },
    NOT { rd: Reg, rs1: Reg },
    LOADI { rd: Reg, imm: Imm },
    LOAD { rd: Reg, rs1: Reg, imm: Imm },
    STORE { rs2: Reg, rs1: Reg, imm: Imm },
    IF { rs2: Reg, imm: Imm },
    IFR { rs2: Reg, imm: Imm },
    JUMP { imm: Imm },
    JUMPR { imm: Imm },
    CALL { imm: Imm },
    RET,
    IRET,
}

impl Asm {
    fn parse(code: &str) -> Result<Asm, String> {
        if let Some((op, args)) = code.split_whitespace().collect::<Vec<_>>().split_first() {
            // Get argument by index and parse as Type
            // Example: arg!(0, Reg) -> Reg
            // Meaning: Get 0th argument and parse as Reg
            macro_rules! arg {
                ($index:expr, $Type:ident) => {
                    $Type::parse(args.get($index).ok_or("More argument required")?)?
                };
            }

            use Asm::*;

            macro_rules! rd_rs1_rs2 {
                ($op:ident) => {
                    $op {
                        rd: arg!(0, Reg),
                        rs1: arg!(1, Reg),
                        rs2: arg!(2, Reg),
                    }
                };
            }

            macro_rules! rd_rs1 {
                ($op:ident) => {
                    $op {
                        rd: arg!(0, Reg),
                        rs1: arg!(1, Reg),
                    }
                };
            }

            macro_rules! rd_rs1_imm {
                ($op:ident) => {
                    $op {
                        rd: arg!(0, Reg),
                        rs1: arg!(1, Reg),
                        imm: arg!(2, Imm),
                    }
                };
            }

            macro_rules! rs2_rs1_imm {
                ($op:ident) => {
                    $op {
                        rs2: arg!(0, Reg),
                        rs1: arg!(1, Reg),
                        imm: arg!(2, Imm),
                    }
                };
            }

            macro_rules! rd_imm {
                ($op:ident) => {
                    $op {
                        rd: arg!(0, Reg),
                        imm: arg!(1, Imm),
                    }
                };
            }

            macro_rules! rs1_imm {
                ($op:ident) => {
                    $op {
                        rs2: arg!(0, Reg),
                        imm: arg!(1, Imm),
                    }
                };
            }

            macro_rules! imm {
                ($op:ident) => {
                    $op { imm: arg!(0, Imm) }
                };
            }

            let op: &str = op.to_owned();
            let asm = match op {
                "nop" => NOP,
                "add" => rd_rs1_rs2!(ADD),
                "sub" => rd_rs1_rs2!(SUB),
                "and" => rd_rs1_rs2!(AND),
                "or" => rd_rs1_rs2!(OR),
                "xor" => rd_rs1_rs2!(XOR),
                "eq" => rd_rs1_rs2!(EQ),
                "neq" => rd_rs1_rs2!(NEQ),
                "lt" => rd_rs1_rs2!(LT),
                "lts" => rd_rs1_rs2!(LTS),
                "sr" => rd_rs1!(SR),
                "srs" => rd_rs1!(SRS),
                "srr" => rd_rs1!(SRR),
                "sl" => rd_rs1!(SL),
                "slr" => rd_rs1!(SLR),
                "mov" => rd_rs1!(MOV),
                "addi" => rd_rs1_imm!(ADDI),
                "subi" => rd_rs1_imm!(SUBI),
                "andi" => rd_rs1_imm!(ANDI),
                "ori" => rd_rs1_imm!(ORI),
                "xori" => rd_rs1_imm!(XORI),
                "eqi" => rd_rs1_imm!(EQI),
                "neqi" => rd_rs1_imm!(NEQI),
                "lti" => rd_rs1_imm!(LTI),
                "ltsi" => rd_rs1_imm!(LTSI),
                "not" => rd_rs1!(NOT),
                "loadi" => rd_imm!(LOADI),
                "load" => rd_rs1_imm!(LOAD),
                "store" => rs2_rs1_imm!(STORE),
                "if" => rs1_imm!(IF),
                "ifr" => rs1_imm!(IFR),
                "jump" => imm!(JUMP),
                "jumpr" => imm!(JUMPR),
                "call" => imm!(CALL),
                "ret" => Asm::RET,
                "iret" => Asm::IRET,
                _ => return Err(format!("Unknown operation: `{}`", op)),
            };
            return Ok(asm);
        }
        return Err(format!("Syntax Error: Cannot parse"));
    }
}

impl Asm {
    fn cformat(&self, labels: &Labels) -> String {
        match self {
            Asm::ADD { rd, rs1, rs2 } => cformat!(
                "  <red>{:<6}</><blue>{:<6}{:<6}{:<8}</>",
                "add",
                rd,
                rs1,
                rs2
            ),
            Asm::SUB { rd, rs1, rs2 } => cformat!(
                "  <red>{:<6}</><blue>{:<6}{:<6}{:<8}</>",
                "sub",
                rd,
                rs1,
                rs2
            ),
            Asm::AND { rd, rs1, rs2 } => cformat!(
                "  <red>{:<6}</><blue>{:<6}{:<6}{:<8}</>",
                "and",
                rd,
                rs1,
                rs2
            ),
            Asm::OR { rd, rs1, rs2 } => cformat!(
                "  <red>{:<6}</><blue>{:<6}{:<6}{:<8}</>",
                "or",
                rd,
                rs1,
                rs2
            ),
            Asm::XOR { rd, rs1, rs2 } => cformat!(
                "  <red>{:<6}</><blue>{:<6}{:<6}{:<8}</>",
                "xor",
                rd,
                rs1,
                rs2
            ),
            Asm::EQ { rd, rs1, rs2 } => cformat!(
                "  <red>{:<6}</><blue>{:<6}{:<6}{:<8}</>",
                "eq",
                rd,
                rs1,
                rs2
            ),
            Asm::NEQ { rd, rs1, rs2 } => cformat!(
                "  <red>{:<6}</><blue>{:<6}{:<6}{:<8}</>",
                "neq",
                rd,
                rs1,
                rs2
            ),
            Asm::LT { rd, rs1, rs2 } => cformat!(
                "  <red>{:<6}</><blue>{:<6}{:<6}{:<8}</>",
                "lt",
                rd,
                rs1,
                rs2
            ),
            Asm::LTS { rd, rs1, rs2 } => cformat!(
                "  <red>{:<6}</><blue>{:<6}{:<6}{:<8}</>",
                "lts",
                rd,
                rs1,
                rs2
            ),

            Asm::SR { rd, rs1 } => {
                cformat!("  <red>{:<6}</><blue>{:<6}{:<6}{:<8}</>", "sr", rd, rs1, "")
            }
            Asm::SRS { rd, rs1 } => cformat!(
                "  <red>{:<6}</><blue>{:<6}{:<6}{:<8}</>",
                "srs",
                rd,
                rs1,
                ""
            ),
            Asm::SRR { rd, rs1 } => cformat!(
                "  <red>{:<6}</><blue>{:<6}{:<6}{:<8}</>",
                "srr",
                rd,
                rs1,
                ""
            ),
            Asm::SL { rd, rs1 } => {
                cformat!("  <red>{:<6}</><blue>{:<6}{:<6}{:<8}</>", "sl", rd, rs1, "")
            }
            Asm::SLR { rd, rs1 } => cformat!(
                "  <red>{:<6}</><blue>{:<6}{:<6}{:<8}</>",
                "slr",
                rd,
                rs1,
                ""
            ),

            Asm::NOP => cformat!("  <red>{:<6}</><blue>{:<6}{:<6}{:<8}</>", "nop", "", "", ""),
            Asm::MOV { rd, rs1 } => cformat!(
                "  <red>{:<6}</><blue>{:<6}{:<6}{:<8}</>",
                "mov",
                rd,
                rs1,
                ""
            ),

            Asm::ADDI { rd, rs1, imm } => cformat!(
                "  <red>{:<6}</><blue>{:<6}{:<6}</>{:<18}",
                "addi",
                rd,
                rs1,
                imm.cformat(labels)
            ),
            Asm::SUBI { rd, rs1, imm } => cformat!(
                "  <red>{:<6}</><blue>{:<6}{:<6}</>{:<18}",
                "subi",
                rd,
                rs1,
                imm.cformat(labels)
            ),
            Asm::ANDI { rd, rs1, imm } => cformat!(
                "  <red>{:<6}</><blue>{:<6}{:<6}</>{:<18}",
                "andi",
                rd,
                rs1,
                imm.cformat(labels)
            ),
            Asm::ORI { rd, rs1, imm } => cformat!(
                "  <red>{:<6}</><blue>{:<6}{:<6}</>{:<18}",
                "ori",
                rd,
                rs1,
                imm.cformat(labels)
            ),
            Asm::XORI { rd, rs1, imm } => cformat!(
                "  <red>{:<6}</><blue>{:<6}{:<6}</>{:<18}",
                "xori",
                rd,
                rs1,
                imm.cformat(labels)
            ),
            Asm::EQI { rd, rs1, imm } => cformat!(
                "  <red>{:<6}</><blue>{:<6}{:<6}</>{:<18}",
                "eqi",
                rd,
                rs1,
                imm.cformat(labels)
            ),
            Asm::NEQI { rd, rs1, imm } => cformat!(
                "  <red>{:<6}</><blue>{:<6}{:<6}</>{:<18}",
                "neqi",
                rd,
                rs1,
                imm.cformat(labels)
            ),
            Asm::LTI { rd, rs1, imm } => cformat!(
                "  <red>{:<6}</><blue>{:<6}{:<6}</>{:<18}",
                "lti",
                rd,
                rs1,
                imm.cformat(labels)
            ),
            Asm::LTSI { rd, rs1, imm } => cformat!(
                "  <red>{:<6}</><blue>{:<6}{:<6}</>{:<18}",
                "ltsi",
                rd,
                rs1,
                imm.cformat(labels)
            ),

            Asm::NOT { rd, rs1 } => cformat!(
                "  <red>{:<6}</><blue>{:<6}{:<6}{:<8}</>",
                "not",
                rd,
                rs1,
                ""
            ),
            Asm::LOADI { rd, imm } => cformat!(
                "  <red>{:<6}</><blue>{:<6}</>{:<18}",
                "loadi",
                rd,
                imm.cformat(labels)
            ),

            Asm::LOAD { rd, rs1, imm } => cformat!(
                "  <red>{:<6}</><blue>{:<6}{:<6}</>{:<18}",
                "load",
                rd,
                rs1,
                imm.cformat(labels)
            ),
            Asm::STORE { rs2, rs1, imm } => cformat!(
                "  <red>{:<6}</><blue>{:<6}{:<6}</>{:<18}",
                "store",
                rs2,
                rs1,
                imm.cformat(labels)
            ),

            Asm::IF { rs2, imm } => cformat!(
                "  <red>{:<6}</><blue>{:<6}</>{:<18}",
                "if",
                rs2,
                imm.cformat(labels)
            ),
            Asm::IFR { rs2, imm } => cformat!(
                "  <red>{:<6}</><blue>{:<6}</>{:<18}",
                "ifr",
                rs2,
                imm.cformat(labels)
            ),
            Asm::JUMP { imm } => cformat!(
                "  <red>{:<6}</><blue>{:<6}</>{:<18}",
                "jump",
                "",
                imm.cformat(labels)
            ),
            Asm::JUMPR { imm } => cformat!(
                "  <red>{:<6}</><blue>{:<6}</>{:<18}",
                "jumpr",
                "",
                imm.cformat(labels)
            ),
            Asm::CALL { imm } => cformat!(
                "  <red>{:<6}</><blue>{:<6}</>{:<18}",
                "call",
                "",
                imm.cformat(labels)
            ),
            Asm::RET => cformat!("  <red>{:<6}</><blue>{:<6}{:<6}{:<8}</>", "ret", "", "", ""),
            Asm::IRET => cformat!(
                "  <red>{:<6}</><blue>{:<6}{:<6}{:<8}</>",
                "iret",
                "",
                "",
                ""
            ),
        }
    }
}

impl Asm {
    pub fn resolve(&self, labels: &Labels) -> Option<Inst> {
        match self {
            Asm::ADD { rd, rs1, rs2 } => Some(Inst::ADD(*rd, *rs1, *rs2)),
            Asm::SUB { rd, rs1, rs2 } => Some(Inst::SUB(*rd, *rs1, *rs2)),
            Asm::AND { rd, rs1, rs2 } => Some(Inst::AND(*rd, *rs1, *rs2)),
            Asm::OR { rd, rs1, rs2 } => Some(Inst::OR(*rd, *rs1, *rs2)),
            Asm::XOR { rd, rs1, rs2 } => Some(Inst::XOR(*rd, *rs1, *rs2)),
            Asm::EQ { rd, rs1, rs2 } => Some(Inst::EQ(*rd, *rs1, *rs2)),
            Asm::NEQ { rd, rs1, rs2 } => Some(Inst::NEQ(*rd, *rs1, *rs2)),
            Asm::LT { rd, rs1, rs2 } => Some(Inst::LT(*rd, *rs1, *rs2)),
            Asm::LTS { rd, rs1, rs2 } => Some(Inst::LTS(*rd, *rs1, *rs2)),
            Asm::SR { rd, rs1 } => Some(Inst::SR(*rd, *rs1)),
            Asm::SRS { rd, rs1 } => Some(Inst::SRS(*rd, *rs1)),
            Asm::SRR { rd, rs1 } => Some(Inst::SRR(*rd, *rs1)),
            Asm::SL { rd, rs1 } => Some(Inst::SL(*rd, *rs1)),
            Asm::SLR { rd, rs1 } => Some(Inst::SLR(*rd, *rs1)),
            Asm::MOV { rd, rs1 } => Some(Inst::MOV(*rd, *rs1)),
            Asm::ADDI { rd, rs1, imm } => Some(Inst::ADDI(*rd, *rs1, imm.resolve(&labels)?)),
            Asm::SUBI { rd, rs1, imm } => Some(Inst::SUBI(*rd, *rs1, imm.resolve(&labels)?)),
            Asm::ANDI { rd, rs1, imm } => Some(Inst::ANDI(*rd, *rs1, imm.resolve(&labels)?)),
            Asm::ORI { rd, rs1, imm } => Some(Inst::ORI(*rd, *rs1, imm.resolve(&labels)?)),
            Asm::XORI { rd, rs1, imm } => Some(Inst::XORI(*rd, *rs1, imm.resolve(&labels)?)),
            Asm::EQI { rd, rs1, imm } => Some(Inst::EQI(*rd, *rs1, imm.resolve(&labels)?)),
            Asm::NEQI { rd, rs1, imm } => Some(Inst::NEQI(*rd, *rs1, imm.resolve(&labels)?)),
            Asm::LTI { rd, rs1, imm } => Some(Inst::LTI(*rd, *rs1, imm.resolve(&labels)?)),
            Asm::LTSI { rd, rs1, imm } => Some(Inst::LTSI(*rd, *rs1, imm.resolve(&labels)?)),
            Asm::NOT { rd, rs1 } => Some(Inst::NOT(*rd, *rs1)),
            Asm::LOADI { rd, imm } => Some(Inst::LOADI(*rd, imm.resolve(&labels)?)),
            Asm::LOAD { rd, rs1, imm } => Some(Inst::LOAD(*rd, *rs1, imm.resolve(&labels)?)),
            Asm::STORE { rs2, rs1, imm } => Some(Inst::STORE(*rs2, *rs1, imm.resolve(&labels)?)),
            Asm::NOP => Some(Inst::NOP),
            Asm::IF { rs2, imm } => Some(Inst::IF(*rs2, imm.resolve(&labels)?)),
            Asm::IFR { rs2, imm } => Some(Inst::IFR(*rs2, imm.resolve(&labels)?)),
            Asm::JUMP { imm } => Some(Inst::JUMP(imm.resolve(&labels)?)),
            Asm::JUMPR { imm } => Some(Inst::JUMPR(imm.resolve(&labels)?)),
            Asm::CALL { imm } => Some(Inst::CALL(imm.resolve(&labels)?)),
            Asm::RET => Some(Inst::RET),
            Asm::IRET => Some(Inst::IRET),
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
    fn parse(s: &str) -> Result<Imm, String> {
        if let Ok(value) = parse_with_prefix(s) {
            return Ok(Imm::Literal(value));
        };
        Ok(Imm::Label(s.to_string()))
    }
    fn cformat(&self, labels: &Labels) -> String {
        match self {
            Imm::Label(str) => match labels.get(str) {
                Some(line) => line.content.cformat(labels),
                None => cformat!("<underline>{}</>", str),
            },
            Imm::Literal(val) => cformat!("<yellow>0x{:0>4X}</>", val),
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
    pub fn resolve(&self, labels: &Labels) -> Option<u16> {
        match self {
            Imm::Literal(val) => Some(*val),
            Imm::Label(label) => {
                if let Some(val) = labels.get_val(label) {
                    Some(val)
                } else {
                    None
                }
            }
        }
    }
}
