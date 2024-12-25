use arch::{psudo::Psudo, reg::Reg};
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

impl Line {
    pub fn cformat(&self, labels: &Labels) -> String {
        let comment = match &self.comment {
            Some(s) => format!(" ;{}", s),
            None => format!(""),
        };

        let binary = {
            if let Content::Asm { pc: _, asm } = &self.content {
                let bin = asm.resolve(labels).to_op().to_bin();
                format!(
                    "{:02X} {:02X} {:02X} {:02X}",
                    (bin >> 24) & 0xFF,
                    (bin >> 16) & 0xFF,
                    (bin >> 8) & 0xFF,
                    bin & 0xFF
                )
            } else {
                " ".repeat(11).to_string()
            }
        };

        let pc = {
            if let Content::Asm { pc, .. } = self.content {
                cformat!("<green>{:0>4X}</>", pc)
            } else {
                " ".repeat(4).to_string()
            }
        };

        let stmt = self.content.cformat();

        let file = if self.idx == 1 {
            let line = "+------+------+-------------+-----------------------------+";
            format!("{}\n| {:<55} |\n{}\n", line, self.file, line)
        } else {
            "".to_string()
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
    fn cformat(&self) -> String {
        match self {
            Content::None => "".to_string(),
            Content::Err => cformat!("<red,bold>! ERROR</>"),
            Content::Asm { pc: _, asm: op } => op.cformat(),
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
            let op: &str = op.to_owned();
            let asm = match op {
                "add" => Asm::ADD {
                    rd: Reg::parse(args[0]).unwrap(),
                    rs1: Reg::parse(args[1]).unwrap(),
                    rs2: Reg::parse(args[2]).unwrap(),
                },
                "sub" => Asm::SUB {
                    rd: Reg::parse(args[0]).unwrap(),
                    rs1: Reg::parse(args[1]).unwrap(),
                    rs2: Reg::parse(args[2]).unwrap(),
                },
                "and" => Asm::AND {
                    rd: Reg::parse(args[0]).unwrap(),
                    rs1: Reg::parse(args[1]).unwrap(),
                    rs2: Reg::parse(args[2]).unwrap(),
                },
                "or" => Asm::OR {
                    rd: Reg::parse(args[0]).unwrap(),
                    rs1: Reg::parse(args[1]).unwrap(),
                    rs2: Reg::parse(args[2]).unwrap(),
                },
                "xor" => Asm::XOR {
                    rd: Reg::parse(args[0]).unwrap(),
                    rs1: Reg::parse(args[1]).unwrap(),
                    rs2: Reg::parse(args[2]).unwrap(),
                },
                "eq" => Asm::EQ {
                    rd: Reg::parse(args[0]).unwrap(),
                    rs1: Reg::parse(args[1]).unwrap(),
                    rs2: Reg::parse(args[2]).unwrap(),
                },
                "neq" => Asm::NEQ {
                    rd: Reg::parse(args[0]).unwrap(),
                    rs1: Reg::parse(args[1]).unwrap(),
                    rs2: Reg::parse(args[2]).unwrap(),
                },
                "lt" => Asm::LT {
                    rd: Reg::parse(args[0]).unwrap(),
                    rs1: Reg::parse(args[1]).unwrap(),
                    rs2: Reg::parse(args[2]).unwrap(),
                },
                "lts" => Asm::LTS {
                    rd: Reg::parse(args[0]).unwrap(),
                    rs1: Reg::parse(args[1]).unwrap(),
                    rs2: Reg::parse(args[2]).unwrap(),
                },
                "sr" => Asm::SR {
                    rd: Reg::parse(args[0]).unwrap(),
                    rs1: Reg::parse(args[1]).unwrap(),
                },
                "srs" => Asm::SRS {
                    rd: Reg::parse(args[0]).unwrap(),
                    rs1: Reg::parse(args[1]).unwrap(),
                },
                "srr" => Asm::SRR {
                    rd: Reg::parse(args[0]).unwrap(),
                    rs1: Reg::parse(args[1]).unwrap(),
                },
                "sl" => Asm::SL {
                    rd: Reg::parse(args[0]).unwrap(),
                    rs1: Reg::parse(args[1]).unwrap(),
                },
                "slr" => Asm::SLR {
                    rd: Reg::parse(args[0]).unwrap(),
                    rs1: Reg::parse(args[1]).unwrap(),
                },
                "nop" => Asm::NOP,
                "mov" => Asm::MOV {
                    rd: Reg::parse(args[0]).unwrap(),
                    rs1: Reg::parse(args[1]).unwrap(),
                },
                "addi" => Asm::ADDI {
                    rd: Reg::parse(args[0]).unwrap(),
                    rs1: Reg::parse(args[1]).unwrap(),
                    imm: Imm::parse(args[2]).unwrap(),
                },
                "subi" => Asm::SUBI {
                    rd: Reg::parse(args[0]).unwrap(),
                    rs1: Reg::parse(args[1]).unwrap(),
                    imm: Imm::parse(args[2]).unwrap(),
                },
                "andi" => Asm::ANDI {
                    rd: Reg::parse(args[0]).unwrap(),
                    rs1: Reg::parse(args[1]).unwrap(),
                    imm: Imm::parse(args[2]).unwrap(),
                },
                "ori" => Asm::ORI {
                    rd: Reg::parse(args[0]).unwrap(),
                    rs1: Reg::parse(args[1]).unwrap(),
                    imm: Imm::parse(args[2]).unwrap(),
                },
                "xori" => Asm::XORI {
                    rd: Reg::parse(args[0]).unwrap(),
                    rs1: Reg::parse(args[1]).unwrap(),
                    imm: Imm::parse(args[2]).unwrap(),
                },
                "eqi" => Asm::EQI {
                    rd: Reg::parse(args[0]).unwrap(),
                    rs1: Reg::parse(args[1]).unwrap(),
                    imm: Imm::parse(args[2]).unwrap(),
                },
                "neqi" => Asm::NEQI {
                    rd: Reg::parse(args[0]).unwrap(),
                    rs1: Reg::parse(args[1]).unwrap(),
                    imm: Imm::parse(args[2]).unwrap(),
                },
                "lti" => Asm::LTI {
                    rd: Reg::parse(args[0]).unwrap(),
                    rs1: Reg::parse(args[1]).unwrap(),
                    imm: Imm::parse(args[2]).unwrap(),
                },
                "ltsi" => Asm::LTSI {
                    rd: Reg::parse(args[0]).unwrap(),
                    rs1: Reg::parse(args[1]).unwrap(),
                    imm: Imm::parse(args[2]).unwrap(),
                },
                "not" => Asm::NOT {
                    rd: Reg::parse(args[0]).unwrap(),
                    rs1: Reg::parse(args[1]).unwrap(),
                },
                "loadi" => Asm::LOADI {
                    rd: Reg::parse(args[0]).unwrap(),
                    imm: Imm::parse(args[1]).unwrap(),
                },
                "load" => Asm::LOAD {
                    rd: Reg::parse(args[0]).unwrap(),
                    rs1: Reg::parse(args[1]).unwrap(),
                    imm: Imm::parse(args[2]).unwrap(),
                },
                "store" => Asm::STORE {
                    rs2: Reg::parse(args[0]).unwrap(),
                    rs1: Reg::parse(args[1]).unwrap(),
                    imm: Imm::parse(args[2]).unwrap(),
                },
                "if" => Asm::IF {
                    rs2: Reg::parse(args[0]).unwrap(),
                    imm: Imm::parse(args[1]).unwrap(),
                },
                "ifr" => Asm::IFR {
                    rs2: Reg::parse(args[0]).unwrap(),
                    imm: Imm::parse(args[1]).unwrap(),
                },
                "jump" => Asm::JUMP {
                    imm: Imm::parse(args[0]).unwrap(),
                },
                "jumpr" => Asm::JUMPR {
                    imm: Imm::parse(args[0]).unwrap(),
                },
                "call" => Asm::CALL {
                    imm: Imm::parse(args[0]).unwrap(),
                },
                "ret" => Asm::RET,
                "iret" => Asm::IRET,
                _ => return Err(format!("Unknown operation: `{}`", op)),
            };
            return Ok(asm);
        }
        return Err(format!("Unknown Error: Cannot parse as Op"));
    }
}

impl Asm {
    fn cformat(&self) -> String {
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
                imm.cformat()
            ),
            Asm::SUBI { rd, rs1, imm } => cformat!(
                "  <red>{:<6}</><blue>{:<6}{:<6}</>{:<18}",
                "subi",
                rd,
                rs1,
                imm.cformat()
            ),
            Asm::ANDI { rd, rs1, imm } => cformat!(
                "  <red>{:<6}</><blue>{:<6}{:<6}</>{:<18}",
                "andi",
                rd,
                rs1,
                imm.cformat()
            ),
            Asm::ORI { rd, rs1, imm } => cformat!(
                "  <red>{:<6}</><blue>{:<6}{:<6}</>{:<18}",
                "ori",
                rd,
                rs1,
                imm.cformat()
            ),
            Asm::XORI { rd, rs1, imm } => cformat!(
                "  <red>{:<6}</><blue>{:<6}{:<6}</>{:<18}",
                "xori",
                rd,
                rs1,
                imm.cformat()
            ),
            Asm::EQI { rd, rs1, imm } => cformat!(
                "  <red>{:<6}</><blue>{:<6}{:<6}</>{:<18}",
                "eqi",
                rd,
                rs1,
                imm.cformat()
            ),
            Asm::NEQI { rd, rs1, imm } => cformat!(
                "  <red>{:<6}</><blue>{:<6}{:<6}</>{:<18}",
                "neqi",
                rd,
                rs1,
                imm.cformat()
            ),
            Asm::LTI { rd, rs1, imm } => cformat!(
                "  <red>{:<6}</><blue>{:<6}{:<6}</>{:<18}",
                "lti",
                rd,
                rs1,
                imm.cformat()
            ),
            Asm::LTSI { rd, rs1, imm } => cformat!(
                "  <red>{:<6}</><blue>{:<6}{:<6}</>{:<18}",
                "ltsi",
                rd,
                rs1,
                imm.cformat()
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
                imm.cformat()
            ),

            Asm::LOAD { rd, rs1, imm } => cformat!(
                "  <red>{:<6}</><blue>{:<6}{:<6}</>{:<18}",
                "load",
                rd,
                rs1,
                imm.cformat()
            ),
            Asm::STORE { rs2, rs1, imm } => cformat!(
                "  <red>{:<6}</><blue>{:<6}{:<6}</>{:<18}",
                "store",
                rs2,
                rs1,
                imm.cformat()
            ),

            Asm::IF { rs2, imm } => cformat!(
                "  <red>{:<6}</><blue>{:<6}</>{:<18}",
                "if",
                rs2,
                imm.cformat()
            ),
            Asm::IFR { rs2, imm } => cformat!(
                "  <red>{:<6}</><blue>{:<6}</>{:<18}",
                "ifr",
                rs2,
                imm.cformat()
            ),
            Asm::JUMP { imm } => cformat!(
                "  <red>{:<6}</><blue>{:<6}</>{:<18}",
                "jump",
                "",
                imm.cformat()
            ),
            Asm::JUMPR { imm } => cformat!(
                "  <red>{:<6}</><blue>{:<6}</>{:<18}",
                "jumpr",
                "",
                imm.cformat()
            ),
            Asm::CALL { imm } => cformat!(
                "  <red>{:<6}</><blue>{:<6}</>{:<18}",
                "call",
                "",
                imm.cformat()
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
    pub fn resolve(&self, labels: &Labels) -> Psudo {
        match self {
            Asm::ADD { rd, rs1, rs2 } => Psudo::ADD(*rd as u8, *rs1 as u8, *rs2 as u8),
            Asm::SUB { rd, rs1, rs2 } => Psudo::SUB(*rd as u8, *rs1 as u8, *rs2 as u8),
            Asm::AND { rd, rs1, rs2 } => Psudo::AND(*rd as u8, *rs1 as u8, *rs2 as u8),
            Asm::OR { rd, rs1, rs2 } => Psudo::OR(*rd as u8, *rs1 as u8, *rs2 as u8),
            Asm::XOR { rd, rs1, rs2 } => Psudo::XOR(*rd as u8, *rs1 as u8, *rs2 as u8),
            Asm::EQ { rd, rs1, rs2 } => Psudo::EQ(*rd as u8, *rs1 as u8, *rs2 as u8),
            Asm::NEQ { rd, rs1, rs2 } => Psudo::NEQ(*rd as u8, *rs1 as u8, *rs2 as u8),
            Asm::LT { rd, rs1, rs2 } => Psudo::LT(*rd as u8, *rs1 as u8, *rs2 as u8),
            Asm::LTS { rd, rs1, rs2 } => Psudo::LTS(*rd as u8, *rs1 as u8, *rs2 as u8),
            Asm::SR { rd, rs1 } => Psudo::SR(*rd as u8, *rs1 as u8),
            Asm::SRS { rd, rs1 } => Psudo::SRS(*rd as u8, *rs1 as u8),
            Asm::SRR { rd, rs1 } => Psudo::SRR(*rd as u8, *rs1 as u8),
            Asm::SL { rd, rs1 } => Psudo::SL(*rd as u8, *rs1 as u8),
            Asm::SLR { rd, rs1 } => Psudo::SLR(*rd as u8, *rs1 as u8),
            Asm::NOP => Psudo::NOP,
            Asm::MOV { rd, rs1 } => Psudo::MOV(*rd as u8, *rs1 as u8),
            Asm::ADDI { rd, rs1, imm } => {
                Psudo::ADDI(*rd as u8, *rs1 as u8, imm.resolve(&labels).unwrap())
            }
            Asm::SUBI { rd, rs1, imm } => {
                Psudo::SUBI(*rd as u8, *rs1 as u8, imm.resolve(&labels).unwrap())
            }
            Asm::ANDI { rd, rs1, imm } => {
                Psudo::ANDI(*rd as u8, *rs1 as u8, imm.resolve(&labels).unwrap())
            }
            Asm::ORI { rd, rs1, imm } => {
                Psudo::ORI(*rd as u8, *rs1 as u8, imm.resolve(&labels).unwrap())
            }
            Asm::XORI { rd, rs1, imm } => {
                Psudo::XORI(*rd as u8, *rs1 as u8, imm.resolve(&labels).unwrap())
            }
            Asm::EQI { rd, rs1, imm } => {
                Psudo::EQI(*rd as u8, *rs1 as u8, imm.resolve(&labels).unwrap())
            }
            Asm::NEQI { rd, rs1, imm } => {
                Psudo::NEQI(*rd as u8, *rs1 as u8, imm.resolve(&labels).unwrap())
            }
            Asm::LTI { rd, rs1, imm } => {
                Psudo::LTI(*rd as u8, *rs1 as u8, imm.resolve(&labels).unwrap())
            }
            Asm::LTSI { rd, rs1, imm } => {
                Psudo::LTSI(*rd as u8, *rs1 as u8, imm.resolve(&labels).unwrap())
            }
            Asm::NOT { rd, rs1 } => Psudo::NOT(*rd as u8, *rs1 as u8),
            Asm::LOADI { rd, imm } => Psudo::LOADI(*rd as u8, imm.resolve(&labels).unwrap()),
            Asm::LOAD { rd, rs1, imm } => {
                Psudo::LOAD(*rd as u8, *rs1 as u8, imm.resolve(&labels).unwrap())
            }
            Asm::STORE { rs2, rs1, imm } => {
                Psudo::STORE(*rs2 as u8, *rs1 as u8, imm.resolve(&labels).unwrap())
            }
            Asm::IF { rs2, imm } => Psudo::IF(*rs2 as u8, imm.resolve(&labels).unwrap()),
            Asm::IFR { rs2, imm } => Psudo::IFR(*rs2 as u8, imm.resolve(&labels).unwrap()),
            Asm::JUMP { imm } => Psudo::JUMP(imm.resolve(&labels).unwrap()),
            Asm::JUMPR { imm } => Psudo::JUMPR(imm.resolve(&labels).unwrap()),
            Asm::CALL { imm } => Psudo::CALL(imm.resolve(&labels).unwrap()),
            Asm::RET => Psudo::RET,
            Asm::IRET => Psudo::IRET,
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
    fn cformat(&self) -> String {
        match self {
            Imm::Label(str) => cformat!("<underline>{}</>", str),
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
