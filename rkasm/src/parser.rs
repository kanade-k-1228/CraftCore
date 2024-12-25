use arch::{op::OpKind, reg::Reg};
use color_print::cformat;
use std::{
    cell::{Cell, OnceCell},
    num::ParseIntError,
};

use crate::{label::Labels, msg::Msgs};

// ----------------------------------------------------------------------------
// Line

#[derive(Debug, Clone)]
pub struct Line {
    path: String,
    idx: usize,
    raw: String,
    code: String,
    comment: Option<String>,
    pub stmt: OnceCell<Option<Stmt>>,
}

impl Line {
    pub fn new(path: &str, idx: usize, str: &str) -> Self {
        let (code, comment) = match str.split_once(";") {
            Some((code, comment)) => (code.to_string(), Some(comment.to_string())),
            None => (str.to_string(), None),
        };
        Self {
            path: path.to_string(),
            idx,
            raw: str.to_string(),
            code,
            comment,
            stmt: OnceCell::new(),
        }
    }
    pub fn pos(&self) -> String {
        format!("{}:{:0>4}", self.path, self.idx + 1)
    }
    pub fn no(&self) -> usize {
        self.idx + 1
    }
    pub fn raw(&self) -> String {
        self.raw.clone()
    }
}

impl Line {
    pub fn parse(&self, pc: u16) -> Msgs {
        let (stmt, msgs) = Stmt::parse(&self, pc);
        self.stmt.set(stmt).unwrap();
        return msgs;
    }
}

impl Line {
    pub fn cformat(&self) -> String {
        let stmt = self.stmt.get().unwrap();

        let comment = match &self.comment {
            Some(s) => format!(" ;{}", s),
            None => format!(""),
        };

        let line = self.no();

        let binary = {
            if let Some(Stmt::Op { pc: _, bin, op: _ }) = &stmt {
                if let Some(bin) = bin.get() {
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
            } else {
                " ".repeat(11).to_string()
            }
        };

        let pc = {
            if let Some(Stmt::Op { pc, .. }) = &stmt {
                cformat!("<green>{:0>4X}</>", pc)
            } else {
                " ".repeat(4).to_string()
            }
        };

        let stmt = {
            match &stmt {
                Some(stmt) => stmt.cformat(),
                None => "".to_string(),
            }
        };

        let file = if line == 1 {
            let line = "+------+------+-------------+-----------------------------+";
            format!("{}\n| {:<55} |\n{}\n", line, self.path, line)
        } else {
            "".to_string()
        };

        format!(
            "{}| {:>4} | {} | {} | {}{}",
            file, line, pc, binary, stmt, comment
        )
    }
}

// ----------------------------------------------------------------------------
// Statement

#[derive(Debug, Clone)]
pub enum Stmt {
    Err,
    Op {
        pc: u16,
        op: UnresolvedOp,
        bin: Cell<Option<u32>>,
    },
    Label(Label),
}

impl Stmt {
    fn parse(line: &Line, pc: u16) -> (Option<Stmt>, Msgs) {
        let mut msgs = Msgs::new();

        let words: Vec<&str> = line.code.split_whitespace().collect();

        if words.len() == 0 {
            return (None, msgs);
        }

        match Label::parse(&line.code, pc) {
            Some(lab) => return (Some(Stmt::Label(lab)), msgs),
            None => {}
        };

        // Operation
        match UnresolvedOp::parse(&line) {
            Ok(op) => {
                return (
                    Some(Stmt::Op {
                        pc,
                        bin: Cell::new(None),
                        op,
                    }),
                    msgs,
                )
            }
            Err(msg) => {
                msgs.error(msg, line.clone());
                return (Some(Stmt::Err), msgs);
            }
        };
    }
}

impl Stmt {
    fn cformat(&self) -> String {
        match self {
            Stmt::Err => cformat!("<red,bold>! ERROR</>"),
            Stmt::Op { pc: _, bin: _, op } => op.cformat(),
            Stmt::Label(label) => label.cformat(),
        }
    }
}

// ----------------------------------------------------------------------------
// Label

#[derive(Debug, Clone)]
pub struct Label {
    kind: LabelKind,
    pub key: String,
    pub val: u16,
}

#[derive(Debug, Clone)]

pub enum LabelKind {
    Code,
    Addr,
    Const,
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
                            return Some(Label {
                                kind: LabelKind::Addr,
                                key,
                                val,
                            });
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
                            return Some(Label {
                                kind: LabelKind::Const,
                                key,
                                val,
                            });
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
                            return Some(Label {
                                kind: LabelKind::Code,
                                key,
                                val: pc,
                            });
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
        match self.kind {
            LabelKind::Code => cformat!("<green>{}:{:04X}</>", self.key, self.val),
            LabelKind::Addr => cformat!("<blue>{:04X} = {}</>", self.val, self.key),
            LabelKind::Const => cformat!("<yellow>{:04X} = {}</>", self.val, self.key),
        }
    }
}

// ----------------------------------------------------------------------------
// Operation

#[derive(Debug, Default, Clone)]
pub struct UnresolvedOp {
    kind: OpKind,
    rs1: Option<Reg>,
    rs2: Option<Reg>,
    rd: Option<Reg>,
    imm: Option<Imm>,
}

impl UnresolvedOp {
    fn parse(line: &Line) -> Result<UnresolvedOp, String> {
        if let Some((op, args)) = line
            .code
            .split_whitespace()
            .collect::<Vec<_>>()
            .split_first()
        {
            let op: &str = op.to_owned();
            if let Ok(kind) = OpKind::parse(op) {
                match kind {
                    // []
                    OpKind::NOP | OpKind::RET | OpKind::IRET => {
                        return Ok(UnresolvedOp {
                            kind: kind,
                            ..Default::default()
                        })
                    }

                    // [rd, rs1]
                    OpKind::SR
                    | OpKind::SRS
                    | OpKind::SRR
                    | OpKind::SL
                    | OpKind::SLR
                    | OpKind::MOV
                    | OpKind::NOT => {
                        if let Some([ref rd, ref rs1]) = args.get(0..2) {
                            let rd = Reg::parse(rd)?;
                            let rs1 = Reg::parse(rs1)?;
                            return Ok(UnresolvedOp {
                                kind: kind,
                                rd: Some(rd),
                                rs1: Some(rs1),
                                ..Default::default()
                            });
                        } else {
                            return Err(format!("Invalid operands: expected [rd rs1]"));
                        }
                    }

                    // [rd, rs1, rs2]
                    OpKind::ADD
                    | OpKind::SUB
                    | OpKind::AND
                    | OpKind::OR
                    | OpKind::XOR
                    | OpKind::EQ
                    | OpKind::NEQ
                    | OpKind::LT
                    | OpKind::LTS => {
                        if let Some([ref rd, ref rs1, ref rs2]) = args.get(0..3) {
                            let rd = Reg::parse(rd)?;
                            let rs1 = Reg::parse(rs1)?;
                            let rs2 = Reg::parse(rs2)?;
                            return Ok(UnresolvedOp {
                                kind: kind,
                                rd: Some(rd),
                                rs1: Some(rs1),
                                rs2: Some(rs2),
                                ..Default::default()
                            });
                        } else {
                            return Err(format!("Invalid operands: expected [rd rs1 rs2]"));
                        }
                    }

                    // [rd, rs1, imm]
                    OpKind::ADDI
                    | OpKind::SUBI
                    | OpKind::ANDI
                    | OpKind::ORI
                    | OpKind::XORI
                    | OpKind::EQI
                    | OpKind::NEQI
                    | OpKind::LTI
                    | OpKind::LTSI
                    | OpKind::LOAD => {
                        if let Some([ref rd, ref rs1, ref imm]) = args.get(0..3) {
                            let rd = Reg::parse(rd)?;
                            let rs1 = Reg::parse(rs1)?;
                            let imm = Imm::parse(imm)?;
                            return Ok(UnresolvedOp {
                                kind: kind,
                                rd: Some(rd),
                                rs1: Some(rs1),
                                imm: Some(imm),
                                ..Default::default()
                            });
                        } else {
                            return Err(format!("Invalid operands: expected [rd rs1 imm]"));
                        }
                    }

                    // [rd, imm]
                    OpKind::LOADI => {
                        if let Some([ref rd, ref imm]) = args.get(0..2) {
                            let rd = Reg::parse(rd)?;
                            let imm = Imm::parse(imm)?;
                            return Ok(UnresolvedOp {
                                kind: kind,
                                rd: Some(rd),
                                imm: Some(imm),
                                ..Default::default()
                            });
                        } else {
                            return Err(format!("Invalid operands: expected [rd imm]"));
                        }
                    }

                    // [rs2, rs1, imm]
                    OpKind::STORE => {
                        if let Some([ref rs2, ref rs1, ref imm]) = args.get(0..3) {
                            let rs2 = Reg::parse(rs2)?;
                            let rs1 = Reg::parse(rs1)?;
                            let imm = Imm::parse(imm)?;
                            return Ok(UnresolvedOp {
                                kind: kind,
                                rs2: Some(rs2),
                                rs1: Some(rs1),
                                imm: Some(imm),
                                ..Default::default()
                            });
                        } else {
                            return Err(format!("Invalid operands: expected [rs2 rs1 imm]"));
                        }
                    }

                    // [rs2, imm]
                    OpKind::IF | OpKind::IFR => {
                        if let Some([ref rs2, ref imm]) = args.get(0..2) {
                            let rs2 = Reg::parse(rs2)?;
                            let imm = Imm::parse(imm)?;
                            return Ok(UnresolvedOp {
                                kind: kind,
                                rs2: Some(rs2),
                                imm: Some(imm),
                                ..Default::default()
                            });
                        } else {
                            return Err(format!("Invalid operands: expected [rs2 imm]"));
                        }
                    }

                    // [imm]
                    OpKind::JUMP | OpKind::JUMPR | OpKind::CALL => {
                        if let Some([ref imm]) = args.get(0..1) {
                            let imm = Imm::parse(imm)?;
                            return Ok(UnresolvedOp {
                                kind: kind,
                                imm: Some(imm),
                                ..Default::default()
                            });
                        } else {
                            return Err(format!("Invalid operands: expected [imm]"));
                        }
                    }
                }
            } else {
                return Err(format!("Unknown operation: `{}`", op));
            }
        }
        return Err(format!("Unknown Error: Cannot parse as Op"));
    }
}

impl UnresolvedOp {
    fn cformat(&self) -> String {
        let format_opt_reg = |r: &Option<Reg>| match r {
            Some(a) => a.to_string(),
            None => "".to_string(),
        };
        let cformat_rrr = |op: &str, r1: &Option<Reg>, r2: &Option<Reg>, r3: &Option<Reg>| {
            cformat!(
                "  <red>{:<6}</><blue>{:<6}{:<6}{:<8}</>",
                op,
                format_opt_reg(r1),
                format_opt_reg(r2),
                format_opt_reg(r3)
            )
        };
        let cformat_rri = |op: &str, r1: &Option<Reg>, r2: &Option<Reg>, imm: &Option<Imm>| {
            cformat!(
                "  <red>{:<6}</><blue>{:<6}{:<6}</>{:<18}",
                op,
                format_opt_reg(r1),
                format_opt_reg(r2),
                if let Some(imm) = imm {
                    imm.cformat()
                } else {
                    "".to_string()
                }
            )
        };
        match self.kind {
            OpKind::ADD => cformat_rrr("add", &self.rd, &self.rs1, &self.rs2),
            OpKind::SUB => cformat_rrr("sub", &self.rd, &self.rs1, &self.rs2),
            OpKind::AND => cformat_rrr("and", &self.rd, &self.rs1, &self.rs2),
            OpKind::OR => cformat_rrr("or", &self.rd, &self.rs1, &self.rs2),
            OpKind::XOR => cformat_rrr("xor", &self.rd, &self.rs1, &self.rs2),
            OpKind::EQ => cformat_rrr("eq", &self.rd, &self.rs1, &self.rs2),
            OpKind::NEQ => cformat_rrr("neq", &self.rd, &self.rs1, &self.rs2),
            OpKind::LT => cformat_rrr("lt", &self.rd, &self.rs1, &self.rs2),
            OpKind::LTS => cformat_rrr("lts", &self.rd, &self.rs1, &self.rs2),

            OpKind::SR => cformat_rrr("sr", &self.rd, &self.rs1, &None),
            OpKind::SRS => cformat_rrr("srs", &self.rd, &self.rs1, &None),
            OpKind::SRR => cformat_rrr("srr", &self.rd, &self.rs1, &None),
            OpKind::SL => cformat_rrr("sl", &self.rd, &self.rs1, &None),
            OpKind::SLR => cformat_rrr("slr", &self.rd, &self.rs1, &None),

            OpKind::NOP => cformat_rrr("nop", &None, &None, &None),
            OpKind::MOV => cformat_rrr("mov", &self.rd, &self.rs1, &None),

            OpKind::ADDI => cformat_rri("addi", &self.rd, &self.rs1, &self.imm),
            OpKind::SUBI => cformat_rri("subi", &self.rd, &self.rs1, &self.imm),
            OpKind::ANDI => cformat_rri("andi", &self.rd, &self.rs1, &self.imm),
            OpKind::ORI => cformat_rri("ori", &self.rd, &self.rs1, &self.imm),
            OpKind::XORI => cformat_rri("xori", &self.rd, &self.rs1, &self.imm),
            OpKind::EQI => cformat_rri("eqi", &self.rd, &self.rs1, &self.imm),
            OpKind::NEQI => cformat_rri("neqi", &self.rd, &self.rs1, &self.imm),
            OpKind::LTI => cformat_rri("lti", &self.rd, &self.rs1, &self.imm),
            OpKind::LTSI => cformat_rri("ltsi", &self.rd, &self.rs1, &self.imm),

            OpKind::NOT => cformat_rrr("not", &self.rd, &self.rs1, &None),
            OpKind::LOADI => cformat_rri("loadi", &self.rd, &None, &self.imm),

            OpKind::LOAD => cformat_rri("load", &self.rd, &self.rs1, &self.imm),
            OpKind::STORE => cformat_rri("store", &self.rs2, &self.rs1, &self.imm),

            OpKind::IF => cformat_rri("if", &self.rs2, &None, &self.imm),
            OpKind::IFR => cformat_rri("ifr", &self.rs2, &None, &self.imm),
            OpKind::JUMP => cformat_rri("jump", &None, &None, &self.imm),
            OpKind::JUMPR => cformat_rri("jumpr", &None, &None, &self.imm),
            OpKind::CALL => cformat_rri("call", &None, &None, &self.imm),
            OpKind::RET => cformat_rrr("ret", &None, &None, &None),
            OpKind::IRET => cformat_rrr("iret", &None, &None, &None),
        }
    }
}

// ----------------------------------------------------------------------------
// Immidiate

#[derive(Debug, Clone)]
pub struct Imm {
    kind: Cell<ImmKind>,
    label: String,
    value: Cell<u16>,
}

#[derive(Debug, Clone, Copy)]
enum ImmKind {
    Literal,
    Unknown,
    OprLab,
    ConstLab,
    AddrLab,
}

impl Imm {
    fn parse(s: &str) -> Result<Imm, String> {
        if let Ok(value) = parse_with_prefix(s) {
            return Ok(Imm {
                kind: Cell::new(ImmKind::Literal),
                label: s.to_string(),
                value: Cell::new(value),
            });
        };
        Ok(Imm {
            kind: Cell::new(ImmKind::Unknown),
            label: s.to_string(),
            value: Cell::new(0),
        })
    }
    fn cformat(&self) -> String {
        match self.kind.get() {
            ImmKind::Literal => cformat!("<yellow>0x{:0>4X}</>", self.value.get()),
            ImmKind::Unknown => cformat!("<underline>{}</>", self.label),
            ImmKind::OprLab => cformat!("<green>0x{:0>4X} = {}</>", self.value.get(), self.label),
            ImmKind::ConstLab => cformat!("<blue>0x{:0>4X} = {}</>", self.value.get(), self.label),
            ImmKind::AddrLab => cformat!("<blue>0x{:0>4X} = {}</>", self.value.get(), self.label),
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

// ----------------------------------------------------------------------------
// Resolve Label

impl Line {
    pub fn resolve(&self, labels: &Labels) -> Msgs {
        let mut msgs = Msgs::new();
        if let Some(Some(stmt)) = &self.stmt.get() {
            if let Stmt::Op { op, .. } = stmt {
                if let Some(imm) = &op.imm {
                    let err = imm.resolve(labels);
                    for e in err {
                        msgs.error(e, self.clone());
                    }
                }
            }
        }
        msgs
    }
}

impl Imm {
    fn resolve(&self, labels: &Labels) -> Vec<String> {
        match self.kind.get() {
            ImmKind::Unknown => {
                if let Some((_line, lab, val)) = labels.get(&self.label) {
                    match lab.kind {
                        LabelKind::Code => self.kind.set(ImmKind::OprLab),
                        LabelKind::Addr => self.kind.set(ImmKind::AddrLab),
                        LabelKind::Const => self.kind.set(ImmKind::ConstLab),
                    }
                    self.value.set(*val);
                } else {
                    return vec![format!("Undefined Label `{}`", self.label)];
                }
            }
            _ => {}
        };
        vec![]
    }
}

// ----------------------------------------------------------------------------
// Generate Binary

fn field(b_31_16: u16, b_15_12: u8, b_11_8: u8, b_7_4: u8, b_3_0: u8) -> u32 {
    (b_31_16 as u32 & 0xFF) << 16
        | (b_15_12 as u32 & 0xF) << 12
        | (b_11_8 as u32 & 0xF) << 8
        | (b_7_4 as u32 & 0xF) << 8
        | (b_3_0 as u32 & 0xF) << 8
}

impl Line {
    pub fn generate_bin(&self) -> Msgs {
        let msgs = Msgs::new();
        if let Some(Some(stmt)) = &self.stmt.get() {
            if let Stmt::Op { pc: _, bin, op } = stmt {
                bin.set(Some(match op.kind {
                    OpKind::ADD => field(0, 0, 0, 0, 0),
                    OpKind::SUB => field(1, 0, 0, 0, 0),
                    OpKind::AND => field(0, 0, 0, 0, 0),
                    OpKind::OR => field(0, 0, 0, 0, 0),
                    OpKind::XOR => field(0, 0, 0, 0, 0),
                    OpKind::EQ => field(0, 0, 0, 0, 0),
                    OpKind::NEQ => field(0, 0, 0, 0, 0),
                    OpKind::LT => field(0, 0, 0, 0, 0),
                    OpKind::LTS => field(0, 0, 0, 0, 0),
                    OpKind::SR => field(0, 0, 0, 0, 0),
                    OpKind::SRS => field(0, 0, 0, 0, 0),
                    OpKind::SRR => field(0, 0, 0, 0, 0),
                    OpKind::SL => field(0, 0, 0, 0, 0),
                    OpKind::SLR => field(0, 0, 0, 0, 0),
                    OpKind::NOP => field(0, 0, 0, 0, 0),
                    OpKind::MOV => field(0, 0, 0, 0, 0),
                    OpKind::ADDI => field(0, 0, 0, 0, 0),
                    OpKind::SUBI => field(0, 0, 0, 0, 0),
                    OpKind::ANDI => field(0, 0, 0, 0, 0),
                    OpKind::ORI => field(0, 0, 0, 0, 0),
                    OpKind::XORI => field(0, 0, 0, 0, 0),
                    OpKind::EQI => field(0, 0, 0, 0, 0),
                    OpKind::NEQI => field(0, 0, 0, 0, 0),
                    OpKind::LTI => field(0, 0, 0, 0, 0),
                    OpKind::LTSI => field(0, 0, 0, 0, 0),
                    OpKind::NOT => field(0, 0, 0, 0, 0),
                    OpKind::LOADI => field(0, 0, 0, 0, 0),
                    OpKind::LOAD => field(0, 0, 0, 0, 0),
                    OpKind::STORE => field(0, 0, 0, 0, 0),
                    OpKind::IF => field(0, 0, 0, 0, 0),
                    OpKind::IFR => field(0, 0, 0, 0, 0),
                    OpKind::JUMP => field(0, 0, 0, 0, 0),
                    OpKind::JUMPR => field(0, 0, 0, 0, 0),
                    OpKind::CALL => field(0, 0, 0, 0, 0),
                    OpKind::RET => field(0, 0, 0, 0, 0),
                    OpKind::IRET => field(0, 0, 0, 0, 0),
                }));
            }
        }
        msgs
    }
}
