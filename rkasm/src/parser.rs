use color_print::cformat;
use rk16::Reg;
use std::{cell::Cell, collections::HashMap, num::ParseIntError};

use crate::message::Msg;

#[derive(Debug, Clone)]
pub struct Line<'a> {
    file: &'a String,
    idx: usize,
    raw: String,
}

impl<'a> Line<'a> {
    pub fn new(file: &'a String, idx: usize, raw: String) -> Self {
        Self { file, idx, raw }
    }
    pub fn pos(&self) -> String {
        format!("{}:{:0>4}", self.file, self.idx + 1)
    }
    pub fn line_no(&self) -> usize {
        self.idx + 1
    }
    pub fn raw(&'a self) -> &'a String {
        &self.raw
    }
}

// ----------------------------------------------------------------------------
// Code

#[derive(Debug)]
pub struct Code<'a> {
    pub line: &'a Line<'a>,
    code: &'a str,
    comment: Option<&'a str>,
    pub stmt: Option<Stmt>,
}

impl<'a> Code<'a> {
    pub fn parse(line: &'a Line, pc: u16) -> (Code<'a>, Vec<Msg<'a>>) {
        let (code, comment) = line.raw.split_once(";").unwrap_or((&line.raw, ""));
        let (stmt, msgs) = Stmt::parse(&code, line, pc);
        return (
            Code {
                line,
                code,
                comment: if comment == "" { None } else { Some(comment) },
                stmt,
            },
            msgs,
        );
    }
}

impl Code<'_> {
    pub fn cformat(&self) -> String {
        let comment = match self.comment {
            Some(s) => format!(" ;{}", s),
            None => format!(""),
        };

        let line = self.line.line_no();

        let binary = {
            if let Some(Stmt::Op { pc: _, bin, op: _ }) = &self.stmt {
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
            if let Some(Stmt::Op { pc, .. }) = &self.stmt {
                cformat!("<green>{:>4X}</>", pc)
            } else {
                " ".repeat(4).to_string()
            }
        };

        let stmt = {
            match &self.stmt {
                Some(stmt) => stmt.cformat(),
                None => "".to_string(),
            }
        };

        let file = if line == 1 {
            let line = "+-----+------+-------------+-----------------------------+";
            format!("{}\n| {:<54} |\n{}\n", line, self.line.file, line)
        } else {
            "".to_string()
        };

        format!(
            "{}| {:>4}| {} | {} | {}{}",
            file, line, pc, binary, stmt, comment
        )
    }
}

// ----------------------------------------------------------------------------
// Statement

#[derive(Debug)]
pub enum Stmt {
    Err,
    Op {
        pc: u16,
        bin: Cell<Option<u32>>,
        op: Op,
    },
    Label(Label),
}

impl Stmt {
    fn parse<'a>(code: &str, line: &'a Line, pc: u16) -> (Option<Stmt>, Vec<Msg<'a>>) {
        let words: Vec<&str> = code.split_whitespace().collect();
        if words.len() == 0 {
            return (None, vec![]);
        }

        match Label::parse(code, pc) {
            Some(lab) => return (Some(Stmt::Label(lab)), vec![]),
            None => {}
        };

        // Operation
        match Op::parse(code, line) {
            Ok(op) => {
                return (
                    Some(Stmt::Op {
                        pc,
                        bin: Cell::new(None),
                        op,
                    }),
                    vec![],
                )
            }
            Err(msg) => return (Some(Stmt::Err), vec![Msg::error(msg, line)]),
        };
    }
}

impl Stmt {
    fn cformat(&self) -> String {
        match self {
            Stmt::Err => cformat!("<red,bold>! ERROR</>"),
            Stmt::Op { pc, bin, op } => op.cformat(),
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

#[derive(Debug)]
pub enum Op {
    // kind:OpKind,
    // rs1:Option<Reg>,
    // rs2:Option<Reg>,
    // rd:Option<Reg>,
    // imm:Option<Imm>,
    // Calculation Operations
    Add { rd: Reg, rs1: Reg, rs2: Reg },
    Sub { rd: Reg, rs1: Reg, rs2: Reg },
    And { rd: Reg, rs1: Reg, rs2: Reg },
    Or { rd: Reg, rs1: Reg, rs2: Reg },
    Xor { rd: Reg, rs1: Reg, rs2: Reg },

    Eq { rd: Reg, rs1: Reg, rs2: Reg },
    Neq { rd: Reg, rs1: Reg, rs2: Reg },
    Lt { rd: Reg, rs1: Reg, rs2: Reg },
    Lts { rd: Reg, rs1: Reg, rs2: Reg },

    Sr { rd: Reg, rs1: Reg },
    Srs { rd: Reg, rs1: Reg },
    Srr { rd: Reg, rs1: Reg },
    Sl { rd: Reg, rs1: Reg },
    Slr { rd: Reg, rs1: Reg },

    Nop,
    Mov { rd: Reg, rs1: Reg },

    Addi { rd: Reg, rs1: Reg, imm: Imm },
    Subi { rd: Reg, rs1: Reg, imm: Imm },
    Andi { rd: Reg, rs1: Reg, imm: Imm },
    Ori { rd: Reg, rs1: Reg, imm: Imm },
    Xori { rd: Reg, rs1: Reg, imm: Imm },

    Eqi { rd: Reg, rs1: Reg, imm: Imm },
    Neqi { rd: Reg, rs1: Reg, imm: Imm },
    Lti { rd: Reg, rs1: Reg, imm: Imm },
    Ltsi { rd: Reg, rs1: Reg, imm: Imm },

    Not { rd: Reg, rs1: Reg },
    Loadi { rd: Reg, imm: Imm },

    // Memory Operations
    Load { rd: Reg, rs1: Reg, imm: Imm },
    Store { rs2: Reg, rs1: Reg, imm: Imm },

    // Controll Operations
    If { rs2: Reg, imm: Imm },
    Ifr { rs2: Reg, imm: Imm },
    Jump { imm: Imm },
    Jumpr { imm: Imm },
    Call { imm: Imm },
    Ret,
    Iret,
}

// #[derive(Debug)]

// pub struct Op {
//     kind: OpKind,
//     rs1: Option<Reg>,
//     rs2: Option<Reg>,
//     rd: Option<Reg>,
//     imm: Option<Imm>,
// }

#[derive(Debug)]
pub enum OpKind {
    // Calculation Operations
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

    // Memory Operations
    Load,
    Store,

    // Controll Operations
    If,
    Ifr,
    Jump,
    Jumpr,
    Call,
    Ret,
    Iret,
}

impl Op {
    fn parse(code: &str, line: &Line) -> Result<Op, String> {
        if let Some((op, args)) = code.split_whitespace().collect::<Vec<_>>().split_first() {
            let op: &str = op.to_owned();
            match op {
                // Calculation Operations

                // []
                "nop" => return Ok(Op::Nop),

                // [rd, rs1]
                "sr" | "srs" | "srr" | "sl" | "slr" | "mov" | "not" => {
                    if let Some([ref rd, ref rs1]) = args.get(0..2) {
                        let rd = Reg::parse(rd)?;
                        let rs1 = Reg::parse(rs1)?;
                        return match op {
                            "sr" => Ok(Op::Sr { rd, rs1 }),
                            "srs" => Ok(Op::Srs { rd, rs1 }),
                            "srr" => Ok(Op::Srr { rd, rs1 }),
                            "sl" => Ok(Op::Sl { rd, rs1 }),
                            "slr" => Ok(Op::Slr { rd, rs1 }),
                            "mov" => Ok(Op::Mov { rd, rs1 }),
                            "not" => Ok(Op::Not { rd, rs1 }),
                            _ => unreachable!(),
                        };
                    } else {
                        return Err(format!("Invalid operands: expected [rd rs1]"));
                    }
                }

                // [rd, rs1, rs2]
                "add" | "sub" | "and" | "or" | "xor" | "eq" | "neq" | "lt" | "lts" => {
                    if let Some([ref rd, ref rs1, ref rs2]) = args.get(0..3) {
                        let rd = Reg::parse(rd)?;
                        let rs1 = Reg::parse(rs1)?;
                        let rs2 = Reg::parse(rs2)?;
                        return match op {
                            "add" => Ok(Op::Add { rd, rs1, rs2 }),
                            "sub" => Ok(Op::Sub { rd, rs1, rs2 }),
                            "and" => Ok(Op::And { rd, rs1, rs2 }),
                            "or" => Ok(Op::Or { rd, rs1, rs2 }),
                            "xor" => Ok(Op::Xor { rd, rs1, rs2 }),
                            "eq" => Ok(Op::Eq { rd, rs1, rs2 }),
                            "neq" => Ok(Op::Neq { rd, rs1, rs2 }),
                            "lt" => Ok(Op::Lt { rd, rs1, rs2 }),
                            "lts" => Ok(Op::Lts { rd, rs1, rs2 }),
                            _ => unreachable!(),
                        };
                    } else {
                        return Err(format!("Invalid operands: expected [rd rs1 rs2]"));
                    }
                }

                // [rd, imm]
                "loadi" => {
                    if let Some([ref rd, ref imm]) = args.get(0..2) {
                        let rd = Reg::parse(rd)?;
                        let imm = Imm::parse(imm)?;
                        return Ok(Op::Loadi { rd, imm });
                    } else {
                        return Err(format!("Invalid operands: expected [rd imm]"));
                    }
                }

                // [rd, rs1, imm]
                "addi" | "subi" | "andi" | "ori" | "xori" | "eqi" | "neqi" | "lti" | "ltsi" => {
                    if let Some([ref rd, ref rs1, ref imm]) = args.get(0..3) {
                        let rd = Reg::parse(rd)?;
                        let rs1 = Reg::parse(rs1)?;
                        let imm = Imm::parse(imm)?;
                        return match op {
                            "addi" => Ok(Op::Addi { rd, rs1, imm }),
                            "subi" => Ok(Op::Subi { rd, rs1, imm }),
                            "andi" => Ok(Op::Andi { rd, rs1, imm }),
                            "ori" => Ok(Op::Ori { rd, rs1, imm }),
                            "xori" => Ok(Op::Xori { rd, rs1, imm }),
                            "eqi" => Ok(Op::Eqi { rd, rs1, imm }),
                            "neqi" => Ok(Op::Neqi { rd, rs1, imm }),
                            "lti" => Ok(Op::Lti { rd, rs1, imm }),
                            "ltsi" => Ok(Op::Ltsi { rd, rs1, imm }),
                            _ => unreachable!(),
                        };
                    } else {
                        return Err(format!("Invalid operands: expected [rd rs1 imm]"));
                    }
                }

                //--------------------------------------
                // Memory Operations
                "load" => {
                    if let Some([ref rd, ref rs1, ref imm]) = args.get(0..3) {
                        let rd = Reg::parse(rd)?;
                        let rs1 = Reg::parse(rs1)?;
                        let imm = Imm::parse(imm)?;
                        return Ok(Op::Load { rd, rs1, imm });
                    } else {
                        return Err(format!("Invalid operands: expected [rd rs1 imm]"));
                    }
                }
                "store" => {
                    if let Some([ref rs2, ref rs1, ref imm]) = args.get(0..3) {
                        let rs2 = Reg::parse(rs2)?;
                        let rs1 = Reg::parse(rs1)?;
                        let imm = Imm::parse(imm)?;
                        return Ok(Op::Store { rs2, rs1, imm });
                    } else {
                        return Err(format!("Invalid operands: expected [rs2 rs1 imm]"));
                    }
                }

                //--------------------------------------
                // Controll Operations
                "if" | "ifr" => {
                    if let Some([ref rs2, ref imm]) = args.get(0..2) {
                        let rs2 = Reg::parse(rs2)?;
                        let imm = Imm::parse(imm)?;
                        match op {
                            "if" => return Ok(Op::If { rs2, imm }),
                            "ifr" => return Ok(Op::Ifr { rs2, imm }),
                            _ => unreachable!(),
                        }
                    } else {
                        return Err(format!("Invalid operands: expected [rs2 imm]"));
                    }
                }
                "jump" | "jumpr" | "call" => {
                    if let Some([ref imm]) = args.get(0..1) {
                        let imm = Imm::parse(imm)?;
                        match op {
                            "jump" => return Ok(Op::Jump { imm }),
                            "jumpr" => return Ok(Op::Jumpr { imm }),
                            "call" => return Ok(Op::Call { imm }),
                            _ => unreachable!(),
                        }
                    } else {
                        return Err(format!("Invalid operands: expected [imm]"));
                    }
                }
                "ret" => return Ok(Op::Ret),
                "iret" => return Ok(Op::Iret),

                _ => return Err(format!("Unknown operation: `{}`", op)),
            };
        }
        return Err(format!("Unknown Error: Cannot parse as Op"));
    }
}

impl Op {
    fn cformat(&self) -> String {
        let format_opt_reg = |r: Option<&Reg>| match r {
            Some(a) => a.format(),
            None => "".to_string(),
        };
        let cformat_rrr = |op: &str, r1: Option<&Reg>, r2: Option<&Reg>, r3: Option<&Reg>| {
            cformat!(
                "  <red>{:<6}</><blue>{:<6}{:<6}{:<8}</>",
                op,
                format_opt_reg(r1),
                format_opt_reg(r2),
                format_opt_reg(r3)
            )
        };
        let cformat_rri = |op: &str, r1: Option<&Reg>, r2: Option<&Reg>, imm: &Imm| {
            cformat!(
                "  <red>{:<6}</><blue>{:<6}{:<6}</>{:<18}",
                op,
                format_opt_reg(r1),
                format_opt_reg(r2),
                imm.cformat()
            )
        };
        match self {
            Op::Add { rd, rs1, rs2 } => cformat_rrr("add", Some(rd), Some(rs1), Some(rs2)),
            Op::Sub { rd, rs1, rs2 } => cformat_rrr("sub", Some(rd), Some(rs1), Some(rs2)),
            Op::And { rd, rs1, rs2 } => cformat_rrr("and", Some(rd), Some(rs1), Some(rs2)),
            Op::Or { rd, rs1, rs2 } => cformat_rrr("or", Some(rd), Some(rs1), Some(rs2)),
            Op::Xor { rd, rs1, rs2 } => cformat_rrr("xor", Some(rd), Some(rs1), Some(rs2)),
            Op::Eq { rd, rs1, rs2 } => cformat_rrr("eq", Some(rd), Some(rs1), Some(rs2)),
            Op::Neq { rd, rs1, rs2 } => cformat_rrr("neq", Some(rd), Some(rs1), Some(rs2)),
            Op::Lt { rd, rs1, rs2 } => cformat_rrr("lt", Some(rd), Some(rs1), Some(rs2)),
            Op::Lts { rd, rs1, rs2 } => cformat_rrr("lts", Some(rd), Some(rs1), Some(rs2)),

            Op::Sr { rd, rs1: rs } => cformat_rrr("sr", Some(rd), Some(rs), None),
            Op::Srs { rd, rs1: rs } => cformat_rrr("srs", Some(rd), Some(rs), None),
            Op::Srr { rd, rs1: rs } => cformat_rrr("srr", Some(rd), Some(rs), None),
            Op::Sl { rd, rs1: rs } => cformat_rrr("sl", Some(rd), Some(rs), None),
            Op::Slr { rd, rs1: rs } => cformat_rrr("slr", Some(rd), Some(rs), None),

            Op::Nop => cformat_rrr("nop", None, None, None),
            Op::Mov { rd, rs1: rs } => cformat_rrr("mov", Some(rd), Some(rs), None),

            Op::Addi { rd, rs1, imm } => cformat_rri("addi", Some(rd), Some(rs1), imm),
            Op::Subi { rd, rs1, imm } => cformat_rri("subi", Some(rd), Some(rs1), imm),
            Op::Andi { rd, rs1, imm } => cformat_rri("andi", Some(rd), Some(rs1), imm),
            Op::Ori { rd, rs1, imm } => cformat_rri("ori", Some(rd), Some(rs1), imm),
            Op::Xori { rd, rs1, imm } => cformat_rri("xori", Some(rd), Some(rs1), imm),
            Op::Eqi { rd, rs1, imm } => cformat_rri("eqi", Some(rd), Some(rs1), imm),
            Op::Neqi { rd, rs1, imm } => cformat_rri("neqi", Some(rd), Some(rs1), imm),
            Op::Lti { rd, rs1, imm } => cformat_rri("lti", Some(rd), Some(rs1), imm),
            Op::Ltsi { rd, rs1, imm } => cformat_rri("ltsi", Some(rd), Some(rs1), imm),

            Op::Not { rd, rs1 } => cformat_rrr("not", Some(rd), Some(rs1), None),
            Op::Loadi { rd, imm } => cformat_rri("loadi", Some(rd), None, imm),

            Op::Load { rd, rs1, imm } => cformat_rri("load", Some(rd), Some(rs1), imm),
            Op::Store { rs2, rs1, imm } => cformat_rri("store", Some(rs2), Some(rs1), imm),

            Op::Jump { imm } => cformat_rri("jump", None, None, imm),
            Op::Jumpr { imm } => cformat_rri("jumpr", None, None, imm),
            Op::If { rs2, imm } => cformat_rri("if", Some(rs2), None, imm),
            Op::Ifr { rs2, imm } => cformat_rri("ifr", Some(rs2), None, imm),
            Op::Call { imm } => cformat_rri("call", None, None, imm),
            Op::Ret => cformat_rrr("ret", None, None, None),
            Op::Iret => cformat_rrr("iret", None, None, None),
        }
    }
}

// ----------------------------------------------------------------------------
// Immidiate

#[derive(Debug)]
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

impl Code<'_> {
    pub fn resolve(&self, labels: &HashMap<String, (&Line, &Label, u16)>) {
        if let Some(stmt) = &self.stmt {
            if let Stmt::Op { pc, bin, op } = stmt {
                match op {
                    Op::Addi { imm, .. }
                    | Op::Subi { imm, .. }
                    | Op::Andi { imm, .. }
                    | Op::Ori { imm, .. } => imm.resolve(labels),
                    _ => {}
                };
            }
        }
    }
}

impl Imm {
    fn resolve(&self, labels: &HashMap<String, (&Line, &Label, u16)>) {
        match self.kind.get() {
            ImmKind::Unknown => {
                if let Some((line, lab, val)) = labels.get(&self.label) {
                    match lab.kind {
                        LabelKind::Code => self.kind.set(ImmKind::OprLab),
                        LabelKind::Addr => self.kind.set(ImmKind::AddrLab),
                        LabelKind::Const => self.kind.set(ImmKind::ConstLab),
                    }
                    self.value.set(*val);
                }
            }
            _ => {}
        }
    }
}

// ----------------------------------------------------------------------------
// Generate Binary

impl Code<'_> {
    pub fn generate_bin(&self) {
        if let Some(stmt) = &self.stmt {
            if let Stmt::Op { pc, bin, op } = stmt {
                bin.set(Some(*pc as u32));
            }
        }
    }
}
