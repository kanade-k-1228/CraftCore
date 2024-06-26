use color_print::cformat;
use rk16::{alu::Alu, op::OpKind, reg::Reg};
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
    pub code: &'a str,
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
            Some(s) => format!(";{}", s),
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
            LabelKind::Addr => cformat!("<blue>{:04X} = {:}</>", self.val, self.key),
            LabelKind::Const => cformat!("<yellow>{:04X} = {:}</>", self.val, self.key),
        }
    }
}

// ----------------------------------------------------------------------------
// Operation

#[derive(Debug, Default)]
pub struct Op {
    kind: OpKind,
    rs1: Option<Reg>,
    rs2: Option<Reg>,
    rd: Option<Reg>,
    imm: Option<Imm>,
}
pub enum Ops {
    None,
    Calc {
        fnct: Alu,
        rs1: Reg,
        rs2: Reg,
        rd: Reg,
    },
    Calci {
        rs1: Reg,
        rd: Reg,
        imm: Imm,
    },
}

impl Op {
    fn parse(code: &str, _line: &Line) -> Result<Op, String> {
        let toks = code.split_whitespace().collect::<Vec<_>>();

        // Mathmatical Style
        if let Some(eq) = toks.get(1) {
            if *eq == "=" {
                println!("Mathmatical Style")
            }
        }

        // Operational Style
        if let Some((op, args)) = toks.split_first() {
            let op: &str = op.to_owned();
            if let Ok(kind) = OpKind::parse(op) {
                match kind {
                    // []
                    OpKind::Nop | OpKind::Ret | OpKind::Iret => {
                        return Ok(Op {
                            kind: kind,
                            ..Default::default()
                        })
                    }

                    // [rd, rs1]
                    OpKind::Sr
                    | OpKind::Srs
                    | OpKind::Srr
                    | OpKind::Sl
                    | OpKind::Slr
                    | OpKind::Mov
                    | OpKind::Not => {
                        if let Some([ref rd, ref rs1]) = args.get(0..2) {
                            let rd = Reg::parse(rd)?;
                            let rs1 = Reg::parse(rs1)?;
                            return Ok(Op {
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
                    OpKind::Add
                    | OpKind::Sub
                    | OpKind::And
                    | OpKind::Or
                    | OpKind::Xor
                    | OpKind::Eq
                    | OpKind::Neq
                    | OpKind::Lt
                    | OpKind::Lts => {
                        if let Some([ref rd, ref rs1, ref rs2]) = args.get(0..3) {
                            let rd = Reg::parse(rd)?;
                            let rs1 = Reg::parse(rs1)?;
                            let rs2 = Reg::parse(rs2)?;
                            return Ok(Op {
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
                    OpKind::Addi
                    | OpKind::Subi
                    | OpKind::Andi
                    | OpKind::Ori
                    | OpKind::Xori
                    | OpKind::Eqi
                    | OpKind::Neqi
                    | OpKind::Lti
                    | OpKind::Ltsi
                    | OpKind::Load => {
                        if let Some([ref rd, ref rs1, ref imm]) = args.get(0..3) {
                            let rd = Reg::parse(rd)?;
                            let rs1 = Reg::parse(rs1)?;
                            let imm = Imm::parse(imm)?;
                            return Ok(Op {
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
                    OpKind::Loadi => {
                        if let Some([ref rd, ref imm]) = args.get(0..2) {
                            let rd = Reg::parse(rd)?;
                            let imm = Imm::parse(imm)?;
                            return Ok(Op {
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
                    OpKind::Store => {
                        if let Some([ref rs2, ref rs1, ref imm]) = args.get(0..3) {
                            let rs2 = Reg::parse(rs2)?;
                            let rs1 = Reg::parse(rs1)?;
                            let imm = Imm::parse(imm)?;
                            return Ok(Op {
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
                    OpKind::If | OpKind::Ifr => {
                        if let Some([ref rs2, ref imm]) = args.get(0..2) {
                            let rs2 = Reg::parse(rs2)?;
                            let imm = Imm::parse(imm)?;
                            return Ok(Op {
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
                    OpKind::Jump | OpKind::Jumpr | OpKind::Call => {
                        if let Some([ref imm]) = args.get(0..1) {
                            let imm = Imm::parse(imm)?;
                            return Ok(Op {
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

impl Op {
    fn cformat(&self) -> String {
        let format_opt_reg = |r: &Option<Reg>| match r {
            Some(a) => a.format(),
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
            OpKind::Add => cformat_rrr("add", &self.rd, &self.rs1, &self.rs2),
            OpKind::Sub => cformat_rrr("sub", &self.rd, &self.rs1, &self.rs2),
            OpKind::And => cformat_rrr("and", &self.rd, &self.rs1, &self.rs2),
            OpKind::Or => cformat_rrr("or", &self.rd, &self.rs1, &self.rs2),
            OpKind::Xor => cformat_rrr("xor", &self.rd, &self.rs1, &self.rs2),
            OpKind::Eq => cformat_rrr("eq", &self.rd, &self.rs1, &self.rs2),
            OpKind::Neq => cformat_rrr("neq", &self.rd, &self.rs1, &self.rs2),
            OpKind::Lt => cformat_rrr("lt", &self.rd, &self.rs1, &self.rs2),
            OpKind::Lts => cformat_rrr("lts", &self.rd, &self.rs1, &self.rs2),

            OpKind::Sr => cformat_rrr("sr", &self.rd, &self.rs1, &None),
            OpKind::Srs => cformat_rrr("srs", &self.rd, &self.rs1, &None),
            OpKind::Srr => cformat_rrr("srr", &self.rd, &self.rs1, &None),
            OpKind::Sl => cformat_rrr("sl", &self.rd, &self.rs1, &None),
            OpKind::Slr => cformat_rrr("slr", &self.rd, &self.rs1, &None),

            OpKind::Nop => cformat_rrr("nop", &None, &None, &None),
            OpKind::Mov => cformat_rrr("mov", &self.rd, &self.rs1, &None),

            OpKind::Addi => cformat_rri("addi", &self.rd, &self.rs1, &self.imm),
            OpKind::Subi => cformat_rri("subi", &self.rd, &self.rs1, &self.imm),
            OpKind::Andi => cformat_rri("andi", &self.rd, &self.rs1, &self.imm),
            OpKind::Ori => cformat_rri("ori", &self.rd, &self.rs1, &self.imm),
            OpKind::Xori => cformat_rri("xori", &self.rd, &self.rs1, &self.imm),
            OpKind::Eqi => cformat_rri("eqi", &self.rd, &self.rs1, &self.imm),
            OpKind::Neqi => cformat_rri("neqi", &self.rd, &self.rs1, &self.imm),
            OpKind::Lti => cformat_rri("lti", &self.rd, &self.rs1, &self.imm),
            OpKind::Ltsi => cformat_rri("ltsi", &self.rd, &self.rs1, &self.imm),

            OpKind::Not => cformat_rrr("not", &self.rd, &self.rs1, &None),
            OpKind::Loadi => cformat_rri("loadi", &self.rd, &None, &self.imm),

            OpKind::Load => cformat_rri("load", &self.rd, &self.rs1, &self.imm),
            OpKind::Store => cformat_rri("store", &self.rs2, &self.rs1, &self.imm),

            OpKind::If => cformat_rri("if", &self.rs2, &None, &self.imm),
            OpKind::Ifr => cformat_rri("ifr", &self.rs2, &None, &self.imm),
            OpKind::Jump => cformat_rri("jump", &None, &None, &self.imm),
            OpKind::Jumpr => cformat_rri("jumpr", &None, &None, &self.imm),
            OpKind::Call => cformat_rri("call", &None, &None, &self.imm),
            OpKind::Ret => cformat_rrr("ret", &None, &None, &None),
            OpKind::Iret => cformat_rrr("iret", &None, &None, &None),
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
    Lit,
    Unknown,
    RomAddr,
    Const,
    RamAddr,
}

impl Imm {
    fn parse(s: &str) -> Result<Imm, String> {
        if let Ok(value) = parse_with_prefix(s) {
            return Ok(Imm {
                kind: Cell::new(ImmKind::Lit),
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
            ImmKind::Lit => cformat!("<yellow>0x{:0>4X}</>", self.value.get()),
            ImmKind::Unknown => cformat!("<underline>{:16}</> ", self.label),
            ImmKind::RomAddr => {
                cformat!("<green>0x{:0>4X} = {:8}</>", self.value.get(), self.label)
            }
            ImmKind::Const => cformat!("<blue>0x{:0>4X} = {:8}</>", self.value.get(), self.label),
            ImmKind::RamAddr => cformat!("<blue>0x{:0>4X} = {:8}</>", self.value.get(), self.label),
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
    pub fn resolve(&self, labels: &HashMap<String, (&Line, &Label, u16)>) -> Vec<Msg> {
        if let Some(stmt) = &self.stmt {
            if let Stmt::Op { op, .. } = stmt {
                if let Some(imm) = &op.imm {
                    let err = imm.resolve(labels);
                    return err
                        .iter()
                        .map(|e| Msg::error(e.to_string(), self.line))
                        .collect();
                }
            }
        }
        vec![]
    }
}

impl Imm {
    fn resolve(&self, labels: &HashMap<String, (&Line, &Label, u16)>) -> Vec<String> {
        match self.kind.get() {
            ImmKind::Unknown => {
                if let Some((_line, lab, val)) = labels.get(&self.label) {
                    match lab.kind {
                        LabelKind::Code => self.kind.set(ImmKind::RomAddr),
                        LabelKind::Addr => self.kind.set(ImmKind::RamAddr),
                        LabelKind::Const => self.kind.set(ImmKind::Const),
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

impl Code<'_> {
    pub fn generate_bin(&self) {
        if let Some(stmt) = &self.stmt {
            if let Stmt::Op { pc: _, bin, op } = stmt {
                bin.set(Some(match op.kind {
                    OpKind::Add => field(
                        Alu::Add.into(),
                        op.rd.unwrap().into(),
                        op.rs2.unwrap().into(),
                        op.rs1.unwrap().into(),
                        0,
                    ),
                    OpKind::Sub => field(Alu::Sub.into(), 0, 0, 0, 0),
                    _ => 0,
                }));
            }
        }
    }
}
