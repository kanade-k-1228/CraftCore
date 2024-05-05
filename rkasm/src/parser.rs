use color_print::cformat;
use rk16::Reg;
use std::num::ParseIntError;

// ----------------------------------------------------------------------------
// Line

#[derive(Debug)]
pub struct Line {
    file: String,
    idx: usize,
    raw: String,
    code: String,
    comment: String,
    pub stmt: Option<Stmt>,
}

impl Line {
    pub fn print_pos(&self) -> String {
        format!("{}:{}", self.file, self.idx + 1)
    }
}

impl Line {
    pub fn parse(file: &str, idx: usize, line: &str) -> Line {
        let (code, comment) = line.split_once(";").unwrap_or((&line, ""));
        return Line {
            file: file.to_string(),
            idx: idx,
            raw: line.to_string(),
            code: code.to_string(),
            comment: comment.to_string(),
            stmt: Stmt::parse(&code),
        };
    }
}

impl Line {
    pub fn cprint(&self) -> String {
        let place = format!("{}:{:0>4}", self.file, self.idx + 1);
        let comment = if self.comment != "" {
            format!(";{}", self.comment)
        } else {
            "".to_string()
        };
        match &self.stmt {
            Some(stmt) => format!("{} | {:<35} {}", place, stmt.cprint(), comment),
            None => format!("{} | {}", place, comment),
        }
    }
}

// ----------------------------------------------------------------------------
// Statement

#[derive(Debug)]
pub enum Stmt {
    Op(Op),
    CodeLabel { label: String },
    AddrLabel { label: String, value: u16 },
    ConstLabel { label: String, value: u16 },
    Err(String),
}

impl Stmt {
    fn parse(code: &str) -> Option<Stmt> {
        let words: Vec<&str> = code.split_whitespace().collect();
        if let Some(key) = words.get(0) {
            if let Some(head) = key.chars().nth(0) {
                // @0x0123 hoge
                if head == '@' {
                    if let Some(label) = words.get(1) {
                        if let Some(value) = key.get(1..) {
                            let label = label.to_string();
                            let value = parse_with_prefix(value).unwrap();
                            return Some(Stmt::AddrLabel { label, value });
                        }
                    }
                }
                // #0x0123 const
                if head == '#' {
                    if let Some(label) = words.get(1) {
                        if let Some(value) = key.get(1..) {
                            let label = label.to_string();
                            let value = parse_with_prefix(value).unwrap();
                            return Some(Stmt::ConstLabel { label, value });
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
                            let label = label.to_string();
                            return Some(Stmt::CodeLabel { label });
                        }
                    }
                }
            }

            // Operation
            match Op::parse(code) {
                Ok(op) => return Some(Stmt::Op(op)),
                Err(e) => return Some(Stmt::Err(e)),
            };
        } else {
            return None;
        }
    }
}

impl Stmt {
    fn cprint(&self) -> String {
        match self {
            Stmt::Err(e) => cformat!("<red>! {:}</>", e),
            Stmt::Op(op) => op.cprint(),
            Stmt::CodeLabel { label } => cformat!("<green>{}:</>", label),
            Stmt::AddrLabel { label, value } => cformat!("<blue>{:04X} = {}</>", value, label),
            Stmt::ConstLabel { label, value } => cformat!("<yellow>{:04X} = {}</>", value, label),
        }
    }
}

// ----------------------------------------------------------------------------
// Label

#[derive(Debug)]
enum Label {
    Code { label: String, value: Option<u16> },
    Addr { label: String, value: u16 },
    Const { label: String, value: u16 },
}

// ----------------------------------------------------------------------------
// Operation

#[derive(Debug)]
pub enum Op {
    // Arith
    Nop,
    Mov { rd: Reg, rs: Reg },
    Add { rd: Reg, rs1: Reg, rs2: Reg },
    Sub { rd: Reg, rs1: Reg, rs2: Reg },
    And { rd: Reg, rs1: Reg, rs2: Reg },
    Or { rd: Reg, rs1: Reg, rs2: Reg },
    Xor { rd: Reg, rs1: Reg, rs2: Reg },
    Not { rd: Reg, rs: Reg },
    Srs { rd: Reg, rs: Reg },
    Sru { rd: Reg, rs: Reg },
    SL { rd: Reg, rs: Reg },
    Eq { rd: Reg, rs1: Reg, rs2: Reg },
    Ltu { rd: Reg, rs1: Reg, rs2: Reg },
    Lts { rd: Reg, rs1: Reg, rs2: Reg },
    Loadi { rd: Reg, imm: Imm },
    Addi { rd: Reg, rs1: Reg, imm: Imm },
    Subi { rd: Reg, rs1: Reg, imm: Imm },
    Andi { rd: Reg, rs1: Reg, imm: Imm },
    Ori { rd: Reg, rs1: Reg, imm: Imm },
    Xori { rd: Reg, rs1: Reg, imm: Imm },
    Eqi { rd: Reg, rs1: Reg, imm: Imm },
    Ltsi { rd: Reg, rs1: Reg, imm: Imm },
    Ltui { rd: Reg, rs1: Reg, imm: Imm },
    // Memory
    Load { rd: Reg, rs1: Reg, imm: Imm },
    Store { rd: Reg, rs1: Reg, imm: Imm },
    // Ctrl
    Jump { imm: Imm },
    Jumpr { imm: Imm },
    If { rs1: Reg, imm: Imm },
    Ifr { rs1: Reg, imm: Imm },
    Call { imm: Imm },
    Ret,
    Iret,
}

impl Op {
    fn parse(code: &str) -> Result<Op, String> {
        if let Some((op, args)) = code.split_whitespace().collect::<Vec<_>>().split_first() {
            let op = op.to_owned();
            match op {
                // Arithmetic Operations

                // []
                "nop" => return Ok(Op::Nop),

                // [R,R]
                "mov" | "not" | "srs" | "sru" | "sl" => {
                    if let [ref rd, ref rs] = args[0..2] {
                        let rd = Reg::parse(rd)?;
                        let rs = Reg::parse(rs)?;
                        return match op {
                            "mov" => Ok(Op::Mov { rd, rs }),
                            "not" => Ok(Op::Not { rd, rs }),
                            "srs" => Ok(Op::Srs { rd, rs }),
                            "sru" => Ok(Op::Sru { rd, rs }),
                            "sl" => Ok(Op::SL { rd, rs }),
                            _ => unreachable!(),
                        };
                    } else {
                        return Err("Argument Error".to_string());
                    }
                }

                // [R,R,R]
                "add" | "sub" | "and" | "or" | "xor" | "eq" | "ltu" | "lts" => {
                    if let [ref rd, ref rs1, ref rs2] = args[0..3] {
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
                            "ltu" => Ok(Op::Ltu { rd, rs1, rs2 }),
                            "lts" => Ok(Op::Lts { rd, rs1, rs2 }),
                            _ => unreachable!(),
                        };
                    } else {
                        return Err("Argument Error".to_string());
                    }
                }

                // [R,I]
                "loadi" => {
                    if let [ref rd, ref imm] = args[0..2] {
                        let rd = Reg::parse(rd)?;
                        let imm = Imm::parse(imm)?;
                        return Ok(Op::Loadi { rd, imm });
                    } else {
                        return Err("Argument Error".to_string());
                    }
                }

                // [R,R,I]
                "addi" | "subi" | "andi" | "ori" | "xori" | "eqi" | "ltui" | "ltsi" => {
                    if let [ref rd, ref rs1, ref imm] = args[0..3] {
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
                            "ltui" => Ok(Op::Ltui { rd, rs1, imm }),
                            "ltsi" => Ok(Op::Ltsi { rd, rs1, imm }),
                            _ => unreachable!(),
                        };
                    } else {
                        return Err("Argument Error".to_string());
                    }
                }

                //--------------------------------------
                // Memory Operations
                "load" => {
                    if let [ref rd, ref rs1, ref imm] = args[0..3] {
                        let rd = Reg::parse(rd)?;
                        let rs1 = Reg::parse(rs1)?;
                        let imm = Imm::parse(imm)?;
                        return Ok(Op::Load { rd, rs1, imm });
                    }
                }
                "store" => {
                    if let [ref rd, ref rs1, ref imm] = args[0..3] {
                        let rd = Reg::parse(rd)?;
                        let rs1 = Reg::parse(rs1)?;
                        let imm = Imm::parse(imm)?;
                        return Ok(Op::Store { rd, rs1, imm });
                    }
                }

                //--------------------------------------
                // Controll Operations
                "jump" | "jumpr" | "call" => {
                    if let [ref imm] = args[0..1] {
                        let imm = Imm::parse(imm)?;
                        match op {
                            "jump" => return Ok(Op::Jump { imm }),
                            "jumpr" => return Ok(Op::Jumpr { imm }),
                            "call" => return Ok(Op::Call { imm }),
                            _ => unreachable!(),
                        }
                    }
                }
                "if" | "ifr" => {
                    if let [ref rs1, ref imm] = args[0..2] {
                        let rs1 = Reg::parse(rs1)?;
                        let imm = Imm::parse(imm)?;
                        match op {
                            "if" => return Ok(Op::If { rs1, imm }),
                            "ifr" => return Ok(Op::Ifr { rs1, imm }),
                            _ => unreachable!(),
                        }
                    }
                }

                "ret" => return Ok(Op::Ret),
                "iret" => return Ok(Op::Iret),

                _ => return Err(format!("Unknown Operation: {}", op)),
            };
        }
        return Err(format!("Unknown Error"));
    }
}

impl Op {
    fn cprint(&self) -> String {
        let cprint_op_reg = |op: &str, r1: Option<&Reg>, r2: Option<&Reg>, r3: Option<&Reg>| {
            let print = |r: Option<&Reg>| match r {
                Some(a) => a.print(),
                None => "".to_string(),
            };
            cformat!(
                "  <red>{:<6}</><blue>{:<4}{:<4}{:<4}</>",
                op,
                print(r1),
                print(r2),
                print(r3)
            )
        };
        let cprint_op_imm = |op: &str, r1: Option<&Reg>, r2: Option<&Reg>, imm: &Imm| {
            let print = |r: Option<&Reg>| match r {
                Some(a) => a.print(),
                None => "".to_string(),
            };
            cformat!(
                "  <red>{:<6}</><blue>{:<4}{:<4}{:<4}</>",
                op,
                print(r1),
                print(r2),
                "imm"
            )
        };
        match self {
            Op::Nop => cprint_op_reg("nop", None, None, None),

            Op::Mov { rd, rs } => cprint_op_reg("mov", Some(rd), Some(rs), None),
            Op::Not { rd, rs } => cprint_op_reg("not", Some(rd), Some(rs), None),
            Op::Srs { rd, rs } => cprint_op_reg("srs", Some(rd), Some(rs), None),
            Op::Sru { rd, rs } => cprint_op_reg("sru", Some(rd), Some(rs), None),
            Op::SL { rd, rs } => cprint_op_reg("sl", Some(rd), Some(rs), None),

            Op::Add { rd, rs1, rs2 } => cprint_op_reg("add", Some(rd), Some(rs1), Some(rs2)),
            Op::Sub { rd, rs1, rs2 } => cprint_op_reg("sub", Some(rd), Some(rs1), Some(rs2)),
            Op::And { rd, rs1, rs2 } => cprint_op_reg("and", Some(rd), Some(rs1), Some(rs2)),
            Op::Or { rd, rs1, rs2 } => cprint_op_reg("or", Some(rd), Some(rs1), Some(rs2)),
            Op::Xor { rd, rs1, rs2 } => cprint_op_reg("xor", Some(rd), Some(rs1), Some(rs2)),
            Op::Eq { rd, rs1, rs2 } => cprint_op_reg("eq", Some(rd), Some(rs1), Some(rs2)),
            Op::Ltu { rd, rs1, rs2 } => cprint_op_reg("ltu", Some(rd), Some(rs1), Some(rs2)),
            Op::Lts { rd, rs1, rs2 } => cprint_op_reg("lts", Some(rd), Some(rs1), Some(rs2)),

            Op::Loadi { rd, imm } => cprint_op_imm("loadi", Some(rd), None, imm),
            Op::Addi { rd, rs1, imm } => cprint_op_imm("addi", Some(rd), Some(rs1), imm),
            Op::Subi { rd, rs1, imm } => cprint_op_imm("subi", Some(rd), Some(rs1), imm),
            Op::Andi { rd, rs1, imm } => cprint_op_imm("andi", Some(rd), Some(rs1), imm),
            Op::Ori { rd, rs1, imm } => cprint_op_imm("ori", Some(rd), Some(rs1), imm),
            Op::Xori { rd, rs1, imm } => cprint_op_imm("xori", Some(rd), Some(rs1), imm),
            Op::Eqi { rd, rs1, imm } => cprint_op_imm("eqi", Some(rd), Some(rs1), imm),
            Op::Ltsi { rd, rs1, imm } => cprint_op_imm("ltsi", Some(rd), Some(rs1), imm),
            Op::Ltui { rd, rs1, imm } => cprint_op_imm("ltui", Some(rd), Some(rs1), imm),

            Op::Load { rd, rs1, imm } => cprint_op_imm("load", Some(rd), Some(rs1), imm),
            Op::Store { rd, rs1, imm } => cprint_op_imm("store", Some(rd), Some(rs1), imm),

            Op::Jump { imm } => cprint_op_imm("jump", None, None, imm),
            Op::Jumpr { imm } => cprint_op_imm("jumpr", None, None, imm),
            Op::If { rs1, imm } => cprint_op_imm("if", Some(rs1), None, imm),
            Op::Ifr { rs1, imm } => cprint_op_imm("ifr", Some(rs1), None, imm),
            Op::Call { imm } => cprint_op_imm("call", None, None, imm),
            Op::Ret => cprint_op_reg("ret", None, None, None),
            Op::Iret => cprint_op_reg("iret", None, None, None),
        }
    }
}

// ----------------------------------------------------------------------------
// Immidiate

#[derive(Debug)]
enum Imm {
    Literal(u16),
    UnknownLabel(String),
    OprLabel(String, u16),
    ConstLabel(String, u16),
    AddrLabel(String, u16),
}

impl Imm {
    fn parse(s: &str) -> Result<Imm, String> {
        if let Ok(lit) = parse_with_prefix(s) {
            return Ok(Imm::Literal(lit));
        };
        Ok(Imm::UnknownLabel(s.to_string()))
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
