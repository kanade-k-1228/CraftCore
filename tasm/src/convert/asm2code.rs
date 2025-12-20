use crate::{
    collect::{AsmMap, ConstMap},
    convert::types::Code,
    grammer::ast,
};
use arch::{inst::Inst, reg::Reg};
use std::collections::HashMap;

pub fn asm2code(asms: &AsmMap, _consts: &ConstMap) -> HashMap<String, Code> {
    let mut result = HashMap::new();
    for (name, (_addr, def)) in &asms.0 {
        if let ast::Def::Asm(_name, _addr_expr, body) = def {
            let insts = gen_asm_block(body);
            result.insert(name.clone(), Code(insts));
        }
    }
    result
}

fn gen_asm_block(body: &[ast::AsmStmt]) -> Vec<(Inst, Option<String>)> {
    // 1. Collect labels
    let mut labels: HashMap<String, u16> = HashMap::new();
    let mut pc: u16 = 0;

    for stmt in body {
        match stmt {
            ast::AsmStmt::Label(name) => {
                labels.insert(name.clone(), pc);
            }
            ast::AsmStmt::Inst(name, args) => {
                // Check if this instruction will be generated
                if parse_instruction(name, args, &labels, pc).is_some() {
                    pc += 1;
                }
            }
        }
    }

    // 2. generate instructions with resolved labels
    let mut insts = Vec::new();
    let mut pc: u16 = 0;

    for stmt in body {
        match stmt {
            ast::AsmStmt::Label(_name) => {
                // Labels don't generate instructions
            }
            ast::AsmStmt::Inst(name, args) => {
                if let Some(inst) = parse_instruction(name, args, &labels, pc) {
                    insts.push(inst);
                    pc += 1;
                }
            }
        }
    }

    insts
}

/// Parse a specific instruction with its arguments
fn parse_instruction(
    inst_name: &str,
    args: &[ast::Expr],
    labels: &HashMap<String, u16>,
    current_pc: u16,
) -> Option<(Inst, Option<String>)> {
    // This is a simplified parser - in reality, you'd need more sophisticated parsing
    match inst_name.to_lowercase().as_str() {
        "nop" => Some((Inst::NOP(), None)),
        "mov" if args.len() == 2 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            Some((Inst::MOV(rd, rs), None))
        }
        "add" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            let rs2 = parse_register(&args[2])?;
            Some((Inst::ADD(rd, rs1, rs2), None))
        }
        "addi" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            if let Some(imm) = parse_immediate(&args[2]) {
                Some((Inst::ADDI(rd, rs, imm), None))
            } else if let ast::Expr::Ident(symbol) = &args[2] {
                Some((Inst::ADDI(rd, rs, 0), Some(symbol.clone())))
            } else {
                None
            }
        }
        "subi" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            if let Some(imm) = parse_immediate(&args[2]) {
                Some((Inst::SUBI(rd, rs, imm), None))
            } else if let ast::Expr::Ident(symbol) = &args[2] {
                Some((Inst::SUBI(rd, rs, 0), Some(symbol.clone())))
            } else {
                None
            }
        }
        "andi" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            if let Some(imm) = parse_immediate(&args[2]) {
                Some((Inst::ANDI(rd, rs, imm), None))
            } else if let ast::Expr::Ident(symbol) = &args[2] {
                Some((Inst::ANDI(rd, rs, 0), Some(symbol.clone())))
            } else {
                None
            }
        }
        "ori" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            if let Some(imm) = parse_immediate(&args[2]) {
                Some((Inst::ORI(rd, rs, imm), None))
            } else if let ast::Expr::Ident(symbol) = &args[2] {
                Some((Inst::ORI(rd, rs, 0), Some(symbol.clone())))
            } else {
                None
            }
        }
        "xori" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            if let Some(imm) = parse_immediate(&args[2]) {
                Some((Inst::XORI(rd, rs, imm), None))
            } else if let ast::Expr::Ident(symbol) = &args[2] {
                Some((Inst::XORI(rd, rs, 0), Some(symbol.clone())))
            } else {
                None
            }
        }
        "eqi" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            if let Some(imm) = parse_immediate(&args[2]) {
                Some((Inst::EQI(rd, rs, imm), None))
            } else if let ast::Expr::Ident(symbol) = &args[2] {
                Some((Inst::EQI(rd, rs, 0), Some(symbol.clone())))
            } else {
                None
            }
        }
        "neqi" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            if let Some(imm) = parse_immediate(&args[2]) {
                Some((Inst::NEQI(rd, rs, imm), None))
            } else if let ast::Expr::Ident(symbol) = &args[2] {
                Some((Inst::NEQI(rd, rs, 0), Some(symbol.clone())))
            } else {
                None
            }
        }
        "lti" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            if let Some(imm) = parse_immediate(&args[2]) {
                Some((Inst::LTI(rd, rs, imm), None))
            } else if let ast::Expr::Ident(symbol) = &args[2] {
                Some((Inst::LTI(rd, rs, 0), Some(symbol.clone())))
            } else {
                None
            }
        }
        "ltsi" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            if let Some(imm) = parse_immediate(&args[2]) {
                Some((Inst::LTSI(rd, rs, imm), None))
            } else if let ast::Expr::Ident(symbol) = &args[2] {
                Some((Inst::LTSI(rd, rs, 0), Some(symbol.clone())))
            } else {
                None
            }
        }
        "not" if args.len() == 2 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            Some((Inst::NOT(rd, rs), None))
        }
        "loadi" if args.len() == 2 => {
            let rd = parse_register(&args[0])?;
            if let Some(imm) = parse_immediate(&args[1]) {
                Some((Inst::LOADI(rd, imm), None))
            } else if let ast::Expr::Ident(symbol) = &args[1] {
                // Symbol reference - will be resolved later
                Some((Inst::LOADI(rd, 0), Some(symbol.clone())))
            } else {
                None
            }
        }
        "sub" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            let rs2 = parse_register(&args[2])?;
            Some((Inst::SUB(rd, rs1, rs2), None))
        }
        "and" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            let rs2 = parse_register(&args[2])?;
            Some((Inst::AND(rd, rs1, rs2), None))
        }
        "or" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            let rs2 = parse_register(&args[2])?;
            Some((Inst::OR(rd, rs1, rs2), None))
        }
        "xor" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            let rs2 = parse_register(&args[2])?;
            Some((Inst::XOR(rd, rs1, rs2), None))
        }
        "eq" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            let rs2 = parse_register(&args[2])?;
            Some((Inst::EQ(rd, rs1, rs2), None))
        }
        "neq" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            let rs2 = parse_register(&args[2])?;
            Some((Inst::NEQ(rd, rs1, rs2), None))
        }
        "lt" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            let rs2 = parse_register(&args[2])?;
            Some((Inst::LT(rd, rs1, rs2), None))
        }
        "lts" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            let rs2 = parse_register(&args[2])?;
            Some((Inst::LTS(rd, rs1, rs2), None))
        }
        "sr" if args.len() == 2 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            Some((Inst::SR(rd, rs), None))
        }
        "srs" if args.len() == 2 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            Some((Inst::SRS(rd, rs), None))
        }
        "srr" if args.len() == 2 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            Some((Inst::SRR(rd, rs), None))
        }
        "sl" if args.len() == 2 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            Some((Inst::SL(rd, rs), None))
        }
        "slr" if args.len() == 2 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            Some((Inst::SLR(rd, rs), None))
        }
        "load" if args.len() == 2 => {
            // load(rd, addr/imm)
            let rd = parse_register(&args[0])?;
            // Check if second arg is an immediate or a symbol
            if let Some(imm) = parse_immediate(&args[1]) {
                Some((Inst::LOADI(rd, imm), None))
            } else if let ast::Expr::Ident(symbol) = &args[1] {
                // Symbol reference - will be resolved later
                Some((Inst::LOADI(rd, 0), Some(symbol.clone())))
            } else {
                None
            }
        }
        "load" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            if let Some(imm) = parse_immediate(&args[2]) {
                Some((Inst::LOAD(rd, rs, imm), None))
            } else if let ast::Expr::Ident(symbol) = &args[2] {
                Some((Inst::LOAD(rd, rs, 0), Some(symbol.clone())))
            } else {
                None
            }
        }
        "store" if args.len() == 2 => {
            // store(symbol, rs)
            if let ast::Expr::Ident(symbol) = &args[0] {
                let rs = parse_register(&args[1])?;
                // Symbol reference - will be resolved later
                Some((Inst::STORE(rs, Reg::Z, 0), Some(symbol.clone())))
            } else {
                None
            }
        }
        "store" if args.len() == 3 => {
            let rs2 = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            // Check if third arg is an immediate or a symbol
            if let Some(imm) = parse_immediate(&args[2]) {
                Some((Inst::STORE(rs2, rs1, imm), None))
            } else if let ast::Expr::Ident(symbol) = &args[2] {
                // Symbol reference - will be resolved later
                Some((Inst::STORE(rs2, rs1, 0), Some(symbol.clone())))
            } else {
                None
            }
        }
        "if" if args.len() == 2 => {
            // if(cond_reg, label/offset)
            let cond = parse_register(&args[0])?;
            // Check if it's a label, symbol, or immediate
            if let Some(imm) = parse_immediate(&args[1]) {
                // Absolute address jump
                Some((Inst::IF(cond, imm), None))
            } else if let ast::Expr::Ident(label_name) = &args[1] {
                // Check if it's a local label
                if let Some(&label_pc) = labels.get(label_name) {
                    // Convert to relative jump
                    let offset = (label_pc as i32 - current_pc as i32) as u16;
                    Some((Inst::IFR(cond, offset), None))
                } else {
                    // External symbol - will be resolved later
                    Some((Inst::IF(cond, 0), Some(label_name.clone())))
                }
            } else if let ast::Expr::Unary(ast::UnaryOp::Neg, inner) = &args[1] {
                // Negative immediate for relative backward jump
                if let Some(imm) = parse_immediate(inner) {
                    let offset = (-(imm as i16)) as u16;
                    Some((Inst::IFR(cond, offset), None))
                } else {
                    None
                }
            } else {
                None
            }
        }
        "jump" if args.len() == 1 => {
            // Check if it's a label, symbol, or immediate
            if let Some(imm) = parse_immediate(&args[0]) {
                Some((Inst::JUMP(imm), None))
            } else if let ast::Expr::Ident(label_name) = &args[0] {
                // Check if it's a local label
                if let Some(&label_pc) = labels.get(label_name) {
                    // Convert to relative jump
                    let offset = (label_pc as i32 - current_pc as i32) as u16;
                    Some((Inst::JUMPR(offset), None))
                } else {
                    // External symbol - will be resolved later
                    Some((Inst::JUMP(0), Some(label_name.clone())))
                }
            } else {
                None
            }
        }
        "call" if args.len() == 1 => {
            // Check if it's a symbol or immediate
            if let Some(imm) = parse_immediate(&args[0]) {
                Some((Inst::CALL(imm), None))
            } else if let ast::Expr::Ident(func) = &args[0] {
                // Function reference - will be resolved later (external symbol)
                Some((Inst::CALL(0), Some(func.clone())))
            } else {
                None
            }
        }
        "ret" if args.is_empty() => Some((Inst::RET(), None)),
        "ifr" if args.len() == 2 => {
            let cond = parse_register(&args[0])?;
            if let Some(imm) = parse_immediate(&args[1]) {
                Some((Inst::IFR(cond, imm), None))
            } else if let ast::Expr::Ident(label_name) = &args[1] {
                // Check if it's a local label
                if let Some(&label_pc) = labels.get(label_name) {
                    // Calculate relative offset
                    let offset = (label_pc as i32 - current_pc as i32) as u16;
                    Some((Inst::IFR(cond, offset), None))
                } else {
                    // External symbol
                    Some((Inst::IFR(cond, 0), Some(label_name.clone())))
                }
            } else {
                None
            }
        }
        "jumpr" if args.len() == 1 => {
            if let Some(imm) = parse_immediate(&args[0]) {
                Some((Inst::JUMPR(imm), None))
            } else if let ast::Expr::Ident(label_name) = &args[0] {
                // Check if it's a local label
                if let Some(&label_pc) = labels.get(label_name) {
                    // Calculate relative offset
                    let offset = (label_pc as i32 - current_pc as i32) as u16;
                    Some((Inst::JUMPR(offset), None))
                } else {
                    // External symbol
                    Some((Inst::JUMPR(0), Some(label_name.clone())))
                }
            } else {
                None
            }
        }
        "iret" if args.is_empty() => Some((Inst::IRET(), None)),
        _ => None,
    }
}

/// Parse a register from an expression
fn parse_register(expr: &ast::Expr) -> Option<Reg> {
    if let ast::Expr::Ident(name) = expr {
        match name.to_lowercase().as_str() {
            "z" | "zero" => Some(Reg::Z),
            "sp" => Some(Reg::SP),
            "ra" => Some(Reg::RA),
            "fp" => Some(Reg::FP),
            "a0" => Some(Reg::A0),
            "a1" => Some(Reg::A1),
            "t0" => Some(Reg::T0),
            "t1" => Some(Reg::T1),
            "t2" => Some(Reg::T2),
            "t3" => Some(Reg::T3),
            "s0" => Some(Reg::S0),
            "s1" => Some(Reg::S1),
            "s2" => Some(Reg::S2),
            "s3" => Some(Reg::S3),
            _ => None,
        }
    } else {
        None
    }
}

/// Parse an immediate value from an expression
fn parse_immediate(expr: &ast::Expr) -> Option<u16> {
    match expr {
        ast::Expr::NumberLit(n) => Some(*n as u16),
        ast::Expr::CharLit(ch) => Some(*ch as u16),
        _ => None,
    }
}
