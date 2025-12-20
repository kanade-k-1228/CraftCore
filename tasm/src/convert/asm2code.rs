use crate::{
    collect::{AsmMap, ConstMap},
    convert::types::Code,
    error::AsmError,
    grammer::ast,
};
use arch::{inst::Inst, reg::Reg};
use std::collections::HashMap;

pub fn asm2code(asms: &AsmMap, _consts: &ConstMap) -> Result<HashMap<String, Code>, AsmError> {
    let mut result = HashMap::new();
    for (name, (_addr, def)) in &asms.0 {
        if let ast::Def::Asm(_, _, stmts) = def {
            result.insert(name.clone(), gen_asm_block(stmts)?);
        }
    }
    Ok(result)
}

fn gen_asm_block(stmts: &[ast::AsmStmt]) -> Result<Code, AsmError> {
    // 1. Collect local labels
    let mut llabels: HashMap<String, u16> = HashMap::new();
    let mut pc: u16 = 0;
    for stmt in stmts {
        match stmt {
            ast::AsmStmt::Label(name) => {
                llabels.insert(name.clone(), pc);
            }
            ast::AsmStmt::Inst(_name, _args) => {
                pc += 1;
            }
        }
    }

    // 2. Generate instructions with resolved labels
    let mut insts = Vec::new();
    let mut pc: u16 = 0;
    for stmt in stmts {
        match stmt {
            ast::AsmStmt::Label(_) => {}
            ast::AsmStmt::Inst(name, args) => {
                let inst = parse_instruction(name, args, &llabels, pc)?;
                insts.push(inst);
                pc += 1;
            }
        }
    }

    Ok(Code(insts))
}

fn parse_instruction(
    inst: &str,
    args: &[ast::Expr],
    labels: &HashMap<String, u16>,
    pc: u16,
) -> Result<(Inst, Option<String>), AsmError> {
    match inst.to_lowercase().as_str() {
        "nop" => Ok((Inst::NOP(), None)),
        "mov" if args.len() == 2 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            Ok((Inst::MOV(rd, rs), None))
        }
        "add" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            let rs2 = parse_register(&args[2])?;
            Ok((Inst::ADD(rd, rs1, rs2), None))
        }
        "addi" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            if let Some(imm) = parse_immediate(&args[2]) {
                Ok((Inst::ADDI(rd, rs, imm), None))
            } else if let ast::Expr::Ident(symbol) = &args[2] {
                Ok((Inst::ADDI(rd, rs, 0), Some(symbol.clone())))
            } else {
                Err(AsmError::InvalidOperandType("addi".to_string()))
            }
        }
        "subi" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            if let Some(imm) = parse_immediate(&args[2]) {
                Ok((Inst::SUBI(rd, rs, imm), None))
            } else if let ast::Expr::Ident(symbol) = &args[2] {
                Ok((Inst::SUBI(rd, rs, 0), Some(symbol.clone())))
            } else {
                Err(AsmError::InvalidOperandType("subi".to_string()))
            }
        }
        "andi" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            if let Some(imm) = parse_immediate(&args[2]) {
                Ok((Inst::ANDI(rd, rs, imm), None))
            } else if let ast::Expr::Ident(symbol) = &args[2] {
                Ok((Inst::ANDI(rd, rs, 0), Some(symbol.clone())))
            } else {
                Err(AsmError::InvalidOperandType("andi".to_string()))
            }
        }
        "ori" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            if let Some(imm) = parse_immediate(&args[2]) {
                Ok((Inst::ORI(rd, rs, imm), None))
            } else if let ast::Expr::Ident(symbol) = &args[2] {
                Ok((Inst::ORI(rd, rs, 0), Some(symbol.clone())))
            } else {
                Err(AsmError::InvalidOperandType("ori".to_string()))
            }
        }
        "xori" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            if let Some(imm) = parse_immediate(&args[2]) {
                Ok((Inst::XORI(rd, rs, imm), None))
            } else if let ast::Expr::Ident(symbol) = &args[2] {
                Ok((Inst::XORI(rd, rs, 0), Some(symbol.clone())))
            } else {
                Err(AsmError::InvalidOperandType("xori".to_string()))
            }
        }
        "eqi" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            if let Some(imm) = parse_immediate(&args[2]) {
                Ok((Inst::EQI(rd, rs, imm), None))
            } else if let ast::Expr::Ident(symbol) = &args[2] {
                Ok((Inst::EQI(rd, rs, 0), Some(symbol.clone())))
            } else {
                Err(AsmError::InvalidOperandType("eqi".to_string()))
            }
        }
        "neqi" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            if let Some(imm) = parse_immediate(&args[2]) {
                Ok((Inst::NEQI(rd, rs, imm), None))
            } else if let ast::Expr::Ident(symbol) = &args[2] {
                Ok((Inst::NEQI(rd, rs, 0), Some(symbol.clone())))
            } else {
                Err(AsmError::InvalidOperandType("neqi".to_string()))
            }
        }
        "lti" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            if let Some(imm) = parse_immediate(&args[2]) {
                Ok((Inst::LTI(rd, rs, imm), None))
            } else if let ast::Expr::Ident(symbol) = &args[2] {
                Ok((Inst::LTI(rd, rs, 0), Some(symbol.clone())))
            } else {
                Err(AsmError::InvalidOperandType("lti".to_string()))
            }
        }
        "ltsi" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            if let Some(imm) = parse_immediate(&args[2]) {
                Ok((Inst::LTSI(rd, rs, imm), None))
            } else if let ast::Expr::Ident(symbol) = &args[2] {
                Ok((Inst::LTSI(rd, rs, 0), Some(symbol.clone())))
            } else {
                Err(AsmError::InvalidOperandType("ltsi".to_string()))
            }
        }
        "not" if args.len() == 2 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            Ok((Inst::NOT(rd, rs), None))
        }
        "loadi" if args.len() == 2 => {
            let rd = parse_register(&args[0])?;
            if let Some(imm) = parse_immediate(&args[1]) {
                Ok((Inst::LOADI(rd, imm), None))
            } else if let ast::Expr::Ident(symbol) = &args[1] {
                // Symbol reference - will be resolved later
                Ok((Inst::LOADI(rd, 0), Some(symbol.clone())))
            } else {
                Err(AsmError::InvalidOperandType("loadi".to_string()))
            }
        }
        "sub" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            let rs2 = parse_register(&args[2])?;
            Ok((Inst::SUB(rd, rs1, rs2), None))
        }
        "and" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            let rs2 = parse_register(&args[2])?;
            Ok((Inst::AND(rd, rs1, rs2), None))
        }
        "or" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            let rs2 = parse_register(&args[2])?;
            Ok((Inst::OR(rd, rs1, rs2), None))
        }
        "xor" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            let rs2 = parse_register(&args[2])?;
            Ok((Inst::XOR(rd, rs1, rs2), None))
        }
        "eq" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            let rs2 = parse_register(&args[2])?;
            Ok((Inst::EQ(rd, rs1, rs2), None))
        }
        "neq" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            let rs2 = parse_register(&args[2])?;
            Ok((Inst::NEQ(rd, rs1, rs2), None))
        }
        "lt" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            let rs2 = parse_register(&args[2])?;
            Ok((Inst::LT(rd, rs1, rs2), None))
        }
        "lts" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            let rs2 = parse_register(&args[2])?;
            Ok((Inst::LTS(rd, rs1, rs2), None))
        }
        "sr" if args.len() == 2 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            Ok((Inst::SR(rd, rs), None))
        }
        "srs" if args.len() == 2 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            Ok((Inst::SRS(rd, rs), None))
        }
        "srr" if args.len() == 2 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            Ok((Inst::SRR(rd, rs), None))
        }
        "sl" if args.len() == 2 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            Ok((Inst::SL(rd, rs), None))
        }
        "slr" if args.len() == 2 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            Ok((Inst::SLR(rd, rs), None))
        }
        "load" if args.len() == 2 => {
            // load(rd, addr/imm)
            let rd = parse_register(&args[0])?;
            // Check if second arg is an immediate or a symbol
            if let Some(imm) = parse_immediate(&args[1]) {
                Ok((Inst::LOADI(rd, imm), None))
            } else if let ast::Expr::Ident(symbol) = &args[1] {
                // Symbol reference - will be resolved later
                Ok((Inst::LOADI(rd, 0), Some(symbol.clone())))
            } else {
                Err(AsmError::InvalidOperandType("load".to_string()))
            }
        }
        "load" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            if let Some(imm) = parse_immediate(&args[2]) {
                Ok((Inst::LOAD(rd, rs, imm), None))
            } else if let ast::Expr::Ident(symbol) = &args[2] {
                Ok((Inst::LOAD(rd, rs, 0), Some(symbol.clone())))
            } else {
                Err(AsmError::InvalidOperandType("load".to_string()))
            }
        }
        "store" if args.len() == 2 => {
            // store(symbol, rs)
            if let ast::Expr::Ident(symbol) = &args[0] {
                let rs = parse_register(&args[1])?;
                // Symbol reference - will be resolved later
                Ok((Inst::STORE(rs, Reg::Z, 0), Some(symbol.clone())))
            } else {
                Err(AsmError::InvalidOperandType("store".to_string()))
            }
        }
        "store" if args.len() == 3 => {
            let rs2 = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            // Check if third arg is an immediate or a symbol
            if let Some(imm) = parse_immediate(&args[2]) {
                Ok((Inst::STORE(rs2, rs1, imm), None))
            } else if let ast::Expr::Ident(symbol) = &args[2] {
                // Symbol reference - will be resolved later
                Ok((Inst::STORE(rs2, rs1, 0), Some(symbol.clone())))
            } else {
                Err(AsmError::InvalidOperandType("store".to_string()))
            }
        }
        "if" if args.len() == 2 => {
            // if(cond_reg, label/offset)
            let cond = parse_register(&args[0])?;
            // Check if it's a label, symbol, or immediate
            if let Some(imm) = parse_immediate(&args[1]) {
                // Absolute address jump
                Ok((Inst::IF(cond, imm), None))
            } else if let ast::Expr::Ident(label_name) = &args[1] {
                // Check if it's a local label
                if let Some(&label_pc) = labels.get(label_name) {
                    // Convert to relative jump
                    let offset = (label_pc as i32 - pc as i32) as u16;
                    Ok((Inst::IFR(cond, offset), None))
                } else {
                    // External symbol - will be resolved later
                    Ok((Inst::IF(cond, 0), Some(label_name.clone())))
                }
            } else if let ast::Expr::Unary(ast::UnaryOp::Neg, inner) = &args[1] {
                // Negative immediate for relative backward jump
                if let Some(imm) = parse_immediate(inner) {
                    let offset = (-(imm as i16)) as u16;
                    Ok((Inst::IFR(cond, offset), None))
                } else {
                    Err(AsmError::InvalidOperandType("if".to_string()))
                }
            } else {
                Err(AsmError::InvalidOperandType("if".to_string()))
            }
        }
        "jump" if args.len() == 1 => {
            // Check if it's a label, symbol, or immediate
            if let Some(imm) = parse_immediate(&args[0]) {
                Ok((Inst::JUMP(imm), None))
            } else if let ast::Expr::Ident(label_name) = &args[0] {
                // Check if it's a local label
                if let Some(&label_pc) = labels.get(label_name) {
                    // Convert to relative jump
                    let offset = (label_pc as i32 - pc as i32) as u16;
                    Ok((Inst::JUMPR(offset), None))
                } else {
                    // External symbol - will be resolved later
                    Ok((Inst::JUMP(0), Some(label_name.clone())))
                }
            } else {
                Err(AsmError::InvalidOperandType("jump".to_string()))
            }
        }
        "call" if args.len() == 1 => {
            // Check if it's a symbol or immediate
            if let Some(imm) = parse_immediate(&args[0]) {
                Ok((Inst::CALL(imm), None))
            } else if let ast::Expr::Ident(func) = &args[0] {
                // Function reference - will be resolved later (external symbol)
                Ok((Inst::CALL(0), Some(func.clone())))
            } else {
                Err(AsmError::InvalidOperandType("call".to_string()))
            }
        }
        "ret" if args.is_empty() => Ok((Inst::RET(), None)),
        "ifr" if args.len() == 2 => {
            let cond = parse_register(&args[0])?;
            if let Some(imm) = parse_immediate(&args[1]) {
                Ok((Inst::IFR(cond, imm), None))
            } else if let ast::Expr::Ident(label_name) = &args[1] {
                // Check if it's a local label
                if let Some(&label_pc) = labels.get(label_name) {
                    // Calculate relative offset
                    let offset = (label_pc as i32 - pc as i32) as u16;
                    Ok((Inst::IFR(cond, offset), None))
                } else {
                    // External symbol
                    Ok((Inst::IFR(cond, 0), Some(label_name.clone())))
                }
            } else {
                Err(AsmError::InvalidOperandType("ifr".to_string()))
            }
        }
        "jumpr" if args.len() == 1 => {
            if let Some(imm) = parse_immediate(&args[0]) {
                Ok((Inst::JUMPR(imm), None))
            } else if let ast::Expr::Ident(label_name) = &args[0] {
                // Check if it's a local label
                if let Some(&label_pc) = labels.get(label_name) {
                    // Calculate relative offset
                    let offset = (label_pc as i32 - pc as i32) as u16;
                    Ok((Inst::JUMPR(offset), None))
                } else {
                    // External symbol
                    Ok((Inst::JUMPR(0), Some(label_name.clone())))
                }
            } else {
                Err(AsmError::InvalidOperandType("jumpr".to_string()))
            }
        }
        "iret" if args.is_empty() => Ok((Inst::IRET(), None)),
        _ => Err(AsmError::InvalidInstruction(inst.to_string())),
    }
}

/// Parse a register from an expression
fn parse_register(expr: &ast::Expr) -> Result<Reg, AsmError> {
    if let ast::Expr::Ident(name) = expr {
        match name.to_lowercase().as_str() {
            "z" | "zero" => Ok(Reg::Z),
            "sp" => Ok(Reg::SP),
            "ra" => Ok(Reg::RA),
            "fp" => Ok(Reg::FP),
            "a0" => Ok(Reg::A0),
            "a1" => Ok(Reg::A1),
            "t0" => Ok(Reg::T0),
            "t1" => Ok(Reg::T1),
            "t2" => Ok(Reg::T2),
            "t3" => Ok(Reg::T3),
            "s0" => Ok(Reg::S0),
            "s1" => Ok(Reg::S1),
            "s2" => Ok(Reg::S2),
            "s3" => Ok(Reg::S3),
            _ => Err(AsmError::InvalidRegister(name.clone())),
        }
    } else {
        Err(AsmError::InvalidRegister(format!("{:?}", expr)))
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
