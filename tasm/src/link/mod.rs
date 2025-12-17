pub mod structs;

use structs::*;
pub use structs::{Allocated, SectionData};

/// Phase 1: Link - allocate addresses for objects
pub fn link(asm_code: &str) -> Result<Allocated, String> {
    // Parse assembly code into items
    let items = parse_asm(asm_code)?;

    // Allocate addresses
    let allocated = crate::allocate::allocator::allocate(items)?;

    Ok(allocated)
}

/// Phase 2: Resolve - resolve symbols and fill unresolved operands
pub fn resolve(allocated: &Allocated) -> Result<Allocated, String> {
    // For now, just return the allocated as-is
    // In the future, this will resolve symbol references in instructions
    Ok(allocated.clone())
}

/// Phase 3: Generate binary from resolved allocation
pub fn generate_binary(allocated: &Allocated) -> Result<Vec<u8>, String> {
    crate::bingen::generator::generate(allocated)
}

/// Parse assembly code string into items
fn parse_asm(asm_code: &str) -> Result<Vec<Item>, String> {
    let mut items = Vec::new();
    let mut current_code: Option<(String, Vec<AsmLine>)> = None;

    for line in asm_code.lines() {
        let line = line.trim();

        // Skip empty lines and comments
        if line.is_empty() || line.starts_with(';') {
            continue;
        }

        // Memory location directive: @0x1234 name or @auto name
        if line.starts_with('@') {
            // Flush any current code section
            if let Some((name, code)) = current_code.take() {
                items.push(Item::Code(name, code));
            }

            let parts: Vec<&str> = line[1..].split_whitespace().collect();
            if parts.len() != 2 {
                return Err(format!("Invalid memory directive: {}", line));
            }

            let addr_str = parts[0];
            let name = parts[1].to_string();

            let addr = if addr_str == "auto" {
                None
            } else {
                Some(parse_hex(addr_str)?)
            };

            // Assume static for now (could be distinguished by context)
            items.push(Item::Static(name, 1, addr));
            continue;
        }

        // Constant data directive: #0x1234 value
        if line.starts_with('#') {
            // Flush any current code section
            if let Some((name, code)) = current_code.take() {
                items.push(Item::Code(name, code));
            }

            // Constants are embedded in code, skip for now
            continue;
        }

        // Label: name:
        if line.ends_with(':') {
            // Flush any current code section
            if let Some((name, code)) = current_code.take() {
                items.push(Item::Code(name, code));
            }

            let label = line[..line.len() - 1].to_string();
            current_code = Some((label, Vec::new()));
            continue;
        }

        // Instruction
        if let Some((_, ref mut code)) = current_code {
            let asm_line = parse_instruction(line)?;
            code.push(asm_line);
        } else {
            // Instruction without label, create anonymous section
            let asm_line = parse_instruction(line)?;
            current_code = Some(("_anon".to_string(), vec![asm_line]));
        }
    }

    // Flush final code section
    if let Some((name, code)) = current_code.take() {
        items.push(Item::Code(name, code));
    }

    Ok(items)
}

/// Parse a single instruction line
fn parse_instruction(line: &str) -> Result<AsmLine, String> {
    let parts: Vec<&str> = line.split_whitespace().collect();
    if parts.is_empty() {
        return Err("Empty instruction".to_string());
    }

    let mnemonic = parts[0];
    let operands = &parts[1..];

    // Parse operands, collecting any symbols
    let mut symbols = Vec::new();
    let mut resolved_operands = Vec::new();

    for op in operands {
        let op = op.trim_end_matches(',');

        // Try to parse as register
        if let Some(reg) = arch::reg::Reg::parse(op) {
            resolved_operands.push(Operand::Reg(reg));
            continue;
        }

        // Try to parse as immediate value
        if let Ok(val) = parse_hex(op) {
            resolved_operands.push(Operand::Imm(val as u16));
            continue;
        }

        // Otherwise it's a symbol
        symbols.push(op.to_string());
        resolved_operands.push(Operand::Symbol(op.to_string()));
    }

    // Build instruction based on mnemonic
    let inst = build_instruction(mnemonic, &resolved_operands)?;

    Ok(AsmLine { inst, symbols })
}

#[derive(Debug, Clone)]
enum Operand {
    Reg(arch::reg::Reg),
    Imm(u16),
    Symbol(String),
}

/// Build an instruction from mnemonic and operands
fn build_instruction(mnemonic: &str, operands: &[Operand]) -> Result<AsmInst, String> {
    use arch::inst::Inst;

    // For now, we'll create a placeholder that will be resolved later
    // This is simplified - full implementation would handle all instruction types

    match mnemonic.to_lowercase().as_str() {
        "nop" => Ok(AsmInst::Inst(Inst::NOP())),
        "ret" => Ok(AsmInst::Inst(Inst::RET())),
        "iret" => Ok(AsmInst::Inst(Inst::IRET())),

        "mov" => {
            if operands.len() != 2 {
                return Err(format!("mov expects 2 operands, got {}", operands.len()));
            }
            let rd = get_reg(&operands[0])?;
            let rs = get_reg(&operands[1])?;
            Ok(AsmInst::Inst(Inst::MOV(rd, rs)))
        }

        "add" => {
            if operands.len() != 3 {
                return Err(format!("add expects 3 operands, got {}", operands.len()));
            }
            let rd = get_reg(&operands[0])?;
            let rs1 = get_reg(&operands[1])?;
            let rs2 = get_reg(&operands[2])?;
            Ok(AsmInst::Inst(Inst::ADD(rd, rs1, rs2)))
        }

        "addi" => {
            if operands.len() != 3 {
                return Err(format!("addi expects 3 operands, got {}", operands.len()));
            }
            let rd = get_reg(&operands[0])?;
            let rs = get_reg(&operands[1])?;
            let imm = get_imm(&operands[2])?;
            Ok(AsmInst::Inst(Inst::ADDI(rd, rs, imm)))
        }

        "sub" => {
            if operands.len() != 3 {
                return Err(format!("sub expects 3 operands, got {}", operands.len()));
            }
            let rd = get_reg(&operands[0])?;
            let rs1 = get_reg(&operands[1])?;
            let rs2 = get_reg(&operands[2])?;
            Ok(AsmInst::Inst(Inst::SUB(rd, rs1, rs2)))
        }

        "subi" => {
            if operands.len() != 3 {
                return Err(format!("subi expects 3 operands, got {}", operands.len()));
            }
            let rd = get_reg(&operands[0])?;
            let rs = get_reg(&operands[1])?;
            let imm = get_imm(&operands[2])?;
            Ok(AsmInst::Inst(Inst::SUBI(rd, rs, imm)))
        }

        "load" => {
            if operands.len() != 3 {
                return Err(format!("load expects 3 operands, got {}", operands.len()));
            }
            let rd = get_reg(&operands[0])?;
            let rs = get_reg(&operands[1])?;
            let imm = get_imm(&operands[2])?;
            Ok(AsmInst::Inst(Inst::LOAD(rd, rs, imm)))
        }

        "loadi" => {
            if operands.len() != 2 {
                return Err(format!("loadi expects 2 operands, got {}", operands.len()));
            }
            let rd = get_reg(&operands[0])?;
            let imm = get_imm(&operands[1])?;
            Ok(AsmInst::Inst(Inst::LOADI(rd, imm)))
        }

        "store" => {
            if operands.len() != 3 {
                return Err(format!("store expects 3 operands, got {}", operands.len()));
            }
            let rs2 = get_reg(&operands[0])?;
            let rs1 = get_reg(&operands[1])?;
            let imm = get_imm(&operands[2])?;
            Ok(AsmInst::Inst(Inst::STORE(rs2, rs1, imm)))
        }

        "jump" => {
            if operands.len() != 1 {
                return Err(format!("jump expects 1 operand, got {}", operands.len()));
            }
            let imm = get_imm(&operands[0])?;
            Ok(AsmInst::Inst(Inst::JUMP(imm)))
        }

        "call" => {
            if operands.len() != 1 {
                return Err(format!("call expects 1 operand, got {}", operands.len()));
            }
            let imm = get_imm(&operands[0])?;
            Ok(AsmInst::Inst(Inst::CALL(imm)))
        }

        "if" => {
            if operands.len() != 2 {
                return Err(format!("if expects 2 operands, got {}", operands.len()));
            }
            let rs = get_reg(&operands[0])?;
            let imm = get_imm(&operands[1])?;
            Ok(AsmInst::Inst(Inst::IF(rs, imm)))
        }

        // Add more instructions as needed
        _ => Err(format!("Unknown instruction: {}", mnemonic)),
    }
}

fn get_reg(operand: &Operand) -> Result<arch::reg::Reg, String> {
    match operand {
        Operand::Reg(r) => Ok(*r),
        _ => Err(format!("Expected register, got {:?}", operand)),
    }
}

fn get_imm(operand: &Operand) -> Result<u16, String> {
    match operand {
        Operand::Imm(i) => Ok(*i),
        Operand::Symbol(_) => Ok(0), // Will be resolved later
        _ => Err(format!("Expected immediate or symbol, got {:?}", operand)),
    }
}

fn parse_hex(s: &str) -> Result<usize, String> {
    if s.starts_with("0x") || s.starts_with("0X") {
        usize::from_str_radix(&s[2..], 16).map_err(|e| format!("Invalid hex number {}: {}", s, e))
    } else {
        s.parse::<usize>()
            .map_err(|e| format!("Invalid number {}: {}", s, e))
    }
}
