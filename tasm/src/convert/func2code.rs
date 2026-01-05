use crate::{
    convert::types::{Code, Immidiate},
    error::FuncGenError,
    eval::{eval::Evaluator, normtype::NormType},
    grammer::ast,
};
use arch::{inst::Inst, reg::Reg};
use std::collections::HashMap;

struct CodeGenContext<'a> {
    lvars: HashMap<String, i16>, // Local variable offsets
    stack_size: i16,             // Current stack frame size
    insts: Vec<(Inst, Option<Immidiate>)>,
    evaluator: &'a Evaluator<'a>,
}

impl<'a> CodeGenContext<'a> {
    fn new(evaluator: &'a Evaluator<'a>) -> Self {
        Self {
            lvars: HashMap::new(),
            stack_size: 0,
            insts: Vec::new(),
            evaluator,
        }
    }

    /// Emit a raw instruction
    fn emit_inst(&mut self, inst: Inst) {
        self.insts.push((inst, None));
    }

    /// Emit an instruction with symbol references
    fn emit_inst_with_symbol(&mut self, inst: Inst, symbol: String) {
        self.insts.push((inst, Some(Immidiate::Symbol(symbol, 0))));
    }

    /// Get the current instruction position
    fn current_pos(&self) -> usize {
        self.insts.len()
    }

    /// Emit a placeholder jump that will be patched later
    fn emit_placeholder_jump(&mut self, is_conditional: bool, cond_reg: Option<Reg>) -> usize {
        let pos = self.current_pos();
        if is_conditional {
            self.emit_inst(Inst::IFR(cond_reg.unwrap_or(Reg::T0), 0));
        } else {
            self.emit_inst(Inst::JUMPR(0));
        }
        pos
    }

    /// Patch a jump instruction with the relative offset
    fn patch_jump(&mut self, jump_pos: usize, target_pos: usize) {
        // Calculate relative offset from the jump instruction to the target
        let offset = (target_pos as i32 - jump_pos as i32) as u16;

        // Replace the instruction at jump_pos with the correct offset
        let (ref mut inst, _) = &mut self.insts[jump_pos];
        match inst {
            Inst::JUMPR(_) => *inst = Inst::JUMPR(offset),
            Inst::IFR(reg, _) => *inst = Inst::IFR(*reg, offset),
            _ => {} // Should not happen
        }
    }

    /// Allocate stack space for a local variable
    fn alloc_local(&mut self, name: String, size: i16) {
        self.stack_size += size;
        self.lvars.insert(name, -self.stack_size);
    }

    /// Get the stack offset for a local variable
    fn get_local_offset(&self, name: &str) -> Option<i16> {
        self.lvars.get(name).copied()
    }
}

/// Generate function prologue from function type information
///
/// Prologue saves the caller's state and sets up the stack frame:
/// 1. Save return address (RA) and frame pointer (FP)
/// 2. Update FP to point to current stack frame
/// 3. Allocate space for local variables and arguments
///
/// Stack layout after prologue:
/// ```
/// [SP] -> local variables...
///         saved arguments...
///         saved FP
///         saved RA
/// [FP] -> (points to saved RA position)
/// ```
fn generate_prologue(args: &[(String, NormType)]) -> Vec<(Inst, Option<Immidiate>)> {
    let mut insts = Vec::new();

    // Calculate total size needed for saved registers (RA + FP)
    let saved_regs_size = 2u16;

    // Calculate size needed for arguments on stack
    let mut args_stack_size = 0u16;
    for (_name, arg_type) in args {
        args_stack_size += arg_type.sizeof() as u16;
    }

    // Total stack allocation for prologue
    let stack_alloc = saved_regs_size + args_stack_size;

    // 1. Allocate stack space
    if stack_alloc > 0 {
        insts.push((Inst::SUBI(Reg::SP, Reg::SP, stack_alloc), None));
    }

    // 2. Save return address and frame pointer
    insts.push((Inst::STORE(Reg::RA, Reg::SP, 0), None));
    insts.push((Inst::STORE(Reg::FP, Reg::SP, 1), None));

    // 3. Set new frame pointer
    insts.push((Inst::MOV(Reg::FP, Reg::SP), None));

    // 4. Save arguments to stack
    // First 2 arguments come in A0, A1 registers
    // Additional arguments are already on the stack (passed by caller)
    let mut offset = saved_regs_size;
    for (i, (_name, arg_type)) in args.iter().enumerate() {
        let arg_size = arg_type.sizeof() as u16;

        if i < 2 {
            // Arguments in registers - save them to stack
            let arg_reg = match i {
                0 => Reg::A0,
                1 => Reg::A1,
                _ => unreachable!(),
            };

            // Store each word of the argument
            for j in 0..arg_size {
                if j == 0 {
                    insts.push((Inst::STORE(arg_reg, Reg::FP, offset + j), None));
                } else {
                    // For multi-word arguments, would need to handle appropriately
                    // For now, assume single-word arguments
                }
            }
        }
        // Arguments beyond the first 2 are already on the stack (caller pushed them)

        offset += arg_size;
    }

    insts
}

/// Generate function epilogue from function type information
///
/// Epilogue restores the caller's state and returns:
/// 1. Restore stack pointer to frame pointer position
/// 2. Restore saved FP and RA
/// 3. Deallocate stack frame
/// 4. Return to caller
///
/// Return value (if any) should be in A0 register before calling epilogue
fn generate_epilogue(
    args: &[(String, NormType)],
    _ret_type: &NormType,
) -> Vec<(Inst, Option<Immidiate>)> {
    let mut insts = Vec::new();

    // Calculate stack sizes
    let saved_regs_size = 2u16;
    let mut args_stack_size = 0u16;
    for (_name, arg_type) in args {
        args_stack_size += arg_type.sizeof() as u16;
    }
    let stack_alloc = saved_regs_size + args_stack_size;

    // 1. Restore stack pointer (discard local variables)
    insts.push((Inst::MOV(Reg::SP, Reg::FP), None));

    // 2. Restore frame pointer and return address
    insts.push((Inst::LOAD(Reg::FP, Reg::SP, 1), None));
    insts.push((Inst::LOAD(Reg::RA, Reg::SP, 0), None));

    // 3. Deallocate stack frame
    if stack_alloc > 0 {
        insts.push((Inst::ADDI(Reg::SP, Reg::SP, stack_alloc), None));
    }

    // 4. Return to caller
    insts.push((Inst::RET(), None));

    insts
}

/// Generate code from all functions in the AST
pub fn func2code<'a>(evaluator: &'a Evaluator<'a>) -> Result<HashMap<&'a str, Code>, FuncGenError> {
    let mut result = HashMap::new();
    for (&name, entry) in evaluator.funcs() {
        if let ast::Def::Func(_, args, ret, stmts) = entry.def {
            result.insert(name, gen_func(args, ret, stmts, evaluator)?);
        }
    }
    Ok(result)
}

/// Generate instructions for a function from its AST
fn gen_func(
    args: &Vec<(String, ast::Type)>,
    ret: &ast::Type,
    stmts: &Vec<ast::Stmt>,
    evaluator: &Evaluator,
) -> Result<Code, FuncGenError> {
    let mut ctx = CodeGenContext::new(evaluator);

    // Convert AST types to normalized types for prologue/epilogue generation
    let mut norm_args = Vec::new();
    for (name, arg_type) in args {
        let norm_type = evaluator
            .normtype(arg_type)
            .map_err(|_| FuncGenError::TypeCollectionFailed(name.clone()))?;
        norm_args.push((name.clone(), norm_type));
    }

    let norm_ret_type = evaluator
        .normtype(ret)
        .map_err(|_| FuncGenError::TypeCollectionFailed("return type".to_string()))?;

    // Generate function prologue
    let prologue = generate_prologue(&norm_args);
    for inst in prologue {
        ctx.insts.push(inst);
    }

    // Set up local variable tracking for arguments
    let saved_regs_size = 2;
    let mut offset = saved_regs_size;
    for (name, norm_type) in &norm_args {
        ctx.lvars.insert(name.clone(), offset as i16);
        offset += norm_type.sizeof() as i16;
    }

    // Compile function body - process all statements
    for stmt in stmts {
        compile_stmt(&mut ctx, stmt)?;
    }

    // Function epilogue
    let epilogue = generate_epilogue(&norm_args, &norm_ret_type);
    for inst in epilogue {
        ctx.insts.push(inst);
    }

    Ok(Code(ctx.insts))
}

/// Compile a statement into instructions
/// Returns the position after the last instruction
fn compile_stmt(ctx: &mut CodeGenContext, stmt: &ast::Stmt) -> Result<usize, FuncGenError> {
    match stmt {
        ast::Stmt::Block(stmts) => {
            for s in stmts {
                compile_stmt(ctx, s)?;
            }
        }
        ast::Stmt::Expr(expr) => {
            compile_expr(ctx, expr, None)?;
        }
        ast::Stmt::Assign(lhs, rhs) => {
            // Compile RHS and get result in a register
            let rhs_reg = compile_expr(ctx, rhs, Some(Reg::T0))?;
            // Store to LHS
            compile_lvalue(ctx, lhs, rhs_reg)?;
        }
        ast::Stmt::Cond(cond, then_stmt, else_stmt) => {
            // Compile condition
            let cond_reg = compile_expr(ctx, cond, Some(Reg::T0))?;

            // Branch to else if condition is false
            ctx.emit_inst(Inst::NOT(Reg::T1, cond_reg));
            let else_jump = ctx.emit_placeholder_jump(true, Some(Reg::T1));

            // Then branch
            compile_stmt(ctx, then_stmt)?;

            if let Some(else_stmt) = else_stmt {
                // Jump over else branch
                let end_jump = ctx.emit_placeholder_jump(false, None);

                // Else branch starts here
                let else_pos = ctx.current_pos();
                ctx.patch_jump(else_jump, else_pos);

                compile_stmt(ctx, else_stmt)?;

                // End of if-else
                let end_pos = ctx.current_pos();
                ctx.patch_jump(end_jump, end_pos);
            } else {
                // No else branch - patch jump to here
                let end_pos = ctx.current_pos();
                ctx.patch_jump(else_jump, end_pos);
            }
        }
        ast::Stmt::Loop(cond, body) => {
            let loop_start = ctx.current_pos();

            // Compile condition
            let cond_reg = compile_expr(ctx, cond, Some(Reg::T0))?;

            // Exit loop if condition is false
            ctx.emit_inst(Inst::NOT(Reg::T1, cond_reg));
            let exit_jump = ctx.emit_placeholder_jump(true, Some(Reg::T1));

            // Loop body
            compile_stmt(ctx, body)?;

            // Jump back to start (calculate relative offset)
            let current = ctx.current_pos();
            let offset = (loop_start as i32 - current as i32) as u16;
            ctx.emit_inst(Inst::JUMPR(offset));

            // Patch exit jump to point here
            let exit_pos = ctx.current_pos();
            ctx.patch_jump(exit_jump, exit_pos);
        }
        ast::Stmt::Return(expr) => {
            if let Some(expr) = expr {
                // Compile return value into A0 (return value register)
                compile_expr(ctx, expr, Some(Reg::A0))?;
            }
            // Jump to function epilogue (calculate offset at runtime)
            // For now, we'll need to count instructions to epilogue
            // This is simplified - in practice you'd track epilogue position
            ctx.emit_inst(Inst::JUMPR(1000)); // Placeholder - should calculate actual offset
        }
        ast::Stmt::Var(name, _ty, init) => {
            // Allocate stack space for the variable
            ctx.alloc_local(name.clone(), 1);

            // Initialize if provided
            if let Some(init_expr) = init {
                let init_reg = compile_expr(ctx, init_expr, Some(Reg::T0))?;
                let offset = ctx
                    .get_local_offset(name)
                    .ok_or_else(|| FuncGenError::UndefinedVariable(name.clone()))?;
                ctx.emit_inst(Inst::STORE(init_reg, Reg::FP, offset as u16));
            }
        }
    }
    Ok(ctx.current_pos())
}

/// Compile an expression into a register
fn compile_expr(
    ctx: &mut CodeGenContext,
    expr: &ast::Expr,
    target: Option<Reg>,
) -> Result<Reg, FuncGenError> {
    let target_reg = target.unwrap_or(Reg::T0);

    Ok(match expr {
        ast::Expr::NumberLit(n) => {
            ctx.emit_inst(Inst::LOADI(target_reg, *n as u16));
            target_reg
        }
        ast::Expr::CharLit(c) => {
            ctx.emit_inst(Inst::LOADI(target_reg, *c as u16));
            target_reg
        }
        ast::Expr::StringLit(_s) => {
            // String literals would need to be stored in data section
            // For now, just load address placeholder
            ctx.emit_inst_with_symbol(
                Inst::LOADI(target_reg, 0), // Address will be resolved later
                "string_placeholder".to_string(),
            );
            target_reg
        }
        ast::Expr::Ident(name) => {
            // Check if it's a local variable
            if let Some(offset) = ctx.get_local_offset(name) {
                ctx.emit_inst(Inst::LOAD(target_reg, Reg::FP, offset as u16));
            } else {
                // Could be a global/static - emit with symbol reference
                ctx.emit_inst_with_symbol(
                    Inst::LOADI(target_reg, 0), // Address will be resolved later
                    name.clone(),
                );
            }
            target_reg
        }
        ast::Expr::Binary(op, lhs, rhs) => {
            // Compile left operand
            let lhs_reg = compile_expr(ctx, lhs, Some(Reg::T0))?;
            // Compile right operand
            let rhs_reg = compile_expr(ctx, rhs, Some(Reg::T1))?;

            // Generate instruction based on operator
            match op {
                ast::BinaryOp::Add => ctx.emit_inst(Inst::ADD(target_reg, lhs_reg, rhs_reg)),
                ast::BinaryOp::Sub => ctx.emit_inst(Inst::SUB(target_reg, lhs_reg, rhs_reg)),
                ast::BinaryOp::And => ctx.emit_inst(Inst::AND(target_reg, lhs_reg, rhs_reg)),
                ast::BinaryOp::Or => ctx.emit_inst(Inst::OR(target_reg, lhs_reg, rhs_reg)),
                ast::BinaryOp::Xor => ctx.emit_inst(Inst::XOR(target_reg, lhs_reg, rhs_reg)),
                ast::BinaryOp::Eq => ctx.emit_inst(Inst::EQ(target_reg, lhs_reg, rhs_reg)),
                ast::BinaryOp::Ne => ctx.emit_inst(Inst::NEQ(target_reg, lhs_reg, rhs_reg)),
                ast::BinaryOp::Lt => ctx.emit_inst(Inst::LT(target_reg, lhs_reg, rhs_reg)),
                ast::BinaryOp::Le => {
                    // a <= b is !(a > b) = !(b < a)
                    ctx.emit_inst(Inst::LT(Reg::T2, rhs_reg, lhs_reg));
                    ctx.emit_inst(Inst::NOT(target_reg, Reg::T2));
                }
                ast::BinaryOp::Gt => ctx.emit_inst(Inst::LT(target_reg, rhs_reg, lhs_reg)),
                ast::BinaryOp::Ge => {
                    // a >= b is !(a < b)
                    ctx.emit_inst(Inst::LT(Reg::T2, lhs_reg, rhs_reg));
                    ctx.emit_inst(Inst::NOT(target_reg, Reg::T2));
                }
                ast::BinaryOp::Shl => ctx.emit_inst(Inst::SL(target_reg, lhs_reg)),
                ast::BinaryOp::Shr => ctx.emit_inst(Inst::SR(target_reg, lhs_reg)),
                _ => {
                    // Mul, Div, Mod not directly supported - would need software implementation
                    ctx.emit_inst(Inst::LOADI(target_reg, 0));
                }
            }
            target_reg
        }
        ast::Expr::Unary(op, operand) => {
            let operand_reg = compile_expr(ctx, operand, Some(Reg::T0))?;

            match op {
                ast::UnaryOp::Pos => {
                    // Positive is a no-op, just move the value
                    ctx.emit_inst(Inst::MOV(target_reg, operand_reg));
                }
                ast::UnaryOp::Neg => {
                    ctx.emit_inst(Inst::LOADI(Reg::T1, 0));
                    ctx.emit_inst(Inst::SUB(target_reg, Reg::T1, operand_reg));
                }
                ast::UnaryOp::Not => {
                    ctx.emit_inst(Inst::NOT(target_reg, operand_reg));
                }
            }
            target_reg
        }
        ast::Expr::Deref(operand) => {
            let operand_reg = compile_expr(ctx, operand, Some(Reg::T0))?;
            // Load from address in operand_reg
            ctx.emit_inst(Inst::LOAD(target_reg, operand_reg, 0));
            target_reg
        }
        ast::Expr::Addr(operand) => {
            // Address-of would need to handle lvalues specially
            // For now, just compile the operand
            let operand_reg = compile_expr(ctx, operand, Some(Reg::T0))?;
            ctx.emit_inst(Inst::MOV(target_reg, operand_reg));
            target_reg
        }
        ast::Expr::Call(func_expr, args) => {
            // Evaluate arguments and push onto stack or registers
            for (i, arg) in args.iter().enumerate() {
                let arg_reg = compile_expr(ctx, arg, Some(Reg::T0))?;
                // For simplicity, pass first 2 args in A0-A1
                if i < 2 {
                    let dest_reg = match i {
                        0 => Reg::A0,
                        1 => Reg::A1,
                        _ => unreachable!(),
                    };
                    if arg_reg != dest_reg {
                        ctx.emit_inst(Inst::MOV(dest_reg, arg_reg));
                    }
                } else {
                    // Additional args go on stack
                    ctx.emit_inst(Inst::SUBI(Reg::SP, Reg::SP, 1));
                    ctx.emit_inst(Inst::STORE(arg_reg, Reg::SP, 0));
                }
            }

            // Call the function
            if let ast::Expr::Ident(func_name) = &**func_expr {
                ctx.emit_inst_with_symbol(
                    Inst::CALL(0), // Address will be resolved later
                    func_name.clone(),
                );
            } else {
                // Indirect call through register
                let _func_reg = compile_expr(ctx, func_expr, Some(Reg::T0))?;
                // Would need a CALLR instruction for indirect calls
                // For now, just use placeholder
                ctx.emit_inst(Inst::NOP());
            }

            // Clean up stack if we pushed arguments
            let stack_args = if args.len() > 2 { args.len() - 2 } else { 0 };
            if stack_args > 0 {
                ctx.emit_inst(Inst::ADDI(Reg::SP, Reg::SP, stack_args as u16));
            }

            // Result is in A0, move to target if needed
            if target_reg != Reg::A0 {
                ctx.emit_inst(Inst::MOV(target_reg, Reg::A0));
            }
            target_reg
        }
        ast::Expr::Cond(cond, then_expr, else_expr) => {
            // Ternary conditional expression
            let cond_reg = compile_expr(ctx, cond, Some(Reg::T0))?;

            // Jump to else if false
            ctx.emit_inst(Inst::NOT(Reg::T1, cond_reg));
            let else_jump = ctx.emit_placeholder_jump(true, Some(Reg::T1));

            // Then expression
            let then_reg = compile_expr(ctx, then_expr, Some(target_reg))?;
            if then_reg != target_reg {
                ctx.emit_inst(Inst::MOV(target_reg, then_reg));
            }

            // Jump over else
            let end_jump = ctx.emit_placeholder_jump(false, None);

            // Else expression
            let else_pos = ctx.current_pos();
            ctx.patch_jump(else_jump, else_pos);

            let else_reg = compile_expr(ctx, else_expr, Some(target_reg))?;
            if else_reg != target_reg {
                ctx.emit_inst(Inst::MOV(target_reg, else_reg));
            }

            // End
            let end_pos = ctx.current_pos();
            ctx.patch_jump(end_jump, end_pos);

            target_reg
        }
        ast::Expr::Sizeof(typ) => {
            // Calculate size at compile time
            let norm_type = ctx
                .evaluator
                .normtype(typ)
                .map_err(|_| FuncGenError::TypeCollectionFailed("sizeof".to_string()))?;
            let size = norm_type.sizeof() as u16;
            ctx.emit_inst(Inst::LOADI(target_reg, size));
            target_reg
        }
        _ => {
            // Other expression types not yet implemented
            ctx.emit_inst(Inst::LOADI(target_reg, 0));
            target_reg
        }
    })
}

/// Compile an lvalue (left-hand side of assignment)
fn compile_lvalue(
    ctx: &mut CodeGenContext,
    lvalue: &ast::Expr,
    value_reg: Reg,
) -> Result<(), FuncGenError> {
    match lvalue {
        ast::Expr::Ident(name) => {
            if let Some(offset) = ctx.get_local_offset(name) {
                ctx.emit_inst(Inst::STORE(value_reg, Reg::FP, offset as u16));
            } else {
                // Global/static variable - emit with symbol reference
                ctx.emit_inst_with_symbol(
                    Inst::STORE(value_reg, Reg::Z, 0), // Address will be resolved later
                    name.clone(),
                );
            }
        }
        ast::Expr::Deref(addr_expr) => {
            // Store through pointer
            let addr_reg = compile_expr(ctx, addr_expr, Some(Reg::T1))?;
            ctx.emit_inst(Inst::STORE(value_reg, addr_reg, 0));
        }
        ast::Expr::Index(array_expr, index_expr) => {
            // Compile array base address
            let array_reg = compile_expr(ctx, array_expr, Some(Reg::T1))?;
            // Compile index
            let index_reg = compile_expr(ctx, index_expr, Some(Reg::T2))?;
            // Add index to base address
            ctx.emit_inst(Inst::ADD(Reg::T1, array_reg, index_reg));
            // Store value
            ctx.emit_inst(Inst::STORE(value_reg, Reg::T1, 0));
        }
        ast::Expr::Member(struct_expr, _field_name) => {
            // Would need type information to calculate field offset
            // For now, just store at base address
            let struct_reg = compile_expr(ctx, struct_expr, Some(Reg::T1))?;
            ctx.emit_inst(Inst::STORE(value_reg, struct_reg, 0));
        }
        _ => {
            return Err(FuncGenError::InvalidLValue(format!("{:?}", lvalue)));
        }
    }
    Ok(())
}
