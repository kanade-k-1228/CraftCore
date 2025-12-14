use crate::global::{Global, Globals, Type};
use crate::grammer::ast::{self, BinaryOp, Expr, Stmt, UnaryOp};
use std::collections::HashMap;

#[derive(Debug)]
pub struct CodeGen {
    output: Vec<String>,
    label_counter: usize,
    temp_counter: usize,
    locals: HashMap<String, LocalVar>,
    stack_offset: isize,
    globals: Globals,
}

#[derive(Debug, Clone)]
struct LocalVar {
    offset: isize, // Offset from frame pointer
    size: usize,   // Size in words
    typ: Type,
}

impl CodeGen {
    pub fn new(globals: Globals) -> Self {
        CodeGen {
            output: Vec::new(),
            label_counter: 0,
            temp_counter: 0,
            locals: HashMap::new(),
            stack_offset: 0,
            globals,
        }
    }

    pub fn generate(globals: &Globals) -> Result<String, String> {
        let mut codegen = CodeGen::new(globals.clone());

        // Add halt address definition
        codegen.emit("@0x0010 halt".to_string());
        codegen.emit("".to_string());

        // Generate code for each global definition
        for (name, global) in &globals.0 {
            match global {
                Global::Func(args, ret_type, body) => {
                    codegen.gen_function(name, args, ret_type, body)?;
                }
                Global::Static(typ, addr) => {
                    codegen.gen_static(name, typ, *addr)?;
                }
                Global::Const(_lit, _typ) => {
                    // Constants will be inlined
                }
                Global::Asm(_stmt, _addr) => {
                    // Direct assembly - not yet implemented
                }
                Global::Type(_) => {
                    // Type definitions don't generate code
                }
            }
        }

        Ok(codegen.output.join("\n"))
    }

    fn emit(&mut self, line: String) {
        self.output.push(line);
    }

    fn fresh_label(&mut self, prefix: &str) -> String {
        let label = format!("{}_{}", prefix, self.label_counter);
        self.label_counter += 1;
        label
    }

    fn gen_static(&mut self, name: &str, _typ: &Type, addr: Option<usize>) -> Result<(), String> {
        if let Some(addr) = addr {
            self.emit(format!("@0x{:04X} {}", addr, name));
        } else {
            self.emit(format!("@auto {}", name));
        }
        Ok(())
    }

    fn gen_function(
        &mut self,
        name: &str,
        args: &[(String, Type)],
        _ret_type: &Type,
        body: &Stmt,
    ) -> Result<(), String> {
        // Reset local state
        self.locals.clear();
        self.stack_offset = 0;

        // Remember if this is the main function
        let is_main = name == "main";

        // Function label
        self.emit(format!("\n{}:", name));

        // TODO: Function prologue (save registers, allocate stack frame)

        // Arguments are passed in A0, A1 or on stack
        for (i, (arg_name, arg_type)) in args.iter().enumerate() {
            let size = self.type_size(arg_type);
            self.locals.insert(
                arg_name.clone(),
                LocalVar {
                    offset: self.stack_offset,
                    size,
                    typ: arg_type.clone(),
                },
            );

            // Copy argument from register to stack
            if i == 0 {
                self.emit(format!("    store a0 sp 0x{:04X}", self.stack_offset));
            } else if i == 1 {
                self.emit(format!("    store a1 sp 0x{:04X}", self.stack_offset));
            }
            // More than 2 arguments would need to be passed on stack
            self.stack_offset += size as isize;
        }

        // Generate function body
        self.gen_stmt_in_func(body, is_main)?;

        // For main function, add halt at the end (in case no return)
        if is_main {
            self.emit("    loadi t0 0x0001".to_string());
            self.emit("    store t0 z halt".to_string());
        }

        // TODO: Function epilogue (restore registers, deallocate stack frame)

        Ok(())
    }

    fn gen_stmt_in_func(&mut self, stmt: &Stmt, is_main: bool) -> Result<(), String> {
        match stmt {
            Stmt::Block(stmts) => {
                for s in stmts {
                    self.gen_stmt_in_func(s, is_main)?;
                }
            }
            Stmt::Return(expr) => {
                if let Some(e) = expr {
                    // Evaluate expression into a0 (return value register)
                    self.gen_expr(e, "a0")?;
                }
                // For main function, halt instead of return
                if is_main {
                    self.emit("    loadi t0 0x0001".to_string());
                    self.emit("    store t0 z halt".to_string());
                } else {
                    self.emit("    ret".to_string());
                }
            }
            _ => {
                // For other statements, use the regular gen_stmt
                self.gen_stmt(stmt)?;
            }
        }
        Ok(())
    }

    fn gen_stmt(&mut self, stmt: &Stmt) -> Result<(), String> {
        match stmt {
            Stmt::Block(stmts) => {
                for s in stmts {
                    self.gen_stmt(s)?;
                }
            }
            Stmt::Var(name, ast_type, init) => {
                // Resolve the type or infer from initializer
                let typ = match self.resolve_type(ast_type)? {
                    Type::Error => {
                        // Type inference needed
                        if let Some(init_expr) = init {
                            self.infer_expr_type(init_expr)?
                        } else {
                            return Err(format!(
                                "Cannot infer type for variable '{}' without initializer",
                                name
                            ));
                        }
                    }
                    t => t,
                };
                let size = self.type_size(&typ);

                self.locals.insert(
                    name.clone(),
                    LocalVar {
                        offset: self.stack_offset,
                        size,
                        typ: typ.clone(),
                    },
                );

                if let Some(init_expr) = init {
                    self.gen_var_init(name, &typ, init_expr)?;
                }

                self.stack_offset += size as isize;
            }
            Stmt::Assign(lhs, rhs) => {
                // Evaluate RHS into t0
                self.gen_expr(rhs, "t0")?;

                // Store to LHS
                match lhs {
                    Expr::Ident(name) => {
                        if let Some(var) = self.locals.get(name) {
                            self.emit(format!("    store t0 sp 0x{:04X}", var.offset));
                        } else {
                            return Err(format!("Undefined variable: {}", name));
                        }
                    }
                    Expr::Member(obj, field) => {
                        // TODO: Handle struct member assignment
                        return Err("Struct member assignment not yet implemented".to_string());
                    }
                    _ => return Err("Invalid assignment target".to_string()),
                }
            }
            Stmt::Return(expr) => {
                if let Some(e) = expr {
                    // Evaluate expression into a0 (return value register)
                    self.gen_expr(e, "a0")?;
                }
                self.emit("    ret".to_string());
            }
            Stmt::Expr(e) => {
                // Evaluate expression for side effects, result in t0
                self.gen_expr(e, "t0")?;
            }
            Stmt::Cond(cond, then_stmt, else_stmt) => {
                let else_label = self.fresh_label("else");
                let end_label = self.fresh_label("endif");

                // Evaluate condition into t0
                self.gen_expr(cond, "t0")?;

                // Jump to else if condition is false (zero)
                self.emit(format!("    eqi t0 t0 0"));
                self.emit(format!("    if t0 {}", else_label));

                // Then branch
                self.gen_stmt(then_stmt)?;
                self.emit(format!("    jump {}", end_label));

                // Else branch
                self.emit(format!("{}:", else_label));
                if let Some(else_s) = else_stmt {
                    self.gen_stmt(else_s)?;
                }

                self.emit(format!("{}:", end_label));
            }
            Stmt::Loop(cond, body) => {
                let loop_label = self.fresh_label("loop");
                let end_label = self.fresh_label("endloop");

                self.emit(format!("{}:", loop_label));

                // Evaluate condition
                self.gen_expr(cond, "t0")?;

                // Exit if condition is false
                self.emit(format!("    eqi t1 t0 0"));
                self.emit(format!("    if t1 {}", end_label));

                // Loop body
                self.gen_stmt(body)?;

                // Jump back to loop start
                self.emit(format!("    jump {}", loop_label));

                self.emit(format!("{}:", end_label));
            }
            Stmt::Error => return Err("Cannot generate code for error statement".to_string()),
        }
        Ok(())
    }

    fn gen_var_init(&mut self, name: &str, typ: &Type, init: &Expr) -> Result<(), String> {
        let var = self
            .locals
            .get(name)
            .ok_or(format!("Variable not found: {}", name))?
            .clone();

        match (typ, init) {
            (Type::Struct(fields), Expr::StructLit(field_values)) => {
                // Initialize struct fields
                let mut offset = var.offset;
                for (field_name, field_type) in fields {
                    if let Some((_, value)) = field_values.iter().find(|(n, _)| n == field_name) {
                        // Evaluate field value into t0
                        self.gen_expr(value, "t0")?;
                        self.emit(format!("    store t0 sp 0x{:04X}", offset));
                        offset += 1; // Each field is 1 word for now
                    }
                }
            }
            (Type::Array(_size, _elem_type), Expr::ArrayLit(elems)) => {
                // Initialize array elements
                let mut offset = var.offset;
                for elem in elems {
                    self.gen_expr(elem, "t0")?;
                    self.emit(format!("    store t0 sp 0x{:04X}", offset));
                    offset += 1;
                }
            }
            _ => {
                // Simple scalar initialization
                self.gen_expr(init, "t0")?;
                self.emit(format!("    store t0 sp 0x{:04X}", var.offset));
            }
        }
        Ok(())
    }

    fn gen_expr(&mut self, expr: &Expr, dest_reg: &str) -> Result<(), String> {
        match expr {
            Expr::NumberLit(n) => {
                self.emit(format!("    loadi {} 0x{:04X}", dest_reg, n));
            }
            Expr::Ident(name) => {
                if let Some(var) = self.locals.get(name) {
                    self.emit(format!("    load {} sp 0x{:04X}", dest_reg, var.offset));
                } else {
                    return Err(format!("Undefined variable: {}", name));
                }
            }
            Expr::Binary(op, lhs, rhs) => {
                // Evaluate LHS into t1
                self.gen_expr(lhs, "t1")?;
                // Evaluate RHS into t2
                self.gen_expr(rhs, "t2")?;

                // Perform operation
                let inst = match op {
                    BinaryOp::Add => "add",
                    BinaryOp::Sub => "sub",
                    BinaryOp::Mul => {
                        return Err("Multiplication not directly supported".to_string())
                    }
                    BinaryOp::Div => return Err("Division not directly supported".to_string()),
                    BinaryOp::Mod => return Err("Modulus not directly supported".to_string()),
                    BinaryOp::And => "and",
                    BinaryOp::Or => "or",
                    BinaryOp::Xor => "xor",
                    BinaryOp::Eq => "eq",
                    BinaryOp::Ne => "neq",
                    BinaryOp::Lt => "lt",
                    BinaryOp::Le => {
                        // a <= b is !(a > b) which is !(b < a)
                        self.emit(format!("    lt {} t2 t1", dest_reg));
                        self.emit(format!("    not {} {}", dest_reg, dest_reg));
                        return Ok(());
                    }
                    BinaryOp::Gt => {
                        // a > b is b < a
                        self.emit(format!("    lt {} t2 t1", dest_reg));
                        return Ok(());
                    }
                    BinaryOp::Ge => {
                        // a >= b is !(a < b)
                        self.emit(format!("    lt {} t1 t2", dest_reg));
                        self.emit(format!("    not {} {}", dest_reg, dest_reg));
                        return Ok(());
                    }
                    BinaryOp::Shl => "sl",
                    BinaryOp::Shr => "sr",
                };

                self.emit(format!("    {} {} t1 t2", inst, dest_reg));
            }
            Expr::Unary(op, operand) => {
                self.gen_expr(operand, "t1")?;

                match op {
                    UnaryOp::Neg => {
                        // -x = 0 - x
                        self.emit(format!("    subi {} z {}", dest_reg, 0));
                        self.emit(format!("    sub {} {} t1", dest_reg, dest_reg));
                    }
                    UnaryOp::Not => {
                        self.emit(format!("    not {} t1", dest_reg));
                    }
                    UnaryOp::Pos => {
                        // +x = x
                        if dest_reg != "t1" {
                            self.emit(format!("    mov {} t1", dest_reg));
                        }
                    }
                    UnaryOp::Deref => {
                        // *x - load from address in t1
                        self.emit(format!("    load {} t1 0", dest_reg));
                    }
                    UnaryOp::Ref => {
                        // &x - get address - not yet implemented
                        return Err("Address-of operator not yet implemented".to_string());
                    }
                }
            }
            Expr::Member(obj, field) => {
                // Evaluate object to get base address
                match obj.as_ref() {
                    Expr::Ident(var_name) => {
                        if let Some(var) = self.locals.get(var_name) {
                            if let Type::Struct(fields) = &var.typ {
                                // Find field offset
                                let mut field_offset = 0;
                                let mut found = false;
                                for (fname, _ftype) in fields {
                                    if fname == field {
                                        found = true;
                                        break;
                                    }
                                    field_offset += 1;
                                }
                                if found {
                                    let total_offset = var.offset + field_offset;
                                    self.emit(format!(
                                        "    load {} sp 0x{:04X}",
                                        dest_reg, total_offset
                                    ));
                                } else {
                                    return Err(format!("Field not found: {}", field));
                                }
                            } else {
                                return Err(format!("Not a struct: {}", var_name));
                            }
                        } else {
                            return Err(format!("Variable not found: {}", var_name));
                        }
                    }
                    _ => return Err("Complex member access not yet implemented".to_string()),
                }
            }
            Expr::Call(func, args) => {
                // Save arguments in registers or stack
                for (i, arg) in args.iter().enumerate() {
                    if i == 0 {
                        self.gen_expr(arg, "a0")?;
                    } else if i == 1 {
                        self.gen_expr(arg, "a1")?;
                    } else {
                        // TODO: Push to stack for more arguments
                        return Err("More than 2 arguments not yet supported".to_string());
                    }
                }

                // Call function
                match func.as_ref() {
                    Expr::Ident(name) => {
                        self.emit(format!("    call {}", name));
                        // Result is in a0, move to destination if needed
                        if dest_reg != "a0" {
                            self.emit(format!("    mov {} a0", dest_reg));
                        }
                    }
                    _ => return Err("Indirect calls not yet implemented".to_string()),
                }
            }
            Expr::Index(arr, idx) => {
                // Array access: arr[idx]
                match arr.as_ref() {
                    Expr::Ident(var_name) => {
                        let var_offset = if let Some(var) = self.locals.get(var_name) {
                            var.offset
                        } else {
                            return Err(format!("Variable not found: {}", var_name));
                        };

                        // Evaluate index into t1
                        self.gen_expr(idx, "t1")?;
                        // Calculate offset: base_offset + index
                        self.emit(format!("    addi t2 sp 0x{:04X}", var_offset));
                        self.emit(format!("    add t2 t2 t1"));
                        // Load from calculated address
                        self.emit(format!("    load {} t2 0", dest_reg));
                    }
                    _ => return Err("Complex array access not yet implemented".to_string()),
                }
            }
            Expr::Cast(e, target_type) => {
                // For now, just evaluate the expression (no type conversion)
                self.gen_expr(e, dest_reg)?;
            }
            Expr::StructLit(_) => {
                return Err(
                    "Struct literals should only appear in variable initializers".to_string(),
                );
            }
            Expr::ArrayLit(_) => {
                return Err(
                    "Array literals should only appear in variable initializers".to_string()
                );
            }
            Expr::CharLit(c) => {
                self.emit(format!("    loadi {} 0x{:04X}", dest_reg, *c as usize));
            }
            Expr::StringLit(s) => {
                // TODO: String literals need to be stored in data section
                return Err("String literals not yet implemented".to_string());
            }
            Expr::Cond(cond, then_expr, else_expr) => {
                let else_label = self.fresh_label("ternary_else");
                let end_label = self.fresh_label("ternary_end");

                self.gen_expr(cond, "t0")?;
                self.emit(format!("    eqi t0 t0 0"));
                self.emit(format!("    if t0 {}", else_label));

                self.gen_expr(then_expr, dest_reg)?;
                self.emit(format!("    jump {}", end_label));

                self.emit(format!("{}:", else_label));
                self.gen_expr(else_expr, dest_reg)?;

                self.emit(format!("{}:", end_label));
            }
            Expr::Error => return Err("Cannot generate code for error expression".to_string()),
        }
        Ok(())
    }

    fn type_size(&self, typ: &Type) -> usize {
        match typ {
            Type::Int => 1,
            Type::Addr(_) => 1,
            Type::Custom(name) => {
                // Look up the type in globals
                if let Some(Global::Type(t)) = self.globals.0.get(name) {
                    self.type_size(t)
                } else {
                    1 // Default size
                }
            }
            Type::Array(size, elem_type) => size * self.type_size(elem_type),
            Type::Struct(fields) => fields.iter().map(|(_, t)| self.type_size(t)).sum(),
            Type::Func(_, _) => 1, // Function pointer
            Type::Error => 0,
        }
    }

    // Infer the type of an expression
    fn infer_expr_type(&self, expr: &Expr) -> Result<Type, String> {
        match expr {
            Expr::NumberLit(_) => Ok(Type::Int),
            Expr::CharLit(_) => Ok(Type::Int), // Char is treated as int
            Expr::StringLit(_) => Ok(Type::Addr(Box::new(Type::Int))), // String is *int
            Expr::Ident(name) => {
                if let Some(var) = self.locals.get(name) {
                    Ok(var.typ.clone())
                } else {
                    Err(format!("Variable not found: {}", name))
                }
            }
            Expr::Call(func, _args) => {
                // Get function return type from globals
                match func.as_ref() {
                    Expr::Ident(name) => {
                        if let Some(Global::Func(_args, ret_type, _body)) = self.globals.0.get(name)
                        {
                            Ok(ret_type.clone())
                        } else {
                            Err(format!("Function not found: {}", name))
                        }
                    }
                    _ => Err("Cannot infer type of indirect call".to_string()),
                }
            }
            Expr::Binary(_op, lhs, _rhs) => {
                // For now, assume binary operations preserve the type of LHS
                self.infer_expr_type(lhs)
            }
            Expr::Unary(_op, operand) => self.infer_expr_type(operand),
            Expr::Member(obj, field) => {
                let obj_type = self.infer_expr_type(obj)?;
                match obj_type {
                    Type::Struct(fields) => {
                        for (fname, ftype) in fields {
                            if fname == *field {
                                return Ok(ftype.clone());
                            }
                        }
                        Err(format!("Field '{}' not found in struct", field))
                    }
                    _ => Err("Member access on non-struct type".to_string()),
                }
            }
            Expr::Index(arr, _idx) => {
                let arr_type = self.infer_expr_type(arr)?;
                match arr_type {
                    Type::Array(_size, elem_type) => Ok(*elem_type),
                    _ => Err("Index access on non-array type".to_string()),
                }
            }
            Expr::Cast(_e, target_type) => self.resolve_type(target_type),
            Expr::StructLit(_fields) => {
                // Can't easily infer struct type without more context
                Err("Cannot infer type of struct literal".to_string())
            }
            Expr::ArrayLit(_elems) => {
                // Can't easily infer array type without more context
                Err("Cannot infer type of array literal".to_string())
            }
            Expr::Cond(_cond, then_expr, _else_expr) => self.infer_expr_type(then_expr),
            Expr::Error => Err("Cannot infer type of error expression".to_string()),
        }
    }

    // Convert ast::Type to global::Type
    fn resolve_type(&self, ast_type: &ast::Type) -> Result<Type, String> {
        match ast_type {
            ast::Type::Int => Ok(Type::Int),
            ast::Type::Custom(name) => {
                // Look up in globals
                if let Some(Global::Type(t)) = self.globals.0.get(name) {
                    Ok(t.clone())
                } else {
                    Err(format!("Unknown type: {}", name))
                }
            }
            ast::Type::Addr(inner) => {
                let inner_type = self.resolve_type(inner)?;
                Ok(Type::Addr(Box::new(inner_type)))
            }
            ast::Type::Array(len_expr, elem_type) => {
                // Evaluate length expression
                let len = match len_expr {
                    Expr::NumberLit(n) => *n,
                    _ => return Err("Array length must be a constant".to_string()),
                };
                let elem = self.resolve_type(elem_type)?;
                Ok(Type::Array(len, Box::new(elem)))
            }
            ast::Type::Struct(fields) => {
                let mut resolved_fields = Vec::new();
                for (name, field_type) in fields {
                    let resolved = self.resolve_type(field_type)?;
                    resolved_fields.push((name.clone(), resolved));
                }
                Ok(Type::Struct(resolved_fields))
            }
            ast::Type::Func(params, ret) => {
                let mut resolved_params = Vec::new();
                for (name, param_type) in params {
                    let resolved = self.resolve_type(param_type)?;
                    resolved_params.push((name.clone(), resolved));
                }
                let resolved_ret = self.resolve_type(ret)?;
                Ok(Type::Func(resolved_params, Box::new(resolved_ret)))
            }
            ast::Type::Error => Ok(Type::Error),
        }
    }
}
