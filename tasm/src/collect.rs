// collect.rs

use crate::ast::{self, AST};
use crate::global::{self, Global, Globals};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum CollectError {
    TODO,
    Duplicate(String),
    MissingTypeAnnotation(String),
    NonLiteralArrayLength(Box<ast::Expr>),
    UnsupportedConstExpr(Box<ast::Expr>),
}

pub fn collect(ast: AST) -> Result<Globals, CollectError> {
    let AST(defs) = ast;
    let mut globals = HashMap::<String, Global>::new();

    for def in defs {
        match def {
            ast::Def::Type(name, ty) => {
                if globals.contains_key(&name) {
                    return Err(CollectError::Duplicate(name));
                }
                let ty = collect_type(&ty, &globals)?;
                globals.insert(name.clone(), Global::Type(ty));
            }
            ast::Def::Const(name, ty, expr) => {
                if globals.contains_key(&name) {
                    return Err(CollectError::Duplicate(name));
                }
                let ty = ty
                    .as_ref()
                    .ok_or_else(|| CollectError::MissingTypeAnnotation(name.clone()))
                    .and_then(|t| collect_type(t, &globals))?;
                let lit = collect_literal(&expr, &globals)
                    .map_err(|_| CollectError::UnsupportedConstExpr(Box::new(expr.clone())))?;
                globals.insert(name, Global::Const(lit, ty));
            }
            ast::Def::Static(name, addr, ty) => {
                if globals.contains_key(&name) {
                    return Err(CollectError::Duplicate(name));
                }
                let ty = collect_type(&ty, &globals)?;
                let addr = addr.and_then(|e| match e {
                    ast::Expr::NumberLit(n) => Some(n),
                    _ => None,
                });
                globals.insert(name, Global::Static(ty, addr));
            }
            ast::Def::Asm(name, maybe_addr, body) => {
                if globals.contains_key(&name) {
                    return Err(CollectError::Duplicate(name));
                }
                let addr = maybe_addr.and_then(|e| {
                    if let ast::Expr::NumberLit(n) = e {
                        Some(n)
                    } else {
                        None
                    }
                });
                globals.insert(name, Global::Asm(body, addr));
            }
            ast::Def::Func(name, args, ty, body) => {
                if globals.contains_key(&name) {
                    return Err(CollectError::Duplicate(name));
                }
                let args = args
                    .into_iter()
                    .map(|(arg_name, arg_ty)| Ok((arg_name, collect_type(&arg_ty, &globals)?)))
                    .collect::<Result<_, CollectError>>()?;
                let ir_ret = collect_type(&ty, &globals)?;
                globals.insert(name, Global::Func(args, ir_ret, body));
            }
        }
    }

    Ok(Globals(globals))
}

fn collect_type(
    ty: &ast::Type,
    globals: &HashMap<String, Global>,
) -> Result<global::Type, CollectError> {
    match ty {
        ast::Type::Int => Ok(global::Type::Int),
        ast::Type::Custom(name) => match globals.get(name) {
            Some(global) => match global {
                Global::Type(ty) => Ok(ty.clone()),
                _ => Err(CollectError::TODO),
            },
            None => Err(CollectError::TODO),
        },
        ast::Type::Addr(inner) => {
            let ty = collect_type(ty, globals)?;
            Ok(global::Type::Addr(Box::new(ty)))
        }
        ast::Type::Array(len, ty) => {
            let len = match len {
                ast::Expr::NumberLit(n) => *n,
                ast::Expr::Ident(name) => match globals.get(name) {
                    Some(Global::Const(global::Literal::Number(n), _)) => *n,
                    _ => return Err(CollectError::TODO),
                },
                _ => return Err(CollectError::NonLiteralArrayLength(Box::new(len.clone()))),
            };
            let ty = collect_type(ty, globals)?;
            Ok(global::Type::Array(len, Box::new(ty)))
        }
        ast::Type::Struct(fields) => {
            let mut rets = Vec::<(String, global::Type)>::new();
            for (name, ty) in fields {
                let ty = collect_type(&ty, globals)?;
                rets.push((name.clone(), ty));
            }
            Ok(global::Type::Struct(rets))
        }
        ast::Type::Func(params, ret) => {
            let ty = collect_type(ret, globals)?;
            let mut rets = Vec::<(String, global::Type)>::new();
            for (name, ty) in params {
                let ty = collect_type(&ty, globals)?;
                rets.push((name.clone(), ty));
            }
            Ok(global::Type::Func(rets, Box::new(ty)))
        }
        ast::Type::Error => Ok(global::Type::Error),
    }
}

fn collect_literal(
    expr: &ast::Expr,
    globals: &HashMap<String, Global>,
) -> Result<global::Literal, CollectError> {
    Ok(match expr {
        ast::Expr::NumberLit(n) => global::Literal::Number(*n),
        ast::Expr::CharLit(c) => global::Literal::Char(*c),
        ast::Expr::StringLit(s) => global::Literal::String(s.clone()),
        ast::Expr::StructLit(fields) => global::Literal::Struct(
            fields
                .iter()
                .map(|(name, expr)| Ok((name.clone(), collect_literal(expr, globals)?)))
                .collect::<Result<_, _>>()?,
        ),
        ast::Expr::ArrayLit(elems) => global::Literal::Array(
            elems
                .iter()
                .map(|expr| collect_literal(expr, globals))
                .collect::<Result<_, _>>()?,
        ),
        _ => return Err(CollectError::UnsupportedConstExpr(Box::new(expr.clone()))),
    })
}
