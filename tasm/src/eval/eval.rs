use crate::{
    error::CollectError,
    eval::constexpr::ConstExpr,
    eval::normtype::collect_type,
    grammer::ast::{BinaryOp, Expr, UnaryOp},
    symbols::{ConstMap, TypeMap},
};

pub fn eval(
    expr: &Expr,
    env: &impl Fn(&str) -> Option<ConstExpr>,
) -> Result<ConstExpr, CollectError> {
    eval_with_types(
        expr,
        env,
        &ConstMap(Default::default()),
        &TypeMap(Default::default()),
    )
}

pub fn eval_with_types(
    expr: &Expr,
    env: &impl Fn(&str) -> Option<ConstExpr>,
    consts: &ConstMap,
    types: &TypeMap,
) -> Result<ConstExpr, CollectError> {
    match expr {
        Expr::NumberLit(n) => Ok(ConstExpr::Number(*n)),
        Expr::CharLit(c) => Ok(ConstExpr::Char(*c)),
        Expr::StringLit(s) => Ok(ConstExpr::String(s.clone())),

        Expr::StructLit(fields) => {
            let mut ret = Vec::new();
            for (name, expr) in fields {
                ret.push((name.clone(), eval_with_types(expr, env, consts, types)?));
            }
            Ok(ConstExpr::Struct(ret))
        }

        Expr::ArrayLit(elems) => {
            let mut ret = Vec::new();
            for expr in elems {
                ret.push(eval_with_types(expr, env, consts, types)?);
            }
            Ok(ConstExpr::Array(ret))
        }

        Expr::Ident(name) => {
            env(name).ok_or_else(|| CollectError::UnsupportedConstExpr(format!("{:?}", expr)))
        }

        Expr::Unary(op, operand) => match (op, eval_with_types(operand, env, consts, types)?) {
            (UnaryOp::Pos, ConstExpr::Number(n)) => Ok(ConstExpr::Number(n)),
            (UnaryOp::Neg, ConstExpr::Number(n)) => {
                Ok(ConstExpr::Number(0usize.wrapping_sub(n) & 0xFFFF))
            }
            (UnaryOp::Not, ConstExpr::Number(n)) => Ok(ConstExpr::Number((!n) & 0xFFFF)),
            _ => Err(CollectError::UnsupportedConstExpr(format!("{:?}", expr))),
        },

        Expr::Addr(_) | Expr::Deref(_) => {
            // Address-of and dereference operations are not supported in const expressions
            Err(CollectError::UnsupportedConstExpr(format!("{:?}", expr)))
        }

        Expr::Binary(op, lhs, rhs) => match (
            op,
            eval_with_types(lhs, env, consts, types)?,
            eval_with_types(rhs, env, consts, types)?,
        ) {
            (BinaryOp::Add, ConstExpr::Number(l), ConstExpr::Number(r)) => {
                Ok(ConstExpr::Number((l.wrapping_add(r)) & 0xFFFF))
            }
            (BinaryOp::Sub, ConstExpr::Number(l), ConstExpr::Number(r)) => {
                Ok(ConstExpr::Number((l.wrapping_sub(r)) & 0xFFFF))
            }
            (BinaryOp::Mul, ConstExpr::Number(l), ConstExpr::Number(r)) => {
                Ok(ConstExpr::Number((l.wrapping_mul(r)) & 0xFFFF))
            }
            (BinaryOp::Div, ConstExpr::Number(l), ConstExpr::Number(r)) => match r {
                0 => Err(CollectError::UnsupportedConstExpr(format!("{:?}", expr))),
                r => Ok(ConstExpr::Number(l / r)),
            },
            (BinaryOp::Mod, ConstExpr::Number(l), ConstExpr::Number(r)) => match r {
                0 => Err(CollectError::UnsupportedConstExpr(format!("{:?}", expr))),
                r => Ok(ConstExpr::Number(l % r)),
            },
            (BinaryOp::And, ConstExpr::Number(l), ConstExpr::Number(r)) => {
                Ok(ConstExpr::Number((l & r) & 0xFFFF))
            }
            (BinaryOp::Or, ConstExpr::Number(l), ConstExpr::Number(r)) => {
                Ok(ConstExpr::Number((l | r) & 0xFFFF))
            }
            (BinaryOp::Xor, ConstExpr::Number(l), ConstExpr::Number(r)) => {
                Ok(ConstExpr::Number((l ^ r) & 0xFFFF))
            }
            (BinaryOp::Shl, ConstExpr::Number(l), ConstExpr::Number(r)) => {
                Ok(ConstExpr::Number((l << (r & 0xF)) & 0xFFFF))
            }
            (BinaryOp::Shr, ConstExpr::Number(l), ConstExpr::Number(r)) => {
                Ok(ConstExpr::Number((l >> (r & 0xF)) & 0xFFFF))
            }
            (BinaryOp::Eq, ConstExpr::Number(l), ConstExpr::Number(r)) => {
                Ok(ConstExpr::Number(if l == r { 1 } else { 0 }))
            }
            (BinaryOp::Ne, ConstExpr::Number(l), ConstExpr::Number(r)) => {
                Ok(ConstExpr::Number(if l != r { 1 } else { 0 }))
            }
            (BinaryOp::Lt, ConstExpr::Number(l), ConstExpr::Number(r)) => {
                Ok(ConstExpr::Number(if l < r { 1 } else { 0 }))
            }
            (BinaryOp::Le, ConstExpr::Number(l), ConstExpr::Number(r)) => {
                Ok(ConstExpr::Number(if l <= r { 1 } else { 0 }))
            }
            (BinaryOp::Gt, ConstExpr::Number(l), ConstExpr::Number(r)) => {
                Ok(ConstExpr::Number(if l > r { 1 } else { 0 }))
            }
            (BinaryOp::Ge, ConstExpr::Number(l), ConstExpr::Number(r)) => {
                Ok(ConstExpr::Number(if l >= r { 1 } else { 0 }))
            }
            _ => Err(CollectError::UnsupportedConstExpr(format!("{:?}", expr))),
        },

        Expr::Member(base, field) => match eval_with_types(base, env, consts, types)? {
            ConstExpr::Struct(fields) => {
                for (name, value) in fields {
                    if name == *field {
                        return Ok(value);
                    }
                }
                Err(CollectError::UnsupportedConstExpr(format!("{:?}", expr)))
            }
            _ => Err(CollectError::UnsupportedConstExpr(format!("{:?}", expr))),
        },

        Expr::Index(base, index) => match (
            eval_with_types(base, env, consts, types)?,
            eval_with_types(index, env, consts, types)?,
        ) {
            (ConstExpr::Array(elems), ConstExpr::Number(idx)) => match idx < elems.len() {
                true => Ok(elems[idx].clone()),
                false => Err(CollectError::UnsupportedConstExpr(format!("{:?}", expr))),
            },
            (ConstExpr::String(s), ConstExpr::Number(idx)) => match idx < s.len() {
                true => Ok(ConstExpr::Char(s.chars().nth(idx).unwrap())),
                false => Err(CollectError::UnsupportedConstExpr(format!("{:?}", expr))),
            },
            _ => Err(CollectError::UnsupportedConstExpr(format!("{:?}", expr))),
        },

        Expr::Cond(cond, true_expr, false_expr) => match eval_with_types(cond, env, consts, types)?
        {
            ConstExpr::Number(n) => match n != 0 {
                true => eval_with_types(true_expr, env, consts, types),
                false => eval_with_types(false_expr, env, consts, types),
            },
            _ => Err(CollectError::UnsupportedConstExpr(format!("{:?}", expr))),
        },

        Expr::Cast(expr, _typ) => eval_with_types(expr, env, consts, types),

        Expr::Sizeof(typ) => {
            let norm_type = collect_type(typ, consts, types)?;
            Ok(ConstExpr::Number(norm_type.sizeof()))
        }

        Expr::Call(_, _) => Err(CollectError::UnsupportedConstExpr(format!("{:?}", expr))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::grammer::ast::Type;

    #[test]
    fn test_eval_number_literal() {
        let expr = Expr::NumberLit(42);
        let env = |_: &str| None;
        let result = eval(&expr, &env).unwrap();
        assert!(matches!(result, ConstExpr::Number(42)));
    }

    #[test]
    fn test_eval_addition() {
        let expr = Expr::Binary(
            BinaryOp::Add,
            Box::new(Expr::NumberLit(10)),
            Box::new(Expr::NumberLit(32)),
        );
        let env = |_: &str| None;
        let result = eval(&expr, &env).unwrap();
        assert!(matches!(result, ConstExpr::Number(42)));
    }

    #[test]
    fn test_eval_negation() {
        let expr = Expr::Unary(UnaryOp::Neg, Box::new(Expr::NumberLit(1)));
        let env = |_: &str| None;
        let result = eval(&expr, &env).unwrap();
        assert!(matches!(result, ConstExpr::Number(0xFFFF)));
    }

    #[test]
    fn test_eval_identifier() {
        let expr = Expr::Ident("FOO".to_string());
        let env = |name: &str| {
            if name == "FOO" {
                Some(ConstExpr::Number(123))
            } else {
                None
            }
        };
        let result = eval(&expr, &env).unwrap();
        assert!(matches!(result, ConstExpr::Number(123)));
    }

    #[test]
    fn test_eval_struct_literal() {
        let expr = Expr::StructLit(vec![
            ("x".to_string(), Expr::NumberLit(10)),
            ("y".to_string(), Expr::NumberLit(20)),
        ]);
        let env = |_: &str| None;
        let result = eval(&expr, &env).unwrap();
        match result {
            ConstExpr::Struct(fields) => {
                assert_eq!(fields.len(), 2);
                assert_eq!(fields[0].0, "x");
                assert!(matches!(fields[0].1, ConstExpr::Number(10)));
                assert_eq!(fields[1].0, "y");
                assert!(matches!(fields[1].1, ConstExpr::Number(20)));
            }
            _ => panic!("Expected struct"),
        }
    }

    #[test]
    fn test_eval_array_index() {
        let array = Expr::ArrayLit(vec![
            Expr::NumberLit(100),
            Expr::NumberLit(200),
            Expr::NumberLit(300),
        ]);
        let expr = Expr::Index(Box::new(array), Box::new(Expr::NumberLit(1)));
        let env = |_: &str| None;
        let result = eval(&expr, &env).unwrap();
        assert!(matches!(result, ConstExpr::Number(200)));
    }

    #[test]
    fn test_eval_member_access() {
        let struct_expr = Expr::StructLit(vec![
            ("x".to_string(), Expr::NumberLit(42)),
            ("y".to_string(), Expr::NumberLit(99)),
        ]);
        let expr = Expr::Member(Box::new(struct_expr), "y".to_string());
        let env = |_: &str| None;
        let result = eval(&expr, &env).unwrap();
        assert!(matches!(result, ConstExpr::Number(99)));
    }

    #[test]
    fn test_eval_conditional_true() {
        let expr = Expr::Cond(
            Box::new(Expr::NumberLit(1)), // condition is true (non-zero)
            Box::new(Expr::NumberLit(42)),
            Box::new(Expr::NumberLit(99)),
        );
        let env = |_: &str| None;
        let result = eval(&expr, &env).unwrap();
        assert!(matches!(result, ConstExpr::Number(42)));
    }

    #[test]
    fn test_eval_conditional_false() {
        let expr = Expr::Cond(
            Box::new(Expr::NumberLit(0)), // condition is false (zero)
            Box::new(Expr::NumberLit(42)),
            Box::new(Expr::NumberLit(99)),
        );
        let env = |_: &str| None;
        let result = eval(&expr, &env).unwrap();
        assert!(matches!(result, ConstExpr::Number(99)));
    }

    #[test]
    fn test_eval_cast() {
        let expr = Expr::Cast(Box::new(Expr::NumberLit(42)), Box::new(Type::Int));
        let env = |_: &str| None;
        let result = eval(&expr, &env).unwrap();
        assert!(matches!(result, ConstExpr::Number(42)));
    }
}
