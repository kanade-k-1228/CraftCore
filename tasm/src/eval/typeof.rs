use crate::{
    error::CollectError,
    eval::normtype::NormType,
    grammer::ast::{BinaryOp, Expr, UnaryOp},
};

pub fn typeof_expr(
    expr: &Expr,
    env: &impl Fn(&str) -> Option<NormType>,
) -> Result<NormType, CollectError> {
    match expr {
        Expr::NumberLit(_) => Ok(NormType::Int),
        Expr::CharLit(_) => Ok(NormType::Int),
        Expr::StringLit(s) => Ok(NormType::Array(s.len(), Box::new(NormType::Int))),

        Expr::StructLit(fields) => {
            let mut ret = Vec::new();
            for (name, expr) in fields {
                ret.push((name.clone(), typeof_expr(expr, env)?));
            }
            Ok(NormType::Struct(ret))
        }

        Expr::ArrayLit(elems) => {
            if elems.is_empty() {
                return Err(CollectError::CannotInferTypeOfEmptyArray);
            }
            let elem_type = typeof_expr(&elems[0], env)?;
            Ok(NormType::Array(elems.len(), Box::new(elem_type)))
        }

        Expr::Ident(name) => {
            env(name).ok_or_else(|| CollectError::UnsupportedConstExpr(format!("{:?}", expr)))
        }

        Expr::Unary(op, operand) => match op {
            UnaryOp::Pos | UnaryOp::Neg | UnaryOp::Not => {
                let operand_type = typeof_expr(operand, env)?;
                match operand_type {
                    NormType::Int => Ok(NormType::Int),
                    _ => Err(CollectError::UnsupportedConstExpr(format!("{:?}", expr))),
                }
            }
            UnaryOp::Deref => {
                let operand_type = typeof_expr(operand, env)?;
                match operand_type {
                    NormType::Addr(inner) => Ok(*inner),
                    _ => Err(CollectError::UnsupportedConstExpr(format!("{:?}", expr))),
                }
            }
            UnaryOp::Ref => {
                let operand_type = typeof_expr(operand, env)?;
                Ok(NormType::Addr(Box::new(operand_type)))
            }
        },

        Expr::Binary(op, lhs, rhs) => {
            let lhs_type = typeof_expr(lhs, env)?;
            let rhs_type = typeof_expr(rhs, env)?;

            match op {
                BinaryOp::Add
                | BinaryOp::Sub
                | BinaryOp::Mul
                | BinaryOp::Div
                | BinaryOp::Mod
                | BinaryOp::And
                | BinaryOp::Or
                | BinaryOp::Xor
                | BinaryOp::Shl
                | BinaryOp::Shr => match (lhs_type, rhs_type) {
                    (NormType::Int, NormType::Int) => Ok(NormType::Int),
                    _ => Err(CollectError::UnsupportedConstExpr(format!("{:?}", expr))),
                },
                BinaryOp::Eq
                | BinaryOp::Ne
                | BinaryOp::Lt
                | BinaryOp::Le
                | BinaryOp::Gt
                | BinaryOp::Ge => match (lhs_type, rhs_type) {
                    (NormType::Int, NormType::Int) => Ok(NormType::Int),
                    _ => Err(CollectError::UnsupportedConstExpr(format!("{:?}", expr))),
                },
            }
        }

        Expr::Member(base, field) => {
            let base_type = typeof_expr(base, env)?;
            match base_type {
                NormType::Struct(fields) => {
                    for (name, ty) in fields {
                        if name == *field {
                            return Ok(ty);
                        }
                    }
                    Err(CollectError::UnsupportedConstExpr(format!("{:?}", expr)))
                }
                _ => Err(CollectError::UnsupportedConstExpr(format!("{:?}", expr))),
            }
        }

        Expr::Index(base, _index) => {
            let base_type = typeof_expr(base, env)?;
            match base_type {
                NormType::Array(_, elem_type) => Ok(*elem_type),
                _ => Err(CollectError::UnsupportedConstExpr(format!("{:?}", expr))),
            }
        }

        Expr::Cond(_cond, true_expr, _false_expr) => {
            // Return type of true branch (both branches should have same type)
            typeof_expr(true_expr, env)
        }

        Expr::Cast(_expr, typ) => {
            // Convert ast::Type to NormType
            // For now, simplified - you may need to call collect_type here
            match typ.as_ref() {
                crate::grammer::ast::Type::Int => Ok(NormType::Int),
                crate::grammer::ast::Type::Addr(inner) => {
                    let inner_type = match inner.as_ref() {
                        crate::grammer::ast::Type::Int => NormType::Int,
                        _ => return Err(CollectError::UnsupportedConstExpr(format!("{:?}", expr))),
                    };
                    Ok(NormType::Addr(Box::new(inner_type)))
                }
                _ => Err(CollectError::UnsupportedConstExpr(format!("{:?}", expr))),
            }
        }

        Expr::Call(func, _args) => {
            let func_type = typeof_expr(func, env)?;
            match func_type {
                NormType::Func(_, ret) => Ok(*ret),
                _ => Err(CollectError::UnsupportedConstExpr(format!("{:?}", expr))),
            }
        }

        Expr::Sizeof(_) => {
            // sizeof expression always returns an integer (the size)
            Ok(NormType::Int)
        }

        Expr::Error => Err(CollectError::UnsupportedConstExpr(format!("{:?}", expr))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::grammer::ast::{Expr, Type};

    #[test]
    fn test_typeof_number_literal() {
        let expr = Expr::NumberLit(42);
        let env = |_: &str| None;
        let result = typeof_expr(&expr, &env).unwrap();
        assert!(matches!(result, NormType::Int));
    }

    #[test]
    fn test_typeof_char_literal() {
        let expr = Expr::CharLit('A');
        let env = |_: &str| None;
        let result = typeof_expr(&expr, &env).unwrap();
        assert!(matches!(result, NormType::Int));
    }

    #[test]
    fn test_typeof_string_literal() {
        let expr = Expr::StringLit("hello".to_string());
        let env = |_: &str| None;
        let result = typeof_expr(&expr, &env).unwrap();
        match result {
            NormType::Array(len, elem_type) => {
                assert_eq!(len, 5);
                assert!(matches!(*elem_type, NormType::Int));
            }
            _ => panic!("Expected array type"),
        }
    }

    #[test]
    fn test_typeof_addition() {
        let expr = Expr::Binary(
            BinaryOp::Add,
            Box::new(Expr::NumberLit(10)),
            Box::new(Expr::NumberLit(32)),
        );
        let env = |_: &str| None;
        let result = typeof_expr(&expr, &env).unwrap();
        assert!(matches!(result, NormType::Int));
    }

    #[test]
    fn test_typeof_negation() {
        let expr = Expr::Unary(UnaryOp::Neg, Box::new(Expr::NumberLit(1)));
        let env = |_: &str| None;
        let result = typeof_expr(&expr, &env).unwrap();
        assert!(matches!(result, NormType::Int));
    }

    #[test]
    fn test_typeof_identifier() {
        let expr = Expr::Ident("x".to_string());
        let env = |name: &str| {
            if name == "x" {
                Some(NormType::Int)
            } else {
                None
            }
        };
        let result = typeof_expr(&expr, &env).unwrap();
        assert!(matches!(result, NormType::Int));
    }

    #[test]
    fn test_typeof_struct_literal() {
        let expr = Expr::StructLit(vec![
            ("x".to_string(), Expr::NumberLit(10)),
            ("y".to_string(), Expr::NumberLit(20)),
        ]);
        let env = |_: &str| None;
        let result = typeof_expr(&expr, &env).unwrap();
        match result {
            NormType::Struct(fields) => {
                assert_eq!(fields.len(), 2);
                assert_eq!(fields[0].0, "x");
                assert!(matches!(fields[0].1, NormType::Int));
                assert_eq!(fields[1].0, "y");
                assert!(matches!(fields[1].1, NormType::Int));
            }
            _ => panic!("Expected struct type"),
        }
    }

    #[test]
    fn test_typeof_array_literal() {
        let expr = Expr::ArrayLit(vec![
            Expr::NumberLit(1),
            Expr::NumberLit(2),
            Expr::NumberLit(3),
        ]);
        let env = |_: &str| None;
        let result = typeof_expr(&expr, &env).unwrap();
        match result {
            NormType::Array(len, elem_type) => {
                assert_eq!(len, 3);
                assert!(matches!(*elem_type, NormType::Int));
            }
            _ => panic!("Expected array type"),
        }
    }

    #[test]
    fn test_typeof_member_access() {
        let struct_expr = Expr::StructLit(vec![
            ("x".to_string(), Expr::NumberLit(42)),
            ("y".to_string(), Expr::NumberLit(99)),
        ]);
        let expr = Expr::Member(Box::new(struct_expr), "y".to_string());
        let env = |_: &str| None;
        let result = typeof_expr(&expr, &env).unwrap();
        assert!(matches!(result, NormType::Int));
    }

    #[test]
    fn test_typeof_array_index() {
        let array = Expr::ArrayLit(vec![
            Expr::NumberLit(100),
            Expr::NumberLit(200),
            Expr::NumberLit(300),
        ]);
        let expr = Expr::Index(Box::new(array), Box::new(Expr::NumberLit(1)));
        let env = |_: &str| None;
        let result = typeof_expr(&expr, &env).unwrap();
        assert!(matches!(result, NormType::Int));
    }

    #[test]
    fn test_typeof_ref() {
        let expr = Expr::Unary(UnaryOp::Ref, Box::new(Expr::NumberLit(42)));
        let env = |_: &str| None;
        let result = typeof_expr(&expr, &env).unwrap();
        match result {
            NormType::Addr(inner) => {
                assert!(matches!(*inner, NormType::Int));
            }
            _ => panic!("Expected address type"),
        }
    }

    #[test]
    fn test_typeof_deref() {
        let expr = Expr::Ident("ptr".to_string());
        let env = |name: &str| {
            if name == "ptr" {
                Some(NormType::Addr(Box::new(NormType::Int)))
            } else {
                None
            }
        };

        let deref_expr = Expr::Unary(UnaryOp::Deref, Box::new(expr));
        let result = typeof_expr(&deref_expr, &env).unwrap();
        assert!(matches!(result, NormType::Int));
    }

    #[test]
    fn test_typeof_cast() {
        let expr = Expr::Cast(Box::new(Expr::NumberLit(42)), Box::new(Type::Int));
        let env = |_: &str| None;
        let result = typeof_expr(&expr, &env).unwrap();
        assert!(matches!(result, NormType::Int));
    }
}
