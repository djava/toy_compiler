use crate::{ast::*, passes::ASTPass};

pub struct TypeCheck;

impl ASTPass for TypeCheck {
    fn run_pass(self, mut m: Module) -> Module {
        type_check_statements(&m.body[..], &mut m.types);

        m
    }
}

fn type_check_expr(e: &Expr, env: &mut TypeEnv) -> ValueType {
    use Expr::*;

    match e {
        BinaryOp(left, op, right) => {
            let l_type = type_check_expr(&*left, env);
            let r_type = type_check_expr(&*right, env);

            let result_type = op.type_of(&l_type, &r_type).unwrap();
            result_type
        }
        UnaryOp(_op, exp) => {
            let exp_type = type_check_expr(&*exp, env);
            assert_eq!(exp_type, ValueType::IntType);
            ValueType::IntType
        }
        Id(id) => env
            .get(id)
            .expect(format!("Unknown Identifier: {id:?}").as_str())
            .clone(),
        Constant(v) => ValueType::from(v),
        Call(id, args) => match id {
            Identifier::Named(name) => {
                if name.as_ref() == "read_int" {
                    assert!(
                        args.is_empty(),
                        "Passed {} of args to {name}, expected 0",
                        args.len()
                    );
                    ValueType::IntType
                } else if name.as_ref() == "print_int" {
                    assert_eq!(
                        args.len(),
                        1,
                        "Passed {} args to {name}, expected 1",
                        args.len()
                    );
                    assert_eq!(
                        type_check_expr(&args[0], env),
                        ValueType::IntType,
                        "Passed wrong arg type to {name}, expected I64"
                    );
                    ValueType::NoneType
                } else if name.as_ref() == "len" {
                    assert_eq!(
                        args.len(),
                        1,
                        "Passed {} args to {name}, expected 1",
                        args.len()
                    );
                    assert!(
                        matches!(type_check_expr(&args[0], env), ValueType::TupleType(_)),
                        "Passed wrong arg type to {name}, expected tuple"
                    );
                    ValueType::IntType
                } else {
                    unimplemented!("Unknown function name")
                }
            }
            _ => unimplemented!("Unknown function name"),
        },
        Ternary(cond, pos, neg) => {
            let cond_type = type_check_expr(&*cond, env);
            assert!([ValueType::BoolType, ValueType::IntType].contains(&cond_type));

            let pos_type = type_check_expr(&*pos, env);
            let neg_type = type_check_expr(&*neg, env);
            assert_eq!(
                pos_type, neg_type,
                "Both branches of a ternary must be the same type"
            );

            pos_type
        }
        StatementBlock(statements, expr) => {
            type_check_statements(statements, env);

            type_check_expr(expr, env)
        }
        Tuple(elements) => {
            let element_types: Vec<_> = elements.iter().map(|e| type_check_expr(e, env)).collect();

            ValueType::TupleType(element_types)
        }
        Subscript(tup, index) => {
            if let ValueType::TupleType(elems) = type_check_expr(tup, env) {
                if let Value::I64(idx) = index {
                    assert!(*idx < elems.len() as i64, "Indexed tuple out of bounds");
                    elems[*idx as usize].clone()
                } else {
                    panic!("Used non-i64 value as subscripting index")
                }
            } else {
                panic!("Subscripted a non-tuple")
            }
        }
    }
}

fn type_check_statements(statements: &[Statement], env: &mut TypeEnv) {
    use Statement::*;

    if !statements.is_empty() {
        match &statements[0] {
            Assign(dest, e) => {
                let t = type_check_expr(e, env);
                if env.contains_key(&dest) {
                    assert_eq!(env[&dest], t)
                } else {
                    env.insert(dest.clone(), t);
                }
                type_check_statements(&statements[1..], env);
            }
            Expr(e) => {
                type_check_expr(e, env);
                type_check_statements(&statements[1..], env);
            }
            Conditional(cond, pos, neg) => {
                let cond_type = type_check_expr(cond, env);
                assert!([ValueType::BoolType, ValueType::IntType].contains(&cond_type));

                type_check_statements(pos, env);
                type_check_statements(neg, env);
            }
            WhileLoop(cond, body) => {
                let cond_type = type_check_expr(cond, env);
                assert!([ValueType::BoolType, ValueType::IntType].contains(&cond_type));

                type_check_statements(body, env);
            }
        }
    }
}
