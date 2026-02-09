use crate::{
    constants::*,
    syntax_trees::{ast, shared::*},
};

impl ast::Expr {
    pub fn type_check(&self, env: &mut TypeEnv) -> ValueType {
        use ast::Expr::*;

        match self {
            BinaryOp(left, op, right) => {
                let l_type = left.type_check(env);
                let r_type = right.type_check(env);

                let result_type = op.type_of(&l_type, &r_type).unwrap();
                result_type
            }
            UnaryOp(op, exp) => {
                let exp_type = exp.type_check(env);
                match op {
                    UnaryOperator::Plus | UnaryOperator::Minus => {
                        assert_eq!(exp_type, ValueType::IntType);
                        ValueType::IntType
                    }
                    UnaryOperator::Not => {
                        assert_eq!(exp_type, ValueType::BoolType);
                        ValueType::BoolType
                    }
                }
            }
            Id(id) => env
                .get(id)
                .expect(format!("Unknown Identifier: {id:?}").as_str())
                .clone(),
            Constant(v) => ValueType::from(v),
            Call(id, args) => match id {
                Identifier::Named(name) => {
                    if name.as_ref() == FN_READ_INT {
                        assert!(
                            args.is_empty(),
                            "Passed {} of args to {name}, expected 0",
                            args.len()
                        );
                        ValueType::IntType
                    } else if name.as_ref() == FN_PRINT_INT {
                        assert_eq!(
                            args.len(),
                            1,
                            "Passed {} args to {name}, expected 1",
                            args.len()
                        );
                        assert_eq!(
                            args[0].type_check(env),
                            ValueType::IntType,
                            "Passed wrong arg type to {name}, expected I64"
                        );
                        ValueType::NoneType
                    } else if name.as_ref() == FN_LEN {
                        assert_eq!(
                            args.len(),
                            1,
                            "Passed {} args to {name}, expected 1",
                            args.len()
                        );
                        assert!(
                            matches!(args[0].type_check(env), ValueType::TupleType(_)),
                            "Passed wrong arg type to {name}, expected tuple"
                        );
                        ValueType::IntType
                    } else if name.as_ref() == GC_COLLECT {
                        assert_eq!(
                            args.len(),
                            1,
                            "Passed {} args to {name}, expected 1",
                            args.len()
                        );
                        assert!(
                            matches!(args[0].type_check(env), ValueType::IntType),
                            "Passed wrong arg type to {name}, expected int"
                        );

                        ValueType::NoneType
                    } else {
                        unimplemented!("Unknown function name: {name}")
                    }
                }
                _ => unimplemented!("Unknown function id: {id:?}"),
            },
            Ternary(cond, pos, neg) => {
                let cond_type = cond.type_check(env);
                assert!([ValueType::BoolType, ValueType::IntType].contains(&cond_type));

                let pos_type = pos.type_check(env);
                let neg_type = neg.type_check(env);
                assert_eq!(
                    pos_type, neg_type,
                    "Both branches of a ternary must be the same type"
                );

                pos_type
            }
            StatementBlock(statements, expr) => {
                for s in statements {
                    s.type_check(env);
                }

                expr.type_check(env)
            }
            Tuple(elements) => {
                let element_types: Vec<_> = elements.iter().map(|e| e.type_check(env)).collect();

                ValueType::TupleType(element_types)
            }
            Subscript(tup, idx) => {
                if let ValueType::TupleType(elems) = tup.type_check(env) {
                    assert!(
                        *idx >= 0 && *idx < elems.len() as i64,
                        "Indexed tuple out of bounds"
                    );
                    elems[*idx as usize].clone()
                } else {
                    panic!("Subscripted a non-tuple")
                }
            }
            Allocate(_, value_type) => ValueType::PointerType(Box::new(value_type.clone())),
            GlobalSymbol(_) => ValueType::IntType,
        }
    }
}

impl ast::Statement {
    pub fn type_check(&self, env: &mut TypeEnv) {
        use ast::Statement::*;

        match self {
            Assign(dest, e) => {
                let t = e.type_check(env);
                match dest {
                    AssignDest::Id(id) => {
                        if env.contains_key(id) {
                            assert_eq!(env[id], t);
                        } else {
                            env.insert(id.clone(), t);
                        }
                    },
                    AssignDest::Subscript(tup_id, idx) => {
                        if let Some(ValueType::TupleType(elems)) = env.get(tup_id) {
                            assert_eq!(elems[*idx as usize], t);
                        } else {
                            panic!("Couldn't find tuple to assign into: `{tup_id:?}`")
                        }
                    },
                }
            }
            Expr(e) => {
                e.type_check(env);
            }
            Conditional(cond, pos, neg) => {
                let cond_type = cond.type_check(env);
                assert!([ValueType::BoolType, ValueType::IntType].contains(&cond_type));

                for s in pos.iter().chain(neg) {
                    s.type_check(env);
                }
            }
            WhileLoop(cond, body) => {
                let cond_type = cond.type_check(env);
                assert!([ValueType::BoolType, ValueType::IntType].contains(&cond_type));

                for s in body {
                    s.type_check(env);
                }
            }
            Return(expr) => {
                expr.type_check(env);
            },
        }
    }
}

impl ast::Program {
    pub fn type_check(&mut self) {
        for f in self.functions.iter_mut() {
            for s in &f.body {
                s.type_check(&mut f.types);
            }
        }
    }
}