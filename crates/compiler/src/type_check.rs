use crate::{
    constants::*,
    syntax_trees::{ast, shared::*},
    utils::global,
};

impl ast::Expr {
    pub fn type_check(
        &mut self,
        env: &mut TypeEnv,
        expected_type: &Option<ValueType>,
    ) -> ValueType {
        use ast::Expr::*;

        let result_type = match self {
            BinaryOp(left, op, right) => {
                let l_type = left.type_check(env, &None);
                let r_type = right.type_check(env, &None);

                let result_type = op.type_of(&l_type, &r_type).expect(
                    format!("Invalid argument types to op {op:?} - {l_type:?}, {r_type:?}")
                        .as_str(),
                );
                result_type
            }
            UnaryOp(op, exp) => {
                let exp_type = exp.type_check(env, &None);
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
            Id(id) | GlobalSymbol(id) => env
                .get(id)
                .expect(format!("Unknown Identifier: {id:?}").as_str())
                .clone(),
            Constant(v) => ValueType::from(&*v),
            Call(func, args) => {
                let func_type = func.type_check(env, &None);
                if let ValueType::FunctionType(arg_types, ret_type) = func_type {
                    assert_eq!(
                        args.len(),
                        arg_types.len(),
                        "Wrong number of args passed to `{func:?}`"
                    );

                    if **func == Id(global!(FN_LEN)) {
                        // This one is actually special, it can be
                        // called with any tuple type and there's no way
                        // to express that in ValueType right now so
                        // this is what we're doing

                        // Worth noting that this sucks because you
                        // can't use len indirectly now :(
                        assert!(matches!(
                            args[0].type_check(env, &None),
                            ValueType::TupleType(_)
                        ));
                    } else {
                        for (a, typ) in args.iter_mut().zip(arg_types) {
                            assert_eq!(
                                a.type_check(env, &Some(typ.clone())),
                                typ,
                                "Passed wrong arg type `{a:?}` to function `{func:?}`"
                            )
                        }
                    }

                    *ret_type
                } else {
                    panic!("Tried to call non-function: {func:?}");
                }
            }
            Ternary(cond, pos, neg) => {
                let cond_type = cond.type_check(env, &None);
                assert!([ValueType::BoolType, ValueType::IntType].contains(&cond_type));

                let pos_type = pos.type_check(env, expected_type);
                let neg_type = neg.type_check(env, expected_type);
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

                expr.type_check(env, &None)
            }
            Tuple(elements) => {
                let element_types: Vec<_> = elements
                    .iter_mut()
                    .map(|e| e.type_check(env, &None))
                    .collect();

                ValueType::TupleType(element_types)
            }
            Subscript(tup, idx) => {
                if let ValueType::TupleType(elems) = tup.type_check(env, &None) {
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
            Lambda(func) => {
                for (k, v) in env {
                    func.types.insert(k.clone(), v.clone());
                }

                if let Some(lambda_type) = expected_type {
                    lambda_type.clone()
                } else {
                    panic!("No expected type for lambda");
                }
            }
            Closure(id, _, _) => {
                let func_type = env.get(id).expect(format!("Unknown closure id: {id:?}").as_str());

                if let ValueType::FunctionType(param_types, _) = func_type {
                    if let Some(ValueType::TupleType(capture_types)) = param_types.get(0) {
                        let capture_types = capture_types.iter().cloned();
                        ValueType::TupleType([func_type.clone()].into_iter().chain(capture_types).collect())
                    } else {
                        panic!("Closure function didn't have captures as first arg")
                    }
                } else {
                    panic!("func_type is non-function type??");
                }
            } 
        };

        // Exclude closures from type hint checks because they have been
        // modified to a different type without changing the type
        // hint... its fine
        if let Some(expected) = expected_type
            && !matches!(self, Closure(..))
        {
            assert_eq!(expected, &result_type, "Did not match expected type");
        }

        result_type
    }
}

impl ast::Statement {
    pub fn type_check(&mut self, env: &mut TypeEnv) {
        use ast::Statement::*;

        match self {
            Assign(dest, e, opt_type_hint) => {
                let mut t = e.type_check(env, opt_type_hint);

                if t == ValueType::Indeterminate {
                    if let Some(type_hint) = opt_type_hint {
                        t = type_hint.clone();
                    } else {
                        panic!("Indeterminate typed assign-expression with no type hint");
                    }
                } else if let Some(type_hint) = opt_type_hint && !matches!(e, ast::Expr::Closure(..)) {
                    // Exclude closures from type-hint checks because
                    // their types change during the closurize_lambdas
                    // pass but the type hint type doesnt change
                    assert_eq!(t, *type_hint, "Type hint didn't match assignment type");
                }

                match dest {
                    AssignDest::Id(id) => {
                        if env.contains_key(id) {
                            assert_eq!(env[id], t);
                        } else {
                            env.insert(id.clone(), t);
                        }
                    }
                    AssignDest::Subscript(tup_id, idx) => {
                        if let Some(ValueType::TupleType(elems)) = env.get(tup_id) {
                            assert_eq!(elems[*idx as usize], t);
                        } else {
                            panic!("Couldn't find tuple to assign into: `{tup_id:?}`")
                        }
                    }
                }
            }
            Expr(e) => {
                e.type_check(env, &None);
            }
            Conditional(cond, pos, neg) => {
                let cond_type = cond.type_check(env, &None);
                assert!([ValueType::BoolType, ValueType::IntType].contains(&cond_type));

                for s in pos.iter_mut().chain(neg) {
                    s.type_check(env);
                }
            }
            WhileLoop(cond, body) => {
                let cond_type = cond.type_check(env, &None);
                assert!([ValueType::BoolType, ValueType::IntType].contains(&cond_type));

                for s in body {
                    s.type_check(env);
                }
            }
            Return(expr) => {
                expr.type_check(env, &None);
            }
        }
    }
}

impl ast::Function {
    pub fn type_check(&mut self) {
        for s in self.body.iter_mut() {
            // Allow type-checker to recognize params
            for (n, t) in self.params.iter() {
                self.types.insert(n.clone(), t.clone());
            }

            s.type_check(&mut self.types);
        }
    }

    pub fn set_param_types(&mut self, param_types: Vec<ValueType>) {
        for ((_, type_field), real_type) in self.params.iter_mut().zip(param_types) {
            *type_field = real_type;
        }
    }
}

impl ast::Program {
    pub fn type_check(&mut self) {
        self.populate_globals();

        for f in self.functions.iter_mut() {
            // Start with dict of other global-scope functions so they
            // can be recognized
            f.types = self.function_types.clone();

            f.type_check();
        }
    }

    pub fn populate_globals(&mut self) {
        let special_functions = [
            (
                global!(FN_READ_INT),
                ValueType::FunctionType(vec![], Box::new(ValueType::IntType)),
            ),
            (
                global!(FN_PRINT_INT),
                ValueType::FunctionType(vec![ValueType::IntType], Box::new(ValueType::NoneType)),
            ),
            (
                global!(FN_LEN),
                ValueType::FunctionType(
                    vec![ValueType::TupleType(vec![])],
                    Box::new(ValueType::IntType),
                ),
            ),
            (
                global!(GC_COLLECT),
                ValueType::FunctionType(vec![ValueType::IntType], Box::new(ValueType::NoneType)),
            ),
        ];

        self.function_types = {
            let mut map = TypeEnv::new();
            for f in &self.functions {
                map.insert(
                    f.name.clone(),
                    ValueType::FunctionType(
                        f.params.iter().map(|(_, t)| t.clone()).collect(),
                        Box::new(f.return_type.clone()),
                    ),
                );
            }

            map.extend(special_functions);
            map
        };
    }
}
