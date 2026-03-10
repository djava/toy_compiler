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
                let callee_type = func.type_check(env, &None);
                if let ValueType::FunctionType(arg_types, ret_type) = callee_type {
                    assert_eq!(
                        args.len(),
                        arg_types.len(),
                        "Wrong number of args passed to `{func:?}`"
                    );

                    if let Some(ret_type) = check_special_functions(func, args, env) {
                        ret_type
                    } else {
                        for (a, typ) in args.iter_mut().zip(arg_types) {
                            assert_eq!(
                                strip_pointer(a.type_check(env, &Some(typ.clone()))),
                                typ,
                                "Passed wrong arg type `{a:?}` to function `{func:?}`"
                            )
                        }
                        *ret_type
                    }
                } else if let ValueType::TupleType(closure_elems) = callee_type {
                    if let Some(ValueType::FunctionType(arg_types, ret_type)) = closure_elems.get(0)
                    {
                        assert_eq!(
                            args.len() + 1,
                            arg_types.len(),
                            "Wrong number of args passed to `{func:?}`"
                        );
                        for (arg_expr, expected) in args.iter_mut().zip(arg_types.iter().skip(1)) {
                            let actual = strip_pointer(arg_expr.type_check(env, &Some(expected.clone())));
                            assert_eq!(
                                actual, *expected,
                                "Passed wrong arg type `{actual:?}` to function `{func:?}`"
                            )
                        }

                        (**ret_type).clone()
                    } else {
                        panic!("Tried to call non-closure-tuple: {func:?}")
                    }
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
                if let Some(ValueType::TupleType(exp_elem_types)) = expected_type {
                    let mut element_types = vec![];
                    for (elem, hint) in elements.iter_mut().zip(exp_elem_types) {
                        element_types.push(elem.type_check(env, &Some(hint.clone())));
                    }

                    ValueType::TupleType(element_types)
                } else {
                    let element_types: Vec<_> = elements
                        .iter_mut()
                        .map(|e| e.type_check(env, &None))
                        .collect();

                    ValueType::TupleType(element_types)
                }
            }
            Array(elements) => {
                let expected_elem_type = expected_type.clone().map_or(None, |t| {
                    if let ValueType::ArrayType(e, _) = t {
                        Some(*e)
                    } else {
                        None
                    }
                });

                if !elements.is_empty() {
                    let first_type = elements[0].type_check(env, &expected_elem_type);
                    for e in elements.iter_mut().skip(1) {
                        assert_eq!(e.type_check(env, &expected_elem_type), first_type);
                    }

                    ValueType::ArrayType(Box::new(first_type), elements.len())
                } else {
                    expected_elem_type
                        .map(|e| ValueType::ArrayType(Box::new(e), 0))
                        .unwrap_or(ValueType::ArrayType(Box::new(ValueType::Indeterminate), 0))
                }
            }
            Subscript(exp, idx) => {
                let exp_type = strip_pointer(exp.type_check(env, &None));
                if let ValueType::TupleType(elems) = exp_type {
                    let const_idx = {
                        if let ast::Expr::Constant(Value::I64(val)) = &**idx {
                            val
                        } else {
                            panic!("Indexing tuple with non-const-i64");
                        }
                    };
                    assert!(
                        *const_idx >= 0 && *const_idx < elems.len() as i64,
                        "Indexed tuple out of bounds"
                    );
                    elems[*const_idx as usize].clone()
                } else if let ValueType::ArrayType(elems, _) = exp_type {
                    assert_eq!(
                        idx.type_check(env, &Some(ValueType::IntType)),
                        ValueType::IntType
                    );
                    *elems
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
                    if let ValueType::FunctionType(args, ret_type) = lambda_type {
                        for (param, param_type) in func.params.iter_mut().zip(args) {
                            *param.1 = param_type.clone();
                            func.types.insert(param.0.clone(), param.1.clone());
                        }
                        func.return_type = *ret_type.clone();
                    }
                    lambda_type.clone()
                } else {
                    panic!("No expected type for lambda");
                }
            }
            Closure(id, _) => {
                let func_type = env
                    .get(id)
                    .expect(format!("Unknown closure id: {id:?}").as_str());

                if let ValueType::FunctionType(param_types, _) = func_type {
                    if let Some(ValueType::TupleType(capture_types)) = param_types.get(0) {
                        let capture_types = capture_types.iter().cloned();
                        ValueType::TupleType(
                            [func_type.clone()]
                                .into_iter()
                                .chain(capture_types)
                                .collect(),
                        )
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
            && result_type != *expected
        {
            println!(
                "WARNING: {result_type:?} did not match expected type: {expected:?} in expr {self:?}"
            );
        }

        result_type
    }

    pub fn get_type(&self, env: &TypeEnv) -> ValueType {
        match self {
            ast::Expr::Constant(value) => ValueType::from(value),
            ast::Expr::BinaryOp(l, op, r) => {
                let l_type = l.get_type(env);
                let r_type = r.get_type(env);
                op.type_of(&l_type, &r_type)
                    .expect("Failed type check in get_type - invalid op")
            }
            ast::Expr::UnaryOp(op, e) => {
                let e_type = e.get_type(env);
                op.type_of(&e_type)
                    .expect("Failed type check in get_type - invalid op")
            }
            ast::Expr::Call(func, args) => {
                let func_type = strip_pointer(func.get_type(env));
                if **func == ast::Expr::GlobalSymbol(global!(FN_SUBSCRIPT_ARRAY)) {
                    let arr_type = strip_pointer(args[0].get_type(env));
                    if let ValueType::ArrayType(elems, _) = arr_type {
                        *elems.clone()
                    } else {
                        panic!(
                            "Failed type check in get_type - Non-Array passed to __subscript_array: {:?} ({:?})",
                            args[0],
                            args[0].get_type(env)
                        )
                    }
                } else if let ValueType::FunctionType(_, ret_type) = &func_type {
                    *ret_type.clone()
                } else if let ValueType::TupleType(elems) = &func_type &&
                          let ValueType::FunctionType(_, ret_type) = &elems[0] {
                    *ret_type.clone()
                } else {
                    panic!("Failed type check in get_type - invalid callee: {func:?} ({func_type:?})")
                }
            }
            ast::Expr::Id(identifier) => env
                .get(identifier)
                .unwrap_or_else(|| panic!("missing type for id {identifier:?}"))
                .clone(),
            ast::Expr::Ternary(_, pos, _) => pos.get_type(env),
            ast::Expr::StatementBlock(_, expr) => expr.get_type(env),
            ast::Expr::Tuple(exprs) => {
                ValueType::TupleType(exprs.iter().map(|e| e.get_type(env)).collect())
            }
            ast::Expr::Array(exprs) => ValueType::ArrayType(
                Box::new(
                    exprs
                        .get(0)
                        .map_or(ValueType::Indeterminate, |e| e.get_type(env)),
                ),
                exprs.len(),
            ),
            ast::Expr::Subscript(expr, idx) => {
                match strip_pointer(expr.get_type(env)) {
                    ValueType::TupleType(mut elems) => {
                        if let ast::Expr::Constant(Value::I64(idx_val)) = **idx {
                            elems.remove(idx_val as usize)
                        } else {
                            panic!("Failed get_type - Indexed with non-constant")
                        }
                    }
                    ValueType::ArrayType(elem, _) => *elem,
                    _ => panic!("Failed get_type - Indexed invalid LHS"),
                }
            }
            ast::Expr::Allocate(_, value_type) => {
                ValueType::PointerType(Box::new(value_type.clone()))
            }
            ast::Expr::GlobalSymbol(identifier) => env[identifier].clone(),
            ast::Expr::Closure(identifier, _) => env[identifier].clone(),
            ast::Expr::Lambda(_) => panic!("Failed get_type - Lambda in get_type?"),
        }
    }
}

fn check_special_functions(
    func: &ast::Expr,
    args: &mut Vec<ast::Expr>,
    env: &mut TypeEnv,
) -> Option<ValueType> {
    use ast::Expr::*;

    if *func == GlobalSymbol(global!(FN_LEN)) {
        // Can be called with any tuple or array type
        assert_eq!(args.len(), 1);
        assert!(matches!(
            args[0].type_check(env, &None),
            ValueType::TupleType(_) | ValueType::ArrayType(_, _)
        ));
        Some(ValueType::IntType)
    } else if *func == GlobalSymbol(global!(FN_SUBSCRIPT_ARRAY)) {
        // Can be called with any array type and an index
        let arr_type = strip_pointer(args[0].type_check(env, &None));
        assert_eq!(args.len(), 2);
        assert!(matches!(arr_type, ValueType::ArrayType(_, _)));
        assert!(matches!(args[1].type_check(env, &None), ValueType::IntType));
        if let ValueType::ArrayType(elem_type, _) = arr_type {
            Some(*elem_type)
        } else {
            unreachable!()
        }
    } else if *func == GlobalSymbol(global!(FN_ASSIGN_TO_ARRAY_ELEM)) {
        // Can be called with any array type, an index and a value
        let arr_type = strip_pointer(args[0].type_check(env, &None));
        assert_eq!(args.len(), 3);
        assert!(matches!(arr_type, ValueType::ArrayType(_, _)));
        assert!(matches!(args[1].type_check(env, &None), ValueType::IntType));

        let elem_type = if let ValueType::ArrayType(elem_type, _) = arr_type {
            *elem_type
        } else {
            unreachable!();
        };
        assert_eq!(args[2].type_check(env, &None), elem_type);

        Some(ValueType::NoneType)
    } else {
        None
    }
}

fn strip_pointer_ref(t: &ValueType) -> &ValueType {
    match t {
        ValueType::PointerType(x) => &**x,
        t => t,
    }
}

fn strip_pointer(t: ValueType) -> ValueType {
    match t {
        ValueType::PointerType(x) => *x,
        t => t,
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
                } else if let Some(type_hint) = opt_type_hint
                    && !matches!(e, ast::Expr::Closure(..))
                {
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
                        let inner_type = env.get(tup_id).map(strip_pointer_ref);
                        if let Some(ValueType::TupleType(elems)) = inner_type {
                            assert!(
                                *idx >= 0 && *idx < elems.len() as i64,
                                "Indexed tuple out of bounds"
                            );
                            assert_eq!(elems[*idx as usize], t);
                        } else if let Some(ValueType::ArrayType(elem_type, len)) = inner_type {
                            assert!(
                                *idx >= 0 && *idx < *len as i64,
                                "Indexed array out of bounds"
                            );
                            assert_eq!(**elem_type, t);
                        } else {
                            panic!("Couldn't find tuple to assign into: `{tup_id:?}`")
                        }
                    }
                    AssignDest::ComplexSubscript(complex) => {
                        let idx_type = complex.index.type_check(env, &Some(ValueType::IntType));
                        assert_eq!(idx_type, ValueType::IntType);

                        let cont_type = complex.container.type_check(env, &None);
                        if let ValueType::ArrayType(elem_type, _) = cont_type {
                            assert_eq!(*elem_type, t);
                        } else if let ValueType::TupleType(elem_types) = cont_type
                            && let ast::Expr::Constant(Value::I64(idx_const)) = complex.index
                        {
                            assert_eq!(elem_types[idx_const as usize], t);
                        } else {
                            panic!("Complex subscript of non-array")
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
                global!(FN_GC_COLLECT),
                ValueType::FunctionType(vec![ValueType::IntType], Box::new(ValueType::NoneType)),
            ),
            (
                global!(FN_SUBSCRIPT_ARRAY),
                ValueType::FunctionType(
                    vec![
                        ValueType::ArrayType(Box::new(ValueType::Indeterminate), 0),
                        ValueType::IntType,
                    ],
                    Box::new(ValueType::Indeterminate),
                ),
            ),
            (
                global!(FN_ASSIGN_TO_ARRAY_ELEM),
                ValueType::FunctionType(
                    vec![
                        ValueType::ArrayType(Box::new(ValueType::Indeterminate), 0),
                        ValueType::IntType,
                        ValueType::Indeterminate,
                    ],
                    Box::new(ValueType::NoneType),
                ),
            ),
        ];

        self.global_types = {
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
            map.extend(
                EXTERNED_VARIABLES
                    .iter()
                    .map(|v| (v.clone(), ValueType::IntType)),
            );
            map
        };

        for f in self.functions.iter_mut() {
            // Start with dict of other global-scope functions so they
            // can be recognized
            f.types = self.global_types.clone();
        }
    }
}
