use crate::{
    passes::ASTPass,
    syntax_trees::{ast::*, shared::*},
};

#[derive(Debug)]
pub struct ClosurizeLambdas;

impl ASTPass for ClosurizeLambdas {
    fn run_pass(self, mut m: Program) -> Program {
        let mut extracted_functions = vec![];

        for f in m.functions.iter_mut() {
            for s in f.body.iter_mut() {
                extract_lambdas_from_statement(s, &mut extracted_functions);
            }
        }

        m.functions.extend(extracted_functions);

        m
    }
}

fn extract_lambdas_from_statement(s: &mut Statement, extracted: &mut Vec<Function>) {
    match s {
        Statement::Assign(_, expr, _) => extract_lambdas_from_expr(expr, extracted),
        Statement::Expr(expr) => extract_lambdas_from_expr(expr, extracted),
        Statement::Conditional(cond, pos_body, neg_body) => {
            extract_lambdas_from_expr(cond, extracted);
            for s in pos_body {
                extract_lambdas_from_statement(s, extracted);
            }
            for s in neg_body {
                extract_lambdas_from_statement(s, extracted);
            }
        }
        Statement::WhileLoop(cond, body) => {
            extract_lambdas_from_expr(cond, extracted);
            for s in body {
                extract_lambdas_from_statement(s, extracted);
            }
        }
        Statement::Return(expr) => {
            extract_lambdas_from_expr(expr, extracted);
        }
    }
}

fn extract_lambdas_from_expr(e: &mut Expr, extracted: &mut Vec<Function>) {
    match e {
        Expr::Lambda(_) => {
            extracted.push(convert_lambda_to_closure(e));
        }
        Expr::BinaryOp(l, _, r) => {
            extract_lambdas_from_expr(l, extracted);
            extract_lambdas_from_expr(r, extracted);
        }
        Expr::UnaryOp(_, expr) => extract_lambdas_from_expr(expr, extracted),
        Expr::Call(func, args) => {
            extract_lambdas_from_expr(func, extracted);
            for a in args {
                extract_lambdas_from_expr(a, extracted);
            }
        }
        Expr::Ternary(cond, pos, neg) => {
            extract_lambdas_from_expr(cond, extracted);
            extract_lambdas_from_expr(pos, extracted);
            extract_lambdas_from_expr(neg, extracted);
        }
        Expr::StatementBlock(statements, expr) => {
            for s in statements {
                extract_lambdas_from_statement(s, extracted);
            }
            extract_lambdas_from_expr(expr, extracted);
        }
        Expr::Tuple(exprs) => {
            for e in exprs {
                extract_lambdas_from_expr(e, extracted);
            }
        }
        Expr::Subscript(expr, _) => {
            extract_lambdas_from_expr(expr, extracted);
        }
        Expr::Constant(_)
        | Expr::Allocate(_, _)
        | Expr::GlobalSymbol(_)
        | Expr::Id(_)
        | Expr::Closure(..) => {}
    }
}

fn convert_lambda_to_closure(e: &mut Expr) -> Function {
    if let Expr::Lambda(func) = e {
        // Collect which IDs are used as captures in this lambda
        let captured_ids = find_and_disambiguate_captures(func);

        let this_func_type = ValueType::FunctionType(
            std::iter::once(ValueType::TupleType(vec![])) // can't recurse here, good enough
                .chain(func.params.iter().map(|(_, typ)| typ.clone()))
                .collect(),
            Box::new(func.return_type.clone()),
        );
        // Add the capture-tuple param
        let captures_id = Identifier::new_ephemeral();
        func.params.insert_before(
            0,
            captures_id.clone(),
            ValueType::TupleType(
                std::iter::once(this_func_type)
                    .chain(captured_ids.iter().map(|id| func.types[id].clone()))
                    .collect(),
            ),
        );

        replace_captures_with_tup_reference(func, &captures_id, &captured_ids);

        let func_name = func.name.clone();
        // Replace the Expr::Lambda with an Expr::Closure
        let lambda_expr = std::mem::replace(e, Expr::Closure(func_name, captured_ids));

        if let Expr::Lambda(func) = lambda_expr {
            func
        } else {
            unreachable!();
        }
    } else {
        panic!("Passed non-lambda to handle_lambda()")
    }
}

fn find_and_disambiguate_captures(func: &mut Function) -> Vec<Identifier> {
    let mut captures = vec![];
    for s in func.body.iter_mut() {
        find_and_disambiguate_captures_for_statement(s, &func.types, &mut captures);
    }

    captures
}

fn find_and_disambiguate_captures_for_statement(
    s: &mut Statement,
    this_env: &TypeEnv,
    captures: &mut Vec<Identifier>,
) {
    match s {
        Statement::Assign(assign_dest, expr, _) => {
            match assign_dest {
                AssignDest::Id(id) | AssignDest::Subscript(id, _) => {
                    if let Some(new_id) = is_id_in_parent(id, this_env) {
                        *id = new_id.clone();
                        captures.push(new_id);
                    }
                }
            }
            find_and_disambiguate_captures_for_expr(expr, this_env, captures);
        }
        Statement::Expr(expr) => find_and_disambiguate_captures_for_expr(expr, this_env, captures),
        Statement::Conditional(cond, pos_body, neg_body) => {
            find_and_disambiguate_captures_for_expr(cond, this_env, captures);
            for s in pos_body {
                find_and_disambiguate_captures_for_statement(s, this_env, captures);
            }
            for s in neg_body {
                find_and_disambiguate_captures_for_statement(s, this_env, captures);
            }
        }
        Statement::WhileLoop(cond, body) => {
            find_and_disambiguate_captures_for_expr(cond, this_env, captures);
            for s in body {
                find_and_disambiguate_captures_for_statement(s, this_env, captures);
            }
        }
        Statement::Return(expr) => {
            find_and_disambiguate_captures_for_expr(expr, this_env, captures);
        }
    }
}

fn find_and_disambiguate_captures_for_expr(
    e: &mut Expr,
    this_env: &TypeEnv,
    captures: &mut Vec<Identifier>,
) {
    match e {
        Expr::Id(id) => {
            if let Some(new_id) = is_id_in_parent(id, this_env) {
                *id = new_id.clone();
                captures.push(new_id);
            }
        }
        Expr::BinaryOp(l, _, r) => {
            find_and_disambiguate_captures_for_expr(l, this_env, captures);
            find_and_disambiguate_captures_for_expr(r, this_env, captures);
        }
        Expr::UnaryOp(_, expr) => find_and_disambiguate_captures_for_expr(expr, this_env, captures),
        Expr::Call(func, args) => {
            find_and_disambiguate_captures_for_expr(func, this_env, captures);
            for a in args {
                find_and_disambiguate_captures_for_expr(a, this_env, captures);
            }
        }
        Expr::Ternary(cond, pos, neg) => {
            find_and_disambiguate_captures_for_expr(cond, this_env, captures);
            find_and_disambiguate_captures_for_expr(pos, this_env, captures);
            find_and_disambiguate_captures_for_expr(neg, this_env, captures);
        }
        Expr::StatementBlock(statements, expr) => {
            for s in statements {
                find_and_disambiguate_captures_for_statement(s, this_env, captures);
            }
            find_and_disambiguate_captures_for_expr(expr, this_env, captures);
        }
        Expr::Tuple(exprs) => {
            for e in exprs {
                find_and_disambiguate_captures_for_expr(e, this_env, captures);
            }
        }
        Expr::Subscript(expr, _) => {
            find_and_disambiguate_captures_for_expr(expr, this_env, captures);
        }
        Expr::Constant(_) => {}
        Expr::Allocate(_, _) => {}
        Expr::GlobalSymbol(_) => {}
        Expr::Lambda(_) => {}
        Expr::Closure(..) => panic!("Closures should not exist yet"),
    }
}

fn is_id_in_parent(id: &Identifier, this_env: &TypeEnv) -> Option<Identifier> {
    if let Identifier::Local(name, _) = id {
        if this_env.contains_key(id) {
            // it's in this env (including globals) - no adjustment needed
            None
        } else {
            // Hopefully belongs to a parent and we just have to swap out
            // its scope

            // TODO: THIS IS A BAD SOLUTION. THIS IS NON-DETERMINISTIC
            //       IN THE FOLLOWING CASE:
            //  fn foo() {
            //    a = 1
            //    x = lambda a: {
            //      print_int(a) # Prints the lambda's argument `a`
            //      y = lambda: print_int(a)
            //      y() # Non-deterministic which `a` it prints :(
            //    }
            //  }

            for env_id in this_env.keys() {
                if let Identifier::Local(env_name, _) = env_id
                    && env_name == name
                {
                    return Some(env_id.clone());
                }
            }
            // Leave it alone, it's a local of the lambda probably
            None
        }
    } else {
        // Global or ephemeral - no adjustment needed
        None
    }
}

fn replace_captures_with_tup_reference(
    f: &mut Function,
    captures_id: &Identifier,
    captures: &Vec<Identifier>,
) {
    for s in f.body.iter_mut() {
        replace_captures_with_tup_reference_for_statement(s, captures_id, captures);
    }
}

fn replace_captures_with_tup_reference_for_statement(
    s: &mut Statement,
    captures_id: &Identifier,
    captures: &Vec<Identifier>,
) {
    match s {
        Statement::Assign(assign_dest, expr, _) => {
            match assign_dest {
                AssignDest::Id(id) => {
                    if let Some(capture_idx) = captures.iter().position(|i| i == id) {
                        *assign_dest =
                            AssignDest::Subscript(captures_id.clone(), (capture_idx + 1) as _);
                    }
                }
                AssignDest::Subscript(_, _) => {
                    unimplemented!("Turns out indexing into captured variables is annoying, sorry");
                }
            }
            replace_captures_with_tup_reference_for_expr(expr, captures_id, captures);
        }
        Statement::Expr(expr) => {
            replace_captures_with_tup_reference_for_expr(expr, captures_id, captures)
        }
        Statement::Conditional(cond, pos_body, neg_body) => {
            replace_captures_with_tup_reference_for_expr(cond, captures_id, captures);
            for s in pos_body {
                replace_captures_with_tup_reference_for_statement(s, captures_id, captures);
            }
            for s in neg_body {
                replace_captures_with_tup_reference_for_statement(s, captures_id, captures);
            }
        }
        Statement::WhileLoop(cond, body) => {
            replace_captures_with_tup_reference_for_expr(cond, captures_id, captures);
            for s in body {
                replace_captures_with_tup_reference_for_statement(s, captures_id, captures);
            }
        }
        Statement::Return(expr) => {
            replace_captures_with_tup_reference_for_expr(expr, captures_id, captures);
        }
    }
}

fn replace_captures_with_tup_reference_for_expr(
    e: &mut Expr,
    captures_id: &Identifier,
    captures: &Vec<Identifier>,
) {
    match e {
        Expr::Id(id) => {
            if let Some(capture_idx) = captures.iter().position(|i| i == id) {
                *e = Expr::Subscript(
                    Box::new(Expr::Id(captures_id.clone())),
                    (capture_idx + 1) as _,
                );
            }
        }
        Expr::BinaryOp(l, _, r) => {
            replace_captures_with_tup_reference_for_expr(l, captures_id, captures);
            replace_captures_with_tup_reference_for_expr(r, captures_id, captures);
        }
        Expr::UnaryOp(_, expr) => {
            replace_captures_with_tup_reference_for_expr(expr, captures_id, captures)
        }
        Expr::Call(func, args) => {
            replace_captures_with_tup_reference_for_expr(func, captures_id, captures);
            for a in args {
                replace_captures_with_tup_reference_for_expr(a, captures_id, captures);
            }
        }
        Expr::Ternary(cond, pos, neg) => {
            replace_captures_with_tup_reference_for_expr(cond, captures_id, captures);
            replace_captures_with_tup_reference_for_expr(pos, captures_id, captures);
            replace_captures_with_tup_reference_for_expr(neg, captures_id, captures);
        }
        Expr::StatementBlock(statements, expr) => {
            for s in statements {
                replace_captures_with_tup_reference_for_statement(s, captures_id, captures);
            }
            replace_captures_with_tup_reference_for_expr(expr, captures_id, captures);
        }
        Expr::Tuple(exprs) => {
            for e in exprs {
                replace_captures_with_tup_reference_for_expr(e, captures_id, captures);
            }
        }
        Expr::Subscript(expr, _) => {
            replace_captures_with_tup_reference_for_expr(expr, captures_id, captures);
        }

        Expr::Constant(_) | Expr::Allocate(_, _) | Expr::GlobalSymbol(_) | Expr::Lambda(_) => {}

        Expr::Closure(..) => panic!("Closures should not exist yet"),
    }
}

#[cfg(test)]
mod tests {
    use crate::utils::{t_global, t_local};
    use indexmap::IndexMap;
    use test_support::compiler::{
        constants::LABEL_MAIN,
        passes::{ASTPass, ClosurizeLambdas, TypeCheck},
        syntax_trees::{ast::*, shared::*},
    };

    macro_rules! main_local {
        ($name:expr) => {
            t_local!($name, t_global!(LABEL_MAIN))
        };
    }

    #[test]
    fn test_lambda_to_closure() {
        let mut program = Program {
            functions: vec![Function {
                name: t_global!(LABEL_MAIN),
                body: vec![
                    Statement::Assign(
                        AssignDest::Id(main_local!("foo")),
                        Expr::Constant(Value::I64(1)),
                        None,
                    ),
                    Statement::Assign(
                        AssignDest::Id(main_local!("func")),
                        Expr::Lambda(Function {
                            name: t_global!("__1"),
                            body: vec![Statement::Expr(Expr::Id(t_local!(
                                "foo",
                                t_global!("__1")
                            )))],
                            types: TypeEnv::new(),
                            params: IndexMap::new(),
                            return_type: ValueType::NoneType,
                        }),
                        Some(ValueType::FunctionType(
                            vec![],
                            Box::new(ValueType::NoneType),
                        )),
                    ),
                ],
                types: TypeEnv::new(),
                params: IndexMap::new(),
                return_type: ValueType::IntType,
            }],
            global_types: TypeEnv::new(),
        };

        program = TypeCheck.run_pass(program);
        program = ClosurizeLambdas.run_pass(program);
        program = TypeCheck.run_pass(program);

        let extracted_closure = &program
            .functions
            .iter()
            .find(|f| f.name == t_global!("__1"))
            .unwrap();
        assert_eq!(extracted_closure.params.len(), 1);

        // The captured `foo` Id in the body must be rewritten to captures_id[0]
        let captures_id = extracted_closure.params.get_index(0).unwrap().0.clone();
        assert_eq!(
            extracted_closure.body[0],
            Statement::Expr(Expr::Subscript(Box::new(Expr::Id(captures_id)), 1))
        );

        // global_types reflects the new signature with the captures-tuple param;
        // element 0 is the function's own type (with a placeholder empty captures),
        // followed by the types of the captured variables.
        assert_eq!(
            program.global_types[&t_global!("__1")],
            ValueType::FunctionType(
                vec![ValueType::TupleType(vec![
                    ValueType::FunctionType(
                        vec![ValueType::TupleType(vec![])],
                        Box::new(ValueType::NoneType),
                    ),
                    ValueType::IntType,
                ])],
                Box::new(ValueType::NoneType),
            )
        );
    }

    #[test]
    fn test_no_capture_lambda() {
        // A lambda that doesn't reference any outer variables still
        // receives an empty captures-tuple as its first parameter.
        let mut program = Program {
            functions: vec![Function {
                name: t_global!(LABEL_MAIN),
                body: vec![Statement::Assign(
                    AssignDest::Id(main_local!("func")),
                    Expr::Lambda(Function {
                        name: t_global!("__no_cap"),
                        body: vec![Statement::Return(Expr::Constant(Value::I64(42)))],
                        types: TypeEnv::new(),
                        params: IndexMap::new(),
                        return_type: ValueType::IntType,
                    }),
                    Some(ValueType::FunctionType(
                        vec![],
                        Box::new(ValueType::IntType),
                    )),
                )],
                types: TypeEnv::new(),
                params: IndexMap::new(),
                return_type: ValueType::IntType,
            }],
            global_types: TypeEnv::new(),
        };

        program = TypeCheck.run_pass(program);
        program = ClosurizeLambdas.run_pass(program);
        program = TypeCheck.run_pass(program);

        assert_eq!(program.functions.len(), 2);
        let lam = program
            .functions
            .iter()
            .find(|f| f.name == t_global!("__no_cap"))
            .unwrap();
        assert_eq!(lam.params.len(), 1);
        // Body has nothing to rewrite — the constant is unchanged
        assert_eq!(
            lam.body[0],
            Statement::Return(Expr::Constant(Value::I64(42)))
        );
        // Signature in global_types: captures-tuple has the function's own type
        // at element 0 (placeholder empty captures), no captured vars after.
        assert_eq!(
            program.global_types[&t_global!("__no_cap")],
            ValueType::FunctionType(
                vec![ValueType::TupleType(vec![ValueType::FunctionType(
                    vec![ValueType::TupleType(vec![])],
                    Box::new(ValueType::IntType),
                )])],
                Box::new(ValueType::IntType),
            )
        );
    }

    #[test]
    fn test_multiple_captures() {
        // A lambda capturing two outer variables gets a captures-tuple
        // containing both types, in the order they first appear in the body.
        let mut program = Program {
            functions: vec![Function {
                name: t_global!(LABEL_MAIN),
                body: vec![
                    Statement::Assign(
                        AssignDest::Id(main_local!("foo")),
                        Expr::Constant(Value::I64(1)),
                        None,
                    ),
                    Statement::Assign(
                        AssignDest::Id(main_local!("bar")),
                        Expr::Constant(Value::Bool(true)),
                        None,
                    ),
                    Statement::Assign(
                        AssignDest::Id(main_local!("func")),
                        Expr::Lambda(Function {
                            name: t_global!("__multi_cap"),
                            body: vec![
                                // foo referenced first → index 0 in captures tuple
                                Statement::Expr(Expr::Id(t_local!(
                                    "foo",
                                    t_global!("__multi_cap")
                                ))),
                                // bar referenced second → index 1
                                Statement::Expr(Expr::Id(t_local!(
                                    "bar",
                                    t_global!("__multi_cap")
                                ))),
                            ],
                            types: TypeEnv::new(),
                            params: IndexMap::new(),
                            return_type: ValueType::NoneType,
                        }),
                        Some(ValueType::FunctionType(
                            vec![],
                            Box::new(ValueType::NoneType),
                        )),
                    ),
                ],
                types: TypeEnv::new(),
                params: IndexMap::new(),
                return_type: ValueType::IntType,
            }],
            global_types: TypeEnv::new(),
        };

        program = TypeCheck.run_pass(program);
        program = ClosurizeLambdas.run_pass(program);
        program = TypeCheck.run_pass(program);

        let lam = program
            .functions
            .iter()
            .find(|f| f.name == t_global!("__multi_cap"))
            .unwrap();
        assert_eq!(lam.params.len(), 1);

        // func_type → captures_id[0], foo → captures_id[1], bar → captures_id[2]
        let captures_id = lam.params.get_index(0).unwrap().0.clone();
        assert_eq!(
            lam.body[0],
            Statement::Expr(Expr::Subscript(Box::new(Expr::Id(captures_id.clone())), 1))
        );
        assert_eq!(
            lam.body[1],
            Statement::Expr(Expr::Subscript(Box::new(Expr::Id(captures_id)), 2))
        );

        assert_eq!(
            program.global_types[&t_global!("__multi_cap")],
            ValueType::FunctionType(
                vec![ValueType::TupleType(vec![
                    ValueType::FunctionType(
                        vec![ValueType::TupleType(vec![])],
                        Box::new(ValueType::NoneType),
                    ),
                    ValueType::IntType,
                    ValueType::BoolType,
                ])],
                Box::new(ValueType::NoneType),
            )
        );
    }

    #[test]
    fn test_capture_in_arithmetic_expression() {
        // A captured variable that appears inside a sub-expression
        // (not at the top level of a statement) is still detected and rewritten.
        let mut program = Program {
            functions: vec![Function {
                name: t_global!(LABEL_MAIN),
                body: vec![
                    Statement::Assign(
                        AssignDest::Id(main_local!("x")),
                        Expr::Constant(Value::I64(10)),
                        None,
                    ),
                    Statement::Assign(
                        AssignDest::Id(main_local!("func")),
                        Expr::Lambda(Function {
                            name: t_global!("__arith_cap"),
                            body: vec![Statement::Return(Expr::BinaryOp(
                                Box::new(Expr::Id(t_local!("x", t_global!("__arith_cap")))),
                                BinaryOperator::Add,
                                Box::new(Expr::Constant(Value::I64(1))),
                            ))],
                            types: TypeEnv::new(),
                            params: IndexMap::new(),
                            return_type: ValueType::IntType,
                        }),
                        Some(ValueType::FunctionType(
                            vec![],
                            Box::new(ValueType::IntType),
                        )),
                    ),
                ],
                types: TypeEnv::new(),
                params: IndexMap::new(),
                return_type: ValueType::IntType,
            }],
            global_types: TypeEnv::new(),
        };

        program = TypeCheck.run_pass(program);
        program = ClosurizeLambdas.run_pass(program);
        program = TypeCheck.run_pass(program);

        let lam = program
            .functions
            .iter()
            .find(|f| f.name == t_global!("__arith_cap"))
            .unwrap();
        assert_eq!(lam.params.len(), 1);

        // The captured `x` inside the BinaryOp becomes captures_id[1]
        let captures_id = lam.params.get_index(0).unwrap().0.clone();
        assert_eq!(
            lam.body[0],
            Statement::Return(Expr::BinaryOp(
                Box::new(Expr::Subscript(Box::new(Expr::Id(captures_id)), 1)),
                BinaryOperator::Add,
                Box::new(Expr::Constant(Value::I64(1))),
            ))
        );

        assert_eq!(
            program.global_types[&t_global!("__arith_cap")],
            ValueType::FunctionType(
                vec![ValueType::TupleType(vec![
                    ValueType::FunctionType(
                        vec![ValueType::TupleType(vec![])],
                        Box::new(ValueType::IntType),
                    ),
                    ValueType::IntType,
                ])],
                Box::new(ValueType::IntType),
            )
        );
    }

    #[test]
    fn test_closure_replaces_lambda() {
        // The Expr::Lambda at the call site must be replaced with Expr::Closure
        // carrying the function name and the list of captured parent-scoped ids.
        let mut program = Program {
            functions: vec![Function {
                name: t_global!(LABEL_MAIN),
                body: vec![
                    Statement::Assign(
                        AssignDest::Id(main_local!("val")),
                        Expr::Constant(Value::I64(7)),
                        None,
                    ),
                    Statement::Assign(
                        AssignDest::Id(main_local!("func")),
                        Expr::Lambda(Function {
                            name: t_global!("__closure_repl"),
                            body: vec![Statement::Expr(Expr::Id(t_local!(
                                "val",
                                t_global!("__closure_repl")
                            )))],
                            types: TypeEnv::new(),
                            params: IndexMap::new(),
                            return_type: ValueType::NoneType,
                        }),
                        Some(ValueType::FunctionType(
                            vec![],
                            Box::new(ValueType::NoneType),
                        )),
                    ),
                ],
                types: TypeEnv::new(),
                params: IndexMap::new(),
                return_type: ValueType::IntType,
            }],
            global_types: TypeEnv::new(),
        };

        program = TypeCheck.run_pass(program);
        program = ClosurizeLambdas.run_pass(program);

        assert_eq!(program.functions.len(), 2);

        let main_func = &program.functions[0];
        match &main_func.body[1] {
            Statement::Assign(_, Expr::Closure(name, captured_ids), _) => {
                assert_eq!(*name, t_global!("__closure_repl"));
                assert_eq!(captured_ids.len(), 1);
                assert_eq!(captured_ids.get(0), Some(&main_local!("val")));
            }
            other => panic!("Expected Closure expression, got: {other:?}"),
        }

        // The extracted function's body must use captures_id[0] instead of Id("val")
        let lam = program
            .functions
            .iter()
            .find(|f| f.name == t_global!("__closure_repl"))
            .unwrap();
        let captures_id = lam.params.get_index(0).unwrap().0.clone();
        assert_eq!(
            lam.body[0],
            Statement::Expr(Expr::Subscript(Box::new(Expr::Id(captures_id)), 1))
        );
    }

    #[test]
    fn test_multiple_lambdas() {
        // Two independent lambdas in the same function are each extracted
        // as separate top-level functions.
        let mut program = Program {
            functions: vec![Function {
                name: t_global!(LABEL_MAIN),
                body: vec![
                    Statement::Assign(
                        AssignDest::Id(main_local!("n")),
                        Expr::Constant(Value::I64(5)),
                        None,
                    ),
                    Statement::Assign(
                        AssignDest::Id(main_local!("f1")),
                        Expr::Lambda(Function {
                            name: t_global!("__lam_a"),
                            body: vec![Statement::Expr(Expr::Id(t_local!(
                                "n",
                                t_global!("__lam_a")
                            )))],
                            types: TypeEnv::new(),
                            params: IndexMap::new(),
                            return_type: ValueType::NoneType,
                        }),
                        Some(ValueType::FunctionType(
                            vec![],
                            Box::new(ValueType::NoneType),
                        )),
                    ),
                    Statement::Assign(
                        AssignDest::Id(main_local!("f2")),
                        Expr::Lambda(Function {
                            name: t_global!("__lam_b"),
                            body: vec![Statement::Expr(Expr::Id(t_local!(
                                "n",
                                t_global!("__lam_b")
                            )))],
                            types: TypeEnv::new(),
                            params: IndexMap::new(),
                            return_type: ValueType::NoneType,
                        }),
                        Some(ValueType::FunctionType(
                            vec![],
                            Box::new(ValueType::NoneType),
                        )),
                    ),
                ],
                types: TypeEnv::new(),
                params: IndexMap::new(),
                return_type: ValueType::IntType,
            }],
            global_types: TypeEnv::new(),
        };

        program = TypeCheck.run_pass(program);
        program = ClosurizeLambdas.run_pass(program);
        program = TypeCheck.run_pass(program);

        assert_eq!(program.functions.len(), 3);
        for lambda_name in [t_global!("__lam_a"), t_global!("__lam_b")] {
            let lam = program
                .functions
                .iter()
                .find(|f| f.name == lambda_name)
                .unwrap();
            assert_eq!(lam.params.len(), 1);
            assert_eq!(
                *lam.params.get_index(0).unwrap().1,
                ValueType::TupleType(vec![
                    ValueType::FunctionType(
                        vec![ValueType::TupleType(vec![])],
                        Box::new(ValueType::NoneType),
                    ),
                    ValueType::IntType,
                ]),
            );
            // Each lambda's body should reference n via captures_id[1]
            let captures_id = lam.params.get_index(0).unwrap().0.clone();
            assert_eq!(
                lam.body[0],
                Statement::Expr(Expr::Subscript(Box::new(Expr::Id(captures_id)), 1))
            );
        }

        // Both Closures in main carry n as the sole captured id
        let main_func = program
            .functions
            .iter()
            .find(|f| f.name == t_global!(LABEL_MAIN))
            .unwrap();
        for (body_idx, expected_name) in [(1, t_global!("__lam_a")), (2, t_global!("__lam_b"))] {
            match &main_func.body[body_idx] {
                Statement::Assign(_, Expr::Closure(name, captured_ids), _) => {
                    assert_eq!(*name, expected_name);
                    assert_eq!(captured_ids.len(), 1);
                    assert_eq!(captured_ids.get(0), Some(&main_local!("n")));
                }
                other => panic!("Expected Closure, got: {other:?}"),
            }
        }
    }

    #[test]
    fn test_lambda_in_conditional() {
        // A lambda nested inside a conditional branch is still extracted.
        let mut program = Program {
            functions: vec![Function {
                name: t_global!(LABEL_MAIN),
                body: vec![
                    Statement::Assign(
                        AssignDest::Id(main_local!("x")),
                        Expr::Constant(Value::I64(3)),
                        None,
                    ),
                    Statement::Conditional(
                        Expr::Constant(Value::Bool(true)),
                        vec![Statement::Assign(
                            AssignDest::Id(main_local!("f")),
                            Expr::Lambda(Function {
                                name: t_global!("__cond_lam"),
                                body: vec![Statement::Expr(Expr::Id(t_local!(
                                    "x",
                                    t_global!("__cond_lam")
                                )))],
                                types: TypeEnv::new(),
                                params: IndexMap::new(),
                                return_type: ValueType::NoneType,
                            }),
                            Some(ValueType::FunctionType(
                                vec![],
                                Box::new(ValueType::NoneType),
                            )),
                        )],
                        vec![],
                    ),
                ],
                types: TypeEnv::new(),
                params: IndexMap::new(),
                return_type: ValueType::IntType,
            }],
            global_types: TypeEnv::new(),
        };

        program = TypeCheck.run_pass(program);
        program = ClosurizeLambdas.run_pass(program);

        assert_eq!(program.functions.len(), 2);
        let lam = program
            .functions
            .iter()
            .find(|f| f.name == t_global!("__cond_lam"))
            .unwrap();
        assert_eq!(lam.params.len(), 1);
        assert_eq!(
            *lam.params.get_index(0).unwrap().1,
            ValueType::TupleType(vec![
                ValueType::FunctionType(
                    vec![ValueType::TupleType(vec![])],
                    Box::new(ValueType::NoneType),
                ),
                ValueType::IntType,
            ]),
        );
    }

    #[test]
    fn test_lambda_in_while_loop() {
        // A lambda nested inside a while loop body is still extracted.
        let mut program = Program {
            functions: vec![Function {
                name: t_global!(LABEL_MAIN),
                body: vec![
                    Statement::Assign(
                        AssignDest::Id(main_local!("flag")),
                        Expr::Constant(Value::Bool(false)),
                        None,
                    ),
                    Statement::WhileLoop(
                        Expr::Constant(Value::Bool(false)),
                        vec![Statement::Assign(
                            AssignDest::Id(main_local!("g")),
                            Expr::Lambda(Function {
                                name: t_global!("__while_lam"),
                                body: vec![Statement::Expr(Expr::Id(t_local!(
                                    "flag",
                                    t_global!("__while_lam")
                                )))],
                                types: TypeEnv::new(),
                                params: IndexMap::new(),
                                return_type: ValueType::NoneType,
                            }),
                            Some(ValueType::FunctionType(
                                vec![],
                                Box::new(ValueType::NoneType),
                            )),
                        )],
                    ),
                ],
                types: TypeEnv::new(),
                params: IndexMap::new(),
                return_type: ValueType::IntType,
            }],
            global_types: TypeEnv::new(),
        };

        program = TypeCheck.run_pass(program);
        program = ClosurizeLambdas.run_pass(program);

        assert_eq!(program.functions.len(), 2);
        let lam = program
            .functions
            .iter()
            .find(|f| f.name == t_global!("__while_lam"))
            .unwrap();
        assert_eq!(lam.params.len(), 1);
        assert_eq!(
            *lam.params.get_index(0).unwrap().1,
            ValueType::TupleType(vec![
                ValueType::FunctionType(
                    vec![ValueType::TupleType(vec![])],
                    Box::new(ValueType::NoneType),
                ),
                ValueType::BoolType,
            ]),
        );
    }
}
