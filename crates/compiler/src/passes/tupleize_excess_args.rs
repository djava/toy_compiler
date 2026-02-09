use indexmap::IndexMap;

use crate::{
    constants::MAX_REGISTER_ARGS,
    passes::ASTPass,
    syntax_trees::{ast::*, shared::*},
};

pub struct TupleizeExcessArgs;

impl ASTPass for TupleizeExcessArgs {
    fn run_pass(self, mut m: Program) -> Program {
        for f in m.functions.iter_mut() {
            // Any function with more than 6 args has its 6th-n'th
            // args collected into a tuple to pass as one arg,
            // instead of doing the typical stack parameter passing.
            if f.params.len() > MAX_REGISTER_ARGS {
                // The first `MAX_REGISTER_ARGS - 1` args are kept normal
                let mut new_params: IndexMap<_, _> = f
                    .params
                    .iter()
                    .take(MAX_REGISTER_ARGS - 1)
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .collect();

                // Any args past the `MAX_REGISTER_ARGS - 1`-th is
                // "excess" and grouped up into a tuple
                let (excess_names, excess_types): (Vec<_>, Vec<_>) =
                    f.params.iter().skip(MAX_REGISTER_ARGS - 1).unzip();

                let tuple_id = Identifier::new_ephemeral();
                new_params.insert(
                    tuple_id.clone(),
                    ValueType::TupleType(excess_types.into_iter().cloned().collect()),
                );

                // Replace any uses of an excess arg with a subscript
                // into the excess-args tuple
                for s in f.body.iter_mut() {
                    replace_excess_use_for_statement(s, &excess_names, &tuple_id);
                }

                f.params = new_params;

                // Update function_types map for the new type
                if let ValueType::FunctionType(old_args_types, ret_type) = &m.function_types[&f.name] {
                    let mut new_args_types: Vec<_> = old_args_types
                        .iter()
                        .take(MAX_REGISTER_ARGS - 1)
                        .cloned()
                        .collect();

                    let excess_args_types = old_args_types
                        .iter()
                        .skip(MAX_REGISTER_ARGS - 1)
                        .cloned()
                        .collect();
                    new_args_types.push(ValueType::TupleType(excess_args_types));

                    m.function_types
                        .insert(f.name.clone(), ValueType::FunctionType(new_args_types, ret_type.clone()));
                } else {
                    panic!("Couldn't find type-checker entry for {:?}", f.name);
                }
            }

            // Also, any calls to functions with >6 args will have the
            // 6th through n'th collected into a tuple. We know the
            // called function is expecting this, because of the
            // operation that happens right above this block
            for s in f.body.iter_mut() {
                replace_excess_calls_for_statement(s);
            }
        }

        m
    }
}

fn replace_excess_use_for_statement(
    s: &mut Statement,
    excess_names: &Vec<&Identifier>,
    tuple_id: &Identifier,
) {
    match s {
        Statement::Assign(assign_dest, expr) => {
            match assign_dest {
                AssignDest::Id(id) => {
                    if let Some(subscript_idx) = excess_names.iter().position(|x| *x == id) {
                        *assign_dest =
                            AssignDest::Subscript(tuple_id.clone(), subscript_idx as i64);
                    }
                }
                AssignDest::Subscript(id, _idx) => {
                    if let Some(_subscript_idx) = excess_names.iter().position(|x| *x == id) {
                        // *assign_dest = AssignDest::Subscript((), ())
                        todo!(
                            "Need to replace AssignDest::Subscript first arg with Box<AssignDest> to allow x[0][0] = .."
                        );
                    }
                }
            }

            replace_excess_use_for_expr(expr, excess_names, tuple_id);
        }
        Statement::Conditional(cond, pos_body, neg_body) => {
            replace_excess_use_for_expr(cond, excess_names, tuple_id);
            for s in pos_body {
                replace_excess_use_for_statement(s, excess_names, tuple_id);
            }
            for s in neg_body {
                replace_excess_use_for_statement(s, excess_names, tuple_id);
            }
        }
        Statement::WhileLoop(cond, body) => {
            replace_excess_use_for_expr(cond, excess_names, tuple_id);
            for s in body {
                replace_excess_use_for_statement(s, excess_names, tuple_id);
            }
        }
        Statement::Expr(expr) | Statement::Return(expr) => {
            replace_excess_use_for_expr(expr, excess_names, tuple_id)
        }
    }
}

fn replace_excess_use_for_expr(
    expr: &mut Expr,
    excess_names: &Vec<&Identifier>,
    tuple_id: &Identifier,
) {
    match expr {
        Expr::Id(id) => {
            if let Some(subscript_idx) = excess_names.iter().position(|x| *x == id) {
                *expr = Expr::Subscript(Box::new(Expr::Id(tuple_id.clone())), subscript_idx as i64);
            }
        }
        Expr::BinaryOp(l, _, r) => {
            replace_excess_use_for_expr(l, excess_names, tuple_id);
            replace_excess_use_for_expr(r, excess_names, tuple_id);
        }
        Expr::UnaryOp(_, e) => {
            replace_excess_use_for_expr(e, excess_names, tuple_id);
        }
        Expr::Call(_, args) => {
            for a in args {
                replace_excess_use_for_expr(a, excess_names, tuple_id);
            }
        }
        Expr::Ternary(cond, pos, neg) => {
            replace_excess_use_for_expr(cond, excess_names, tuple_id);
            replace_excess_use_for_expr(pos, excess_names, tuple_id);
            replace_excess_use_for_expr(neg, excess_names, tuple_id);
        }
        Expr::StatementBlock(statements, expr) => {
            for s in statements {
                replace_excess_use_for_statement(s, excess_names, tuple_id);
            }
            replace_excess_use_for_expr(expr, excess_names, tuple_id);
        }
        Expr::Tuple(elems) => {
            for e in elems {
                replace_excess_use_for_expr(e, excess_names, tuple_id);
            }
        }
        Expr::Subscript(expr, _) => {
            replace_excess_use_for_expr(expr, excess_names, tuple_id);
        }

        Expr::Allocate(_, _) | Expr::Constant(_) | Expr::GlobalSymbol(_) => {}
    }
}

fn replace_excess_calls_for_statement(s: &mut Statement) {
    match s {
        Statement::Assign(_, expr) | Statement::Expr(expr) | Statement::Return(expr) => {
            replace_excess_calls_for_expr(expr);
        }
        Statement::Conditional(cond, pos_body, neg_body) => {
            replace_excess_calls_for_expr(cond);
            for s in pos_body {
                replace_excess_calls_for_statement(s);
            }
            for s in neg_body {
                replace_excess_calls_for_statement(s);
            }
        }
        Statement::WhileLoop(cond, body) => {
            replace_excess_calls_for_expr(cond);
            for s in body {
                replace_excess_calls_for_statement(s);
            }
        }
    }
}

fn replace_excess_calls_for_expr(e: &mut Expr) {
    match e {
        Expr::Call(_, args) => {
            if args.len() > MAX_REGISTER_ARGS {
                let mut new_args: Vec<_> =
                    args.iter().take(MAX_REGISTER_ARGS - 1).cloned().collect();

                let excess_args = args.iter().skip(MAX_REGISTER_ARGS - 1).cloned();
                let excess_tup = Expr::Tuple(excess_args.collect());
                new_args.push(excess_tup);

                *args = new_args;
            }
        }
        Expr::BinaryOp(l, _, r) => {
            replace_excess_calls_for_expr(l);
            replace_excess_calls_for_expr(r);
        }
        Expr::UnaryOp(_, expr) => {
            replace_excess_calls_for_expr(expr);
        }
        Expr::Ternary(cond, pos, neg) => {
            replace_excess_calls_for_expr(cond);
            replace_excess_calls_for_expr(pos);
            replace_excess_calls_for_expr(neg);
        }
        Expr::StatementBlock(statements, expr) => {
            for s in statements {
                replace_excess_calls_for_statement(s);
            }
            replace_excess_calls_for_expr(expr);
        }
        Expr::Tuple(elems) => {
            for e in elems {
                replace_excess_calls_for_expr(e);
            }
        }
        Expr::Subscript(expr, _) => {
            replace_excess_calls_for_expr(expr);
        }

        Expr::Id(_) | Expr::Allocate(_, _) | Expr::Constant(_) | Expr::GlobalSymbol(_) => {}
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::utils::t_id;
    use indexmap::IndexMap;
    use test_support::{
        ast_const_int, ast_print_int,
        compiler::{
            passes::{ASTPass, TupleizeExcessArgs},
            syntax_trees::{ast::*, shared::*},
        },
    };

    struct TestCase {
        ast: Program,
    }

    fn check_invariant_statement(
        before: &Statement,
        after: &Statement,
        tup_id: &Identifier,
        map: &HashMap<Identifier, i64>,
    ) {
        match (before, after) {
            (Statement::Assign(_, b_expr), Statement::Assign(_, a_expr))
            | (Statement::Expr(b_expr), Statement::Expr(a_expr)) => {
                check_invariant_expr(b_expr, a_expr, tup_id, map);
            }
            (
                Statement::Conditional(b_cond, b_pos_body, b_neg_body),
                Statement::Conditional(a_cond, a_pos_body, a_neg_body),
            ) => {
                check_invariant_expr(b_cond, a_cond, tup_id, map);
                for (b_s, a_s) in b_pos_body.iter().zip(a_pos_body) {
                    check_invariant_statement(b_s, a_s, tup_id, map);
                }
                for (b_s, a_s) in b_neg_body.iter().zip(a_neg_body) {
                    check_invariant_statement(b_s, a_s, tup_id, map);
                }
            }
            (Statement::WhileLoop(b_cond, b_body), Statement::WhileLoop(a_cond, a_body)) => {
                check_invariant_expr(b_cond, a_cond, tup_id, map);
                for (b_s, a_s) in b_body.iter().zip(a_body) {
                    check_invariant_statement(b_s, a_s, tup_id, map);
                }
            }
            _ => {}
        }
    }

    fn check_invariant_expr(
        before: &Expr,
        after: &Expr,
        tup_id: &Identifier,
        map: &HashMap<Identifier, i64>,
    ) {
        match (before, after) {
            (Expr::Id(b_id), _) => {
                if let Some(subscript_idx) = map.get(b_id) {
                    assert_eq!(
                        after,
                        &Expr::Subscript(Box::new(Expr::Id(tup_id.clone())), *subscript_idx)
                    );
                }
            }
            (Expr::BinaryOp(b_l, _, b_r), Expr::BinaryOp(a_l, _, a_r)) => {
                check_invariant_expr(b_l, a_l, tup_id, map);
                check_invariant_expr(b_r, a_r, tup_id, map);
            }
            (Expr::UnaryOp(_, b_e), Expr::UnaryOp(_, a_e)) => {
                check_invariant_expr(b_e, a_e, tup_id, map);
            }
            (Expr::Call(_, b_args), Expr::Call(_, a_args)) => {
                // Will be different if >= 5 because of this operation,
                // not sure how to deal with that. just skip for now
                for (b_a, a_a) in b_args.iter().zip(a_args).take(5) {
                    check_invariant_expr(b_a, a_a, tup_id, map);
                }

                assert!(a_args.len() <= 6);
            }
            (Expr::Ternary(b_cond, b_pos, b_neg), Expr::Ternary(a_cond, a_pos, a_neg)) => {
                check_invariant_expr(b_cond, a_cond, tup_id, map);
                check_invariant_expr(b_pos, a_pos, tup_id, map);
                check_invariant_expr(b_neg, a_neg, tup_id, map);
            }
            (
                Expr::StatementBlock(b_statements, b_expr),
                Expr::StatementBlock(a_statements, a_expr),
            ) => {
                for (b_s, a_s) in b_statements.iter().zip(a_statements) {
                    check_invariant_statement(b_s, a_s, tup_id, map);
                }
                check_invariant_expr(b_expr, a_expr, tup_id, map);
            }
            (Expr::Tuple(b_elems), Expr::Tuple(a_elems)) => {
                for (b_e, a_e) in b_elems.iter().zip(a_elems) {
                    check_invariant_expr(b_e, a_e, tup_id, map);
                }
            }
            (Expr::Subscript(b_expr, _), Expr::Subscript(a_expr, _)) => {
                check_invariant_expr(b_expr, a_expr, tup_id, map);
            }

            _ => {}
        }
    }

    fn check_invariant(before: &Program, after: &Program) {
        for (b_f, a_f) in before.functions.iter().zip(&after.functions) {
            if b_f.params.len() > 6 {
                let mut arg_map = HashMap::new();
                for (idx, (name, _)) in b_f.params.iter().skip(5).enumerate() {
                    arg_map.insert(name.clone(), idx as i64);
                }

                let tup_id = a_f.params.get_index(5).unwrap().0;
                for (b_s, a_s) in b_f.body.iter().zip(&a_f.body) {
                    check_invariant_statement(b_s, a_s, tup_id, &arg_map);
                }
            }
        }
    }

    fn execute_test_case(tc: TestCase) {
        let before = tc.ast.clone();
        println!("-- AST before TupleizeExcessArgs:\n{:?}", tc.ast);
        let post_run = TupleizeExcessArgs.run_pass(tc.ast);
        println!("-- AST after TupleizeExcessArgs:\n{post_run:?}");

        check_invariant(&before, &post_run);
    }

    #[test]
    fn test_non_excess_args() {
        execute_test_case(TestCase {
            ast: Program {
                functions: vec![Function {
                    name: t_id!("a"),
                    params: IndexMap::from_iter([
                        (t_id!("a1"), ValueType::IntType),
                        (t_id!("a2"), ValueType::IntType),
                        (t_id!("a3"), ValueType::IntType),
                        (t_id!("a4"), ValueType::IntType),
                    ]),
                    body: vec![
                        Statement::Expr(ast_print_int(Expr::Id(t_id!("a1")))),
                        Statement::Expr(ast_print_int(Expr::Id(t_id!("a2")))),
                        Statement::Expr(ast_print_int(Expr::Id(t_id!("a3")))),
                        Statement::Expr(ast_print_int(Expr::Id(t_id!("a4")))),
                    ],
                    types: TypeEnv::new(),
                    return_type: ValueType::NoneType,
                }],
                function_types: TypeEnv::from_iter([(
                    t_id!("a"),
                    ValueType::FunctionType(vec![ValueType::IntType; 4], Box::new(ValueType::NoneType)),
                )]),
            },
        });
    }

    #[test]
    fn test_single_func_excess_args() {
        execute_test_case(TestCase {
            ast: Program {
                functions: vec![Function {
                    name: t_id!("a"),
                    params: IndexMap::from_iter([
                        (t_id!("a1"), ValueType::IntType),
                        (t_id!("a2"), ValueType::IntType),
                        (t_id!("a3"), ValueType::IntType),
                        (t_id!("a4"), ValueType::IntType),
                        (t_id!("a5"), ValueType::IntType),
                        (t_id!("a6"), ValueType::IntType),
                        (t_id!("a7"), ValueType::IntType),
                    ]),
                    body: vec![
                        Statement::Expr(ast_print_int(Expr::Id(t_id!("a1")))),
                        Statement::Expr(ast_print_int(Expr::Id(t_id!("a2")))),
                        Statement::Expr(ast_print_int(Expr::Id(t_id!("a3")))),
                        Statement::Expr(ast_print_int(Expr::Id(t_id!("a4")))),
                        Statement::Expr(ast_print_int(Expr::Id(t_id!("a5")))),
                        Statement::Expr(ast_print_int(Expr::Id(t_id!("a6")))),
                        Statement::Expr(ast_print_int(Expr::Id(t_id!("a7")))),
                    ],
                    types: TypeEnv::new(),
                    return_type: ValueType::NoneType,
                }],
                function_types: TypeEnv::from_iter([(
                    t_id!("a"),
                    ValueType::FunctionType(vec![ValueType::IntType; 7], Box::new(ValueType::NoneType)),
                )]),
            },
        });
    }

    #[test]
    fn test_single_func_many_excess_args() {
        execute_test_case(TestCase {
            ast: Program {
                functions: vec![Function {
                    name: t_id!("a"),
                    params: IndexMap::from_iter([
                        (t_id!("a1"), ValueType::IntType),
                        (t_id!("a2"), ValueType::IntType),
                        (t_id!("a3"), ValueType::IntType),
                        (t_id!("a4"), ValueType::IntType),
                        (t_id!("a5"), ValueType::IntType),
                        (t_id!("a6"), ValueType::IntType),
                        (t_id!("a7"), ValueType::IntType),
                        (t_id!("a8"), ValueType::IntType),
                        (t_id!("a9"), ValueType::IntType),
                        (t_id!("a10"), ValueType::IntType),
                        (t_id!("a11"), ValueType::IntType),
                        (t_id!("a12"), ValueType::IntType),
                        (t_id!("a13"), ValueType::IntType),
                        (t_id!("a14"), ValueType::IntType),
                        (t_id!("a15"), ValueType::IntType),
                        (t_id!("a16"), ValueType::IntType),
                        (t_id!("a17"), ValueType::IntType),
                    ]),
                    body: vec![
                        Statement::Expr(ast_print_int(Expr::Id(t_id!("a1")))),
                        Statement::Expr(ast_print_int(Expr::Id(t_id!("a2")))),
                        Statement::Expr(ast_print_int(Expr::Id(t_id!("a3")))),
                        Statement::Expr(ast_print_int(Expr::Id(t_id!("a4")))),
                        Statement::Expr(ast_print_int(Expr::Id(t_id!("a5")))),
                        Statement::Expr(ast_print_int(Expr::Id(t_id!("a6")))),
                        Statement::Expr(ast_print_int(Expr::Id(t_id!("a7")))),
                        Statement::Expr(ast_print_int(Expr::Id(t_id!("a8")))),
                        Statement::Expr(ast_print_int(Expr::Id(t_id!("a9")))),
                        Statement::Expr(ast_print_int(Expr::Id(t_id!("a10")))),
                        Statement::Expr(ast_print_int(Expr::Id(t_id!("a11")))),
                        Statement::Expr(ast_print_int(Expr::Id(t_id!("a12")))),
                        Statement::Expr(ast_print_int(Expr::Id(t_id!("a13")))),
                        Statement::Expr(ast_print_int(Expr::Id(t_id!("a14")))),
                        Statement::Expr(ast_print_int(Expr::Id(t_id!("a15")))),
                        Statement::Expr(ast_print_int(Expr::Id(t_id!("a16")))),
                        Statement::Expr(ast_print_int(Expr::Id(t_id!("a17")))),
                    ],
                    types: TypeEnv::new(),
                    return_type: ValueType::NoneType,
                }],
                function_types: TypeEnv::from_iter([(
                    t_id!("a"),
                    ValueType::FunctionType(vec![ValueType::IntType; 17], Box::new(ValueType::NoneType)),
                )]),
            },
        });
    }

    #[test]
    fn test_multi_func_non_excess_args() {
        execute_test_case(TestCase {
            ast: Program {
                functions: vec![
                    Function {
                        name: t_id!("a"),
                        params: IndexMap::from_iter([
                            (t_id!("a1"), ValueType::IntType),
                            (t_id!("a2"), ValueType::IntType),
                            (t_id!("a3"), ValueType::IntType),
                            (t_id!("a4"), ValueType::IntType),
                        ]),
                        body: vec![
                            Statement::Expr(ast_print_int(Expr::Id(t_id!("a1")))),
                            Statement::Expr(ast_print_int(Expr::Id(t_id!("a2")))),
                            Statement::Expr(ast_print_int(Expr::Id(t_id!("a3")))),
                            Statement::Expr(ast_print_int(Expr::Id(t_id!("a4")))),
                        ],
                        types: TypeEnv::new(),
                        return_type: ValueType::NoneType,
                    },
                    Function {
                        name: t_id!("b"),
                        params: IndexMap::new(),
                        body: vec![Statement::Expr(Expr::Call(
                            t_id!("a"),
                            vec![ast_const_int(1); 4],
                        ))],
                        types: TypeEnv::new(),
                        return_type: ValueType::NoneType,
                    },
                ],
                function_types: TypeEnv::from_iter([(
                    t_id!("a"),
                    ValueType::FunctionType(vec![ValueType::IntType; 4], Box::new(ValueType::NoneType)),
                )]),
            },
        });
    }

    #[test]
    fn test_multi_func_excess_args() {
        execute_test_case(TestCase {
            ast: Program {
                functions: vec![
                    Function {
                        name: t_id!("a"),
                        params: IndexMap::from_iter([
                            (t_id!("a1"), ValueType::IntType),
                            (t_id!("a2"), ValueType::IntType),
                            (t_id!("a3"), ValueType::IntType),
                            (t_id!("a4"), ValueType::IntType),
                            (t_id!("a5"), ValueType::IntType),
                            (t_id!("a6"), ValueType::IntType),
                            (t_id!("a7"), ValueType::IntType),
                            (t_id!("a8"), ValueType::IntType),
                            (t_id!("a9"), ValueType::IntType),
                            (t_id!("a10"), ValueType::IntType),
                            (t_id!("a11"), ValueType::IntType),
                            (t_id!("a12"), ValueType::IntType),
                            (t_id!("a13"), ValueType::IntType),
                            (t_id!("a14"), ValueType::IntType),
                            (t_id!("a15"), ValueType::IntType),
                            (t_id!("a16"), ValueType::IntType),
                            (t_id!("a17"), ValueType::IntType),
                        ]),
                        body: vec![
                            Statement::Expr(ast_print_int(Expr::Id(t_id!("a1")))),
                            Statement::Expr(ast_print_int(Expr::Id(t_id!("a2")))),
                            Statement::Expr(ast_print_int(Expr::Id(t_id!("a3")))),
                            Statement::Expr(ast_print_int(Expr::Id(t_id!("a4")))),
                            Statement::Expr(ast_print_int(Expr::Id(t_id!("a5")))),
                            Statement::Expr(ast_print_int(Expr::Id(t_id!("a6")))),
                            Statement::Expr(ast_print_int(Expr::Id(t_id!("a7")))),
                            Statement::Expr(ast_print_int(Expr::Id(t_id!("a8")))),
                            Statement::Expr(ast_print_int(Expr::Id(t_id!("a9")))),
                            Statement::Expr(ast_print_int(Expr::Id(t_id!("a10")))),
                            Statement::Expr(ast_print_int(Expr::Id(t_id!("a11")))),
                            Statement::Expr(ast_print_int(Expr::Id(t_id!("a12")))),
                            Statement::Expr(ast_print_int(Expr::Id(t_id!("a13")))),
                            Statement::Expr(ast_print_int(Expr::Id(t_id!("a14")))),
                            Statement::Expr(ast_print_int(Expr::Id(t_id!("a15")))),
                            Statement::Expr(ast_print_int(Expr::Id(t_id!("a16")))),
                            Statement::Expr(ast_print_int(Expr::Id(t_id!("a17")))),
                        ],
                        types: TypeEnv::new(),
                        return_type: ValueType::NoneType,
                    },
                    Function {
                        name: t_id!("b"),
                        params: IndexMap::new(),
                        body: vec![Statement::Expr(Expr::Call(
                            t_id!("a"),
                            vec![ast_const_int(1); 17],
                        ))],
                        types: TypeEnv::new(),
                        return_type: ValueType::NoneType,
                    },
                ],
                function_types: TypeEnv::from_iter([(
                    t_id!("a"),
                    ValueType::FunctionType(vec![ValueType::IntType; 17], Box::new(ValueType::NoneType)),
                )]),
            },
        });
    }
}
