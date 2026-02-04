use crate::{ast::*, passes::ASTPass};

pub struct RemoveComplexOperands;

struct ExprTransformation {
    new_expr: Expr,
    ephemeral_assigns: Vec<(Identifier, Expr)>,
}

impl ASTPass for RemoveComplexOperands {
    fn run_pass(self, mut m: Module) -> Module {
        let mut new_body: Vec<Statement> = vec![];

        for s in m.body {
            rco_statement(&s, &mut new_body);
        }

        m.body = new_body;
        m
    }
}

fn rco_statement(s: &Statement, new_body: &mut Vec<Statement>) {
    match s {
        Statement::Assign(dest, expr) => {
            let transform = rco_expr(&expr, false);

            // Take all the ephemeral transforms that happen
            // inside the expression and add them before this
            // statement
            let ephemeral_assign_stmts = transform
                .ephemeral_assigns
                .iter()
                .map(|(id, expr)| Statement::Assign(AssignDest::Id(id.clone()), expr.clone()));
            new_body.extend(ephemeral_assign_stmts);

            // Add the updated version of this statement with
            // the new expression to the body
            new_body.push(Statement::Assign(dest.clone(), transform.new_expr));
        }
        Statement::Expr(expr) => {
            // The expression statement itself doesn't need to
            // be atomic, because its not actually  being used
            // by anything, so theres no dependence on what
            // happens with the output
            let transform = rco_expr(&expr, false);

            // Take all the ephemeral transforms that happen
            // inside the expression and add them before this
            // statement
            let ephemeral_assign_stmts = transform
                .ephemeral_assigns
                .iter()
                .map(|(id, expr)| Statement::Assign(AssignDest::Id(id.clone()), expr.clone()));
            new_body.extend(ephemeral_assign_stmts);

            // Add the updated version of this statement with
            // the new expression to the body
            new_body.push(Statement::Expr(transform.new_expr));
        }
        Statement::Conditional(cond, pos, neg) => {
            // Condition must be non-atomic to enable good codegen
            let cond_transform = rco_expr(cond, false);

            let ephemeral_assign_stmts = cond_transform
                .ephemeral_assigns
                .iter()
                .map(|(id, expr)| Statement::Assign(AssignDest::Id(id.clone()), expr.clone()));
            new_body.extend(ephemeral_assign_stmts);

            let mut pos_new_body = Vec::new();
            pos.iter().for_each(|s| rco_statement(s, &mut pos_new_body));

            let mut neg_new_body = Vec::new();
            neg.iter().for_each(|s| rco_statement(s, &mut neg_new_body));

            new_body.push(Statement::Conditional(
                cond_transform.new_expr,
                pos_new_body,
                neg_new_body,
            ));
        }
        Statement::WhileLoop(cond, loop_body) => {
            // Condition must be non-atomic to enable good codegen
            let cond_transform = rco_expr(cond, false);

            // Put cond in a statement block so that it recalculates it
            // every iteration
            let ephemeral_assign_stmts = cond_transform
                .ephemeral_assigns
                .iter()
                .map(|(id, expr)| Statement::Assign(AssignDest::Id(id.clone()), expr.clone()));
            let cond_statement_block = Expr::StatementBlock(
                ephemeral_assign_stmts.collect(),
                Box::new(cond_transform.new_expr),
            );

            let mut new_loop_body = Vec::new();
            loop_body
                .iter()
                .for_each(|s| rco_statement(s, &mut new_loop_body));

            new_body.push(Statement::WhileLoop(cond_statement_block, new_loop_body));
        }
    }
}

fn rco_expr(e: &Expr, needs_atomicity: bool) -> ExprTransformation {
    match e {
        Expr::BinaryOp(left, op, right) => {
            // Get the transformed versions of the operands first
            let left_transform = rco_expr(&*left, true);
            let right_transform = rco_expr(&*right, true);

            // The ephermal assigns first need to include the ones for
            // the left and right operands, in that order
            let mut ephemeral_assigns = vec![];
            ephemeral_assigns.extend(left_transform.ephemeral_assigns);
            ephemeral_assigns.extend(right_transform.ephemeral_assigns);

            // This same operation, but with the transformed operands
            let transformed_op = Expr::BinaryOp(
                Box::new(left_transform.new_expr),
                *op,
                Box::new(right_transform.new_expr),
            );

            // If *this* expression needs to be atomic, extract it into an
            // assignment and an id-expr. Otherwise, just use it directly.
            let new_expr = if needs_atomicity {
                let id = Identifier::new_ephemeral();
                ephemeral_assigns.push((id.clone(), transformed_op));
                Expr::Id(id)
            } else {
                transformed_op
            };

            ExprTransformation {
                new_expr,
                ephemeral_assigns,
            }
        }
        Expr::UnaryOp(op, val) => {
            let val_transform = rco_expr(&*val, true);

            // The ephermal assigns first need to include the ones for
            // the operand
            let mut ephemeral_assigns = val_transform.ephemeral_assigns;

            let transformed_op = Expr::UnaryOp(*op, Box::new(val_transform.new_expr));

            // If *this* expression needs to be atomic, extract it into an
            // assignment and an id-expr. Otherwise, just use it directly.
            let new_expr = if needs_atomicity {
                let id = Identifier::new_ephemeral();
                ephemeral_assigns.push((id.clone(), transformed_op));
                Expr::Id(id)
            } else {
                transformed_op
            };

            ExprTransformation {
                new_expr,
                ephemeral_assigns,
            }
        }
        Expr::Constant(_) => ExprTransformation {
            new_expr: e.clone(),
            ephemeral_assigns: vec![],
        },
        Expr::Id(_) => ExprTransformation {
            new_expr: e.clone(),
            ephemeral_assigns: vec![],
        },
        Expr::Call(name, args) => {
            let mut ephemeral_assigns = vec![];
            let mut new_args = vec![];
            // Each arg must be transformed, and all the ephermal
            // assignments must be inserted before this call happens
            for arg in args {
                let arg_transform = rco_expr(&*arg, true);
                ephemeral_assigns.extend(arg_transform.ephemeral_assigns);
                new_args.push(arg_transform.new_expr);
            }

            let transformed_call = Expr::Call(name.clone(), new_args);

            let new_expr = if needs_atomicity {
                let id = Identifier::new_ephemeral();
                ephemeral_assigns.push((id.clone(), transformed_call));
                Expr::Id(id)
            } else {
                transformed_call
            };

            ExprTransformation {
                new_expr,
                ephemeral_assigns,
            }
        }
        Expr::Ternary(cond, pos, neg) => {
            let mut ephemeral_assigns = vec![];

            // Condition must be non-atomic to enable good codegen
            let transformed_cond = rco_expr(cond, false);
            let transformed_pos = rco_expr(pos, true);
            let transformed_neg = rco_expr(neg, true);

            ephemeral_assigns.extend(transformed_cond.ephemeral_assigns);
            let new_pos = if transformed_pos.ephemeral_assigns.is_empty() {
                transformed_pos.new_expr
            } else {
                Expr::StatementBlock(
                    transformed_pos
                        .ephemeral_assigns
                        .into_iter()
                        .map(|(dest_id, e)| Statement::Assign(AssignDest::Id(dest_id), e))
                        .collect(),
                    Box::new(transformed_pos.new_expr),
                )
            };
            let new_neg = if transformed_neg.ephemeral_assigns.is_empty() {
                transformed_neg.new_expr
            } else {
                Expr::StatementBlock(
                    transformed_neg
                        .ephemeral_assigns
                        .into_iter()
                        .map(|(dest_id, e)| Statement::Assign(AssignDest::Id(dest_id), e))
                        .collect(),
                    Box::new(transformed_neg.new_expr),
                )
            };

            let transformed_ternary = Expr::Ternary(
                Box::new(transformed_cond.new_expr),
                Box::new(new_pos),
                Box::new(new_neg),
            );

            let new_expr = if needs_atomicity {
                let id = Identifier::new_ephemeral();
                ephemeral_assigns.push((id.clone(), transformed_ternary));
                Expr::Id(id)
            } else {
                transformed_ternary
            };

            ExprTransformation {
                new_expr,
                ephemeral_assigns,
            }
        }
        Expr::StatementBlock(statements, expr) => {
            let mut new_body = vec![];
            statements
                .iter()
                .for_each(|s| rco_statement(s, &mut new_body));

            let transformed_expr = rco_expr(expr, true);

            let ephemeral_assigns = transformed_expr.ephemeral_assigns;
            let transformed_block =
                Expr::StatementBlock(new_body, Box::new(transformed_expr.new_expr));

            let new_expr = if needs_atomicity {
                let id = Identifier::new_ephemeral();
                let ephemeral_assign =
                    Statement::Assign(AssignDest::Id(id.clone()), transformed_block);

                Expr::StatementBlock(vec![ephemeral_assign], Box::new(Expr::Id(id)))
            } else {
                transformed_block
            };

            ExprTransformation {
                new_expr,
                ephemeral_assigns,
            }
        }
        Expr::Tuple(elems) => {
            let mut ephemeral_assigns = vec![];
            let mut new_elems = vec![];

            // Each arg must be transformed, and all the ephermal
            // assignments must be inserted before this call happens
            for elem in elems {
                let elem_transform = rco_expr(&*elem, true);
                ephemeral_assigns.extend(elem_transform.ephemeral_assigns);
                new_elems.push(elem_transform.new_expr);
            }

            let new_expr = if needs_atomicity {
                let id = Identifier::new_ephemeral();
                ephemeral_assigns.push((id.clone(), Expr::Tuple(new_elems)));
                Expr::Id(id)
            } else {
                Expr::Tuple(new_elems)
            };

            ExprTransformation {
                new_expr,
                ephemeral_assigns,
            }
        }
        Expr::Subscript(tup, index_val) => {
            let mut ephemeral_assigns = vec![];

            let tup_transform = rco_expr(&*tup, true);
            ephemeral_assigns.extend(tup_transform.ephemeral_assigns);

            let new_subscript =
                Expr::Subscript(Box::new(tup_transform.new_expr), index_val.clone());

            let new_expr = if needs_atomicity {
                let id = Identifier::new_ephemeral();
                ephemeral_assigns.push((id.clone(), new_subscript));
                Expr::Id(id)
            } else {
                new_subscript
            };

            ExprTransformation {
                new_expr,
                ephemeral_assigns,
            }
        }
        Expr::GlobalSymbol(_) => ExprTransformation {
            new_expr: e.clone(),
            ephemeral_assigns: vec![],
        },
        Expr::Allocate(_, _) => {
            panic!("This pass should've happened before any Allocate's were injected")
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;

    use test_support::{
        ast_interpreter::interpret,
        compiler::{
            ast::*,
            passes::{ASTPass, remove_complex_operands::RemoveComplexOperands},
            utils::type_check_ast_statements
        },
    };

    struct TestCase {
        ast: Module,
        inputs: VecDeque<i64>,
        expected_outputs: VecDeque<i64>,
    }

    fn is_atomic(e: &Expr) -> bool {
        matches!(e, Expr::Constant(_) | Expr::Id(_) | Expr::GlobalSymbol(_))
    }

    fn check_expr_invariants(e: &Expr) {
        match e {
            Expr::BinaryOp(left, _, right) => {
                assert!(is_atomic(left), "BinaryOp left operand must be atomic, got {left:?}");
                assert!(is_atomic(right), "BinaryOp right operand must be atomic, got {right:?}");
            }
            Expr::UnaryOp(_, val) => {
                assert!(is_atomic(val), "UnaryOp operand must be atomic, got {val:?}");
            }
            Expr::Call(_, args) => {
                for arg in args {
                    assert!(is_atomic(arg), "Call argument must be atomic, got {arg:?}");
                }
            }
            Expr::Tuple(elems) => {
                for elem in elems {
                    assert!(is_atomic(elem), "Tuple element must be atomic, got {elem:?}");
                }
            }
            Expr::Subscript(tup, _) => {
                assert!(is_atomic(tup), "Subscript target must be atomic, got {tup:?}");
            }
            Expr::Ternary(cond, pos, neg) => {
                check_expr_invariants(cond);
                check_expr_invariants(pos);
                check_expr_invariants(neg);
            }
            Expr::StatementBlock(stmts, expr) => {
                stmts.iter().for_each(check_statement_invariants);
                check_expr_invariants(expr);
            }
            Expr::Constant(_) | Expr::Id(_) | Expr::GlobalSymbol(_) | Expr::Allocate(_, _) => {}
        }
    }

    fn check_statement_invariants(s: &Statement) {
        match s {
            Statement::Assign(_, expr) => check_expr_invariants(expr),
            Statement::Expr(expr) => check_expr_invariants(expr),
            Statement::Conditional(cond, pos, neg) => {
                check_expr_invariants(cond);
                pos.iter().for_each(check_statement_invariants);
                neg.iter().for_each(check_statement_invariants);
            }
            Statement::WhileLoop(cond, body) => {
                check_expr_invariants(cond);
                body.iter().for_each(check_statement_invariants);
            }
        }
    }

    fn check_invariants(m: &Module) {
        m.body.iter().for_each(check_statement_invariants);
    }

    fn execute_test_case(mut tc: TestCase) {
        type_check_ast_statements(&tc.ast.body, &mut TypeEnv::new());

        println!("AST before RCO: {:#?}", tc.ast);
        let post_run_ast = RemoveComplexOperands.run_pass(tc.ast);
        println!("AST after RCO: {:#?}", post_run_ast);

        type_check_ast_statements(&post_run_ast.body, &mut TypeEnv::new());
        check_invariants(&post_run_ast);

        let mut outputs = VecDeque::<i64>::new();
        interpret(&post_run_ast, &mut tc.inputs, &mut outputs);

        assert_eq!(outputs, tc.expected_outputs);
    }

    #[test]
    fn test_add() {
        execute_test_case(TestCase {
            ast: Module {
                body: vec![Statement::Expr(Expr::Call(
                    Identifier::from("print_int"),
                    vec![Expr::BinaryOp(
                        Box::new(Expr::Constant(Value::I64(40))),
                        BinaryOperator::Add,
                        Box::new(Expr::Constant(Value::I64(2))),
                    )],
                ))],
                types: TypeEnv::new(),
            },
            inputs: VecDeque::new(),
            expected_outputs: VecDeque::from(vec![42]),
        })
    }

    #[test]
    fn test_input() {
        execute_test_case(TestCase {
            ast: Module {
                body: vec![Statement::Expr(Expr::Call(
                    Identifier::from("print_int"),
                    vec![Expr::Call(Identifier::from("read_int"), vec![])],
                ))],
                types: TypeEnv::new(),
            },
            inputs: VecDeque::from(vec![42]),
            expected_outputs: VecDeque::from(vec![42]),
        })
    }

    #[test]
    fn test_subinput() {
        execute_test_case(TestCase {
            ast: Module {
                body: vec![Statement::Expr(Expr::Call(
                    Identifier::from("print_int"),
                    vec![Expr::BinaryOp(
                        Box::new(Expr::Call(Identifier::from("read_int"), vec![])),
                        BinaryOperator::Subtract,
                        Box::new(Expr::Call(Identifier::from("read_int"), vec![])),
                    )],
                ))],
                types: TypeEnv::new(),
            },
            inputs: VecDeque::from(vec![5, 3]),
            expected_outputs: VecDeque::from(vec![2]),
        });
    }

    #[test]
    fn test_zero() {
        execute_test_case(TestCase {
            ast: Module {
                body: vec![Statement::Expr(Expr::Call(
                    Identifier::from("print_int"),
                    vec![Expr::Constant(Value::I64(0))],
                ))],
                types: TypeEnv::new(),
            },
            inputs: VecDeque::from(vec![]),
            expected_outputs: VecDeque::from(vec![0]),
        });
    }

    #[test]
    fn test_nested() {
        execute_test_case(TestCase {
            ast: Module {
                body: vec![Statement::Expr(Expr::Call(
                    Identifier::from("print_int"),
                    vec![Expr::BinaryOp(
                        Box::new(Expr::BinaryOp(
                            Box::new(Expr::Constant(Value::I64(40))),
                            BinaryOperator::Add,
                            Box::new(Expr::Constant(Value::I64(2))),
                        )),
                        BinaryOperator::Add,
                        Box::new(Expr::BinaryOp(
                            Box::new(Expr::Constant(Value::I64(40))),
                            BinaryOperator::Add,
                            Box::new(Expr::Constant(Value::I64(2))),
                        )),
                    )],
                ))],
                types: TypeEnv::new(),
            },
            inputs: VecDeque::from(vec![]),
            expected_outputs: VecDeque::from(vec![84]),
        });
    }

    #[test]
    fn test_mixed() {
        execute_test_case(TestCase {
            ast: Module {
                body: vec![Statement::Expr(Expr::Call(
                    Identifier::from("print_int"),
                    vec![Expr::BinaryOp(
                        Box::new(Expr::BinaryOp(
                            Box::new(Expr::Call(Identifier::from("read_int"), vec![])),
                            BinaryOperator::Add,
                            Box::new(Expr::Constant(Value::I64(2))),
                        )),
                        BinaryOperator::Add,
                        Box::new(Expr::BinaryOp(
                            Box::new(Expr::Constant(Value::I64(40))),
                            BinaryOperator::Add,
                            Box::new(Expr::Constant(Value::I64(2))),
                        )),
                    )],
                ))],
                types: TypeEnv::new(),
            },
            inputs: VecDeque::from(vec![-100]),
            expected_outputs: VecDeque::from(vec![44 - 100]),
        });
    }

    #[test]
    fn test_simple_assignment() {
        execute_test_case(TestCase {
            ast: Module {
                body: vec![
                    Statement::Assign(
                        AssignDest::Id(Identifier::from("x")),
                        Expr::Constant(Value::I64(1000)),
                    ),
                    Statement::Expr(Expr::Call(
                        Identifier::from("print_int"),
                        vec![Expr::Id(Identifier::from("x"))],
                    )),
                ],
                types: TypeEnv::new(),
            },
            inputs: VecDeque::from(vec![]),
            expected_outputs: VecDeque::from(vec![1000]),
        });
    }

    #[test]
    fn test_complex_assignment() {
        // -- Original
        // print((input_int() + 2) + (40 - 2))

        // -- Expected
        // e0 = input_int()
        // e1 = e0 + 2
        // e2 = 40 - 2
        // e3 = e1 + e2
        // print(e3)
        execute_test_case(TestCase {
            ast: Module {
                body: vec![
                    Statement::Assign(
                        AssignDest::Id(Identifier::from("foofoo")),
                        Expr::BinaryOp(
                            Box::new(Expr::BinaryOp(
                                Box::new(Expr::Call(Identifier::from("read_int"), vec![])),
                                BinaryOperator::Add,
                                Box::new(Expr::Constant(Value::I64(2))),
                            )),
                            BinaryOperator::Add,
                            Box::new(Expr::BinaryOp(
                                Box::new(Expr::Constant(Value::I64(40))),
                                BinaryOperator::Subtract,
                                Box::new(Expr::Constant(Value::I64(2))),
                            )),
                        ),
                    ),
                    Statement::Expr(Expr::Call(
                        Identifier::from("print_int"),
                        vec![Expr::Id(Identifier::from("foofoo"))],
                    )),
                ],
                types: TypeEnv::new(),
            },
            inputs: VecDeque::from(vec![10]),
            expected_outputs: VecDeque::from(vec![10 + 2 + 40 - 2]),
        });
    }

    #[test]
    fn test_complex_args() {
        execute_test_case(TestCase {
            ast: Module {
                body: vec![Statement::Expr(Expr::Call(
                    Identifier::from("print_int"),
                    vec![Expr::BinaryOp(
                        Box::new(Expr::BinaryOp(
                            Box::new(Expr::Call(Identifier::from("read_int"), vec![])),
                            BinaryOperator::Add,
                            Box::new(Expr::Constant(Value::I64(2))),
                        )),
                        BinaryOperator::Add,
                        Box::new(Expr::BinaryOp(
                            Box::new(Expr::Constant(Value::I64(40))),
                            BinaryOperator::Subtract,
                            Box::new(Expr::Constant(Value::I64(2))),
                        )),
                    )],
                ))],
                types: TypeEnv::new(),
            },
            inputs: VecDeque::from(vec![10]),
            expected_outputs: VecDeque::from(vec![10 + 2 + 40 - 2]),
        });
    }

    #[test]
    fn test_cascading_assigns() {
        // -- Original
        // foo = input_int(10)        # 10
        // bar = input_int(20) + foo  # 20 + 10 = 30
        // baz = input_int(30) + bar  # 30 + 30 = 60
        // bop = foo + bar + baz      # 10 + 30 + 60 = 100
        // print(input_int(40) + bop) # 40 + 100 = 140

        // -- Expected
        // foo = input_int(10)
        // e0 = input_int(20)
        // bar = e0 + foo
        // e1 = input_int(30)
        // baz = e1 + bar
        // e2 = foo + bar
        // bop = e2 + baz
        // e3 = input_int(40)
        // e4 = e3 + bop
        // print(e4)

        execute_test_case(TestCase {
            ast: Module {
                body: vec![
                    Statement::Assign(
                        AssignDest::Id(Identifier::from("foo")),
                        Expr::Call(Identifier::from("read_int"), vec![]),
                    ),
                    Statement::Assign(
                        AssignDest::Id(Identifier::from("bar")),
                        Expr::BinaryOp(
                            Box::new(Expr::Call(Identifier::from("read_int"), vec![])),
                            BinaryOperator::Add,
                            Box::new(Expr::Id(Identifier::from("foo"))),
                        ),
                    ),
                    Statement::Assign(
                        AssignDest::Id(Identifier::from("baz")),
                        Expr::BinaryOp(
                            Box::new(Expr::Call(Identifier::from("read_int"), vec![])),
                            BinaryOperator::Add,
                            Box::new(Expr::Id(Identifier::from("bar"))),
                        ),
                    ),
                    Statement::Assign(
                        AssignDest::Id(Identifier::from("bop")),
                        Expr::BinaryOp(
                            Box::new(Expr::BinaryOp(
                                Box::new(Expr::Id(Identifier::from("foo"))),
                                BinaryOperator::Add,
                                Box::new(Expr::Id(Identifier::from("bar"))),
                            )),
                            BinaryOperator::Add,
                            Box::new(Expr::Id(Identifier::from("baz"))),
                        ),
                    ),
                    Statement::Expr(Expr::Call(
                        Identifier::from("print_int"),
                        vec![Expr::BinaryOp(
                            Box::new(Expr::Call(Identifier::from("read_int"), vec![])),
                            BinaryOperator::Add,
                            Box::new(Expr::Id(Identifier::from("bop"))),
                        )],
                    )),
                ],
                types: TypeEnv::new(),
            },
            inputs: VecDeque::from(vec![10, 20, 30, 40]),
            expected_outputs: VecDeque::from(vec![10 + (10 + 20) + (10 + 20 + 30) + 40]),
        });
    }

    #[test]
    fn test_while_loop_simple() {
        // x = 5
        // while x > 0 {
        //     print_int(x)
        //     x = x - 1
        // }
        execute_test_case(TestCase {
            ast: Module {
                body: vec![
                    Statement::Assign(
                        AssignDest::Id(Identifier::from("x")),
                        Expr::Constant(Value::I64(5)),
                    ),
                    Statement::WhileLoop(
                        Expr::BinaryOp(
                            Box::new(Expr::Id(Identifier::from("x"))),
                            BinaryOperator::Greater,
                            Box::new(Expr::Constant(Value::I64(0))),
                        ),
                        vec![
                            Statement::Expr(Expr::Call(
                                Identifier::from("print_int"),
                                vec![Expr::Id(Identifier::from("x"))],
                            )),
                            Statement::Assign(
                                AssignDest::Id(Identifier::from("x")),
                                Expr::BinaryOp(
                                    Box::new(Expr::Id(Identifier::from("x"))),
                                    BinaryOperator::Subtract,
                                    Box::new(Expr::Constant(Value::I64(1))),
                                ),
                            ),
                        ],
                    ),
                ],
                types: TypeEnv::new(),
            },
            inputs: VecDeque::new(),
            expected_outputs: VecDeque::from(vec![5, 4, 3, 2, 1]),
        });
    }

    #[test]
    fn test_while_loop_complex_condition() {
        // x = 10
        // while (x - 5) > 0 {
        //     print_int(x)
        //     x = x - 1
        // }
        // The condition should have ephemeral assignment extracted
        execute_test_case(TestCase {
            ast: Module {
                body: vec![
                    Statement::Assign(
                        AssignDest::Id(Identifier::from("x")),
                        Expr::Constant(Value::I64(10)),
                    ),
                    Statement::WhileLoop(
                        Expr::BinaryOp(
                            Box::new(Expr::BinaryOp(
                                Box::new(Expr::Id(Identifier::from("x"))),
                                BinaryOperator::Subtract,
                                Box::new(Expr::Constant(Value::I64(5))),
                            )),
                            BinaryOperator::Greater,
                            Box::new(Expr::Constant(Value::I64(0))),
                        ),
                        vec![
                            Statement::Expr(Expr::Call(
                                Identifier::from("print_int"),
                                vec![Expr::Id(Identifier::from("x"))],
                            )),
                            Statement::Assign(
                                AssignDest::Id(Identifier::from("x")),
                                Expr::BinaryOp(
                                    Box::new(Expr::Id(Identifier::from("x"))),
                                    BinaryOperator::Subtract,
                                    Box::new(Expr::Constant(Value::I64(1))),
                                ),
                            ),
                        ],
                    ),
                ],
                types: TypeEnv::new(),
            },
            inputs: VecDeque::new(),
            expected_outputs: VecDeque::from(vec![10, 9, 8, 7, 6]),
        });
    }

    #[test]
    fn test_while_loop_complex_body() {
        // i = 3
        // while i > 0 {
        //     print_int((read_int() + 10) + (20 - 5))
        //     i = i - 1
        // }
        execute_test_case(TestCase {
            ast: Module {
                body: vec![
                    Statement::Assign(
                        AssignDest::Id(Identifier::from("i")),
                        Expr::Constant(Value::I64(3)),
                    ),
                    Statement::WhileLoop(
                        Expr::BinaryOp(
                            Box::new(Expr::Id(Identifier::from("i"))),
                            BinaryOperator::Greater,
                            Box::new(Expr::Constant(Value::I64(0))),
                        ),
                        vec![
                            Statement::Expr(Expr::Call(
                                Identifier::from("print_int"),
                                vec![Expr::BinaryOp(
                                    Box::new(Expr::BinaryOp(
                                        Box::new(Expr::Call(Identifier::from("read_int"), vec![])),
                                        BinaryOperator::Add,
                                        Box::new(Expr::Constant(Value::I64(10))),
                                    )),
                                    BinaryOperator::Add,
                                    Box::new(Expr::BinaryOp(
                                        Box::new(Expr::Constant(Value::I64(20))),
                                        BinaryOperator::Subtract,
                                        Box::new(Expr::Constant(Value::I64(5))),
                                    )),
                                )],
                            )),
                            Statement::Assign(
                                AssignDest::Id(Identifier::from("i")),
                                Expr::BinaryOp(
                                    Box::new(Expr::Id(Identifier::from("i"))),
                                    BinaryOperator::Subtract,
                                    Box::new(Expr::Constant(Value::I64(1))),
                                ),
                            ),
                        ],
                    ),
                ],
                types: TypeEnv::new(),
            },
            inputs: VecDeque::from(vec![1, 2, 3]),
            expected_outputs: VecDeque::from(vec![1 + 10 + 15, 2 + 10 + 15, 3 + 10 + 15]),
        });
    }
}
