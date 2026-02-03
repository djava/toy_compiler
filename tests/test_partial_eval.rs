mod infra;
use std::collections::VecDeque;

use cs4999_compiler::{
    ast::*,
    passes::{ASTPass, TypeCheck, partial_eval::PartialEval},
};

use crate::infra::ast_interpreter::interpret;

struct TestCase {
    ast: Module,
    inputs: VecDeque<i64>,
    expected_outputs: VecDeque<i64>,
}

fn execute_test_case(mut tc: TestCase) {
    println!("\n==================");

    tc.ast = TypeCheck.run_pass(tc.ast);
    println!("Type-check passed on source");

    println!("AST before Partial Eval: {:?}", tc.ast);
    let post_run_ast = PartialEval.run_pass(tc.ast);
    println!("AST after Partial Eval: {:?}", post_run_ast);

    let type_checked = TypeCheck.run_pass(post_run_ast);
    println!("Type-check passed after pass");

    let mut outputs = VecDeque::<i64>::new();
    interpret(&type_checked, &mut tc.inputs, &mut outputs);

    assert_eq!(outputs, tc.expected_outputs);
}

#[test]
fn test_partial_eval_add() {
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
fn test_partial_eval_input() {
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
fn test_partial_eval_subinput() {
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
fn test_partial_eval_zero() {
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
fn test_partial_eval_nested() {
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
fn test_partial_eval_mixed() {
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
fn test_partial_eval_while_loop_simple() {
    // x = 5
    // while x > 0 {
    //     print_int(x)
    //     x = x - 1
    // }
    execute_test_case(TestCase {
        ast: Module {
            body: vec![
                Statement::Assign(AssignDest::Id(Identifier::from("x")), Expr::Constant(Value::I64(5))),
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
fn test_partial_eval_while_loop_constant_false() {
    // while false {
    //     print_int(100)
    // }
    // print_int(42)
    execute_test_case(TestCase {
        ast: Module {
            body: vec![
                Statement::WhileLoop(
                    Expr::Constant(Value::Bool(false)),
                    vec![Statement::Expr(Expr::Call(
                        Identifier::from("print_int"),
                        vec![Expr::Constant(Value::I64(100))],
                    ))],
                ),
                Statement::Expr(Expr::Call(
                    Identifier::from("print_int"),
                    vec![Expr::Constant(Value::I64(42))],
                )),
            ],
            types: TypeEnv::new(),
        },
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![42]), // Loop should be removed entirely
    });
}

#[test]
fn test_partial_eval_while_loop_with_partial_eval_in_body() {
    // x = 3
    // while x > 0 {
    //     print_int(10 + 10)  // Should be partial evaluated to 20
    //     x = x - 1
    // }
    execute_test_case(TestCase {
        ast: Module {
            body: vec![
                Statement::Assign(AssignDest::Id(Identifier::from("x")), Expr::Constant(Value::I64(3))),
                Statement::WhileLoop(
                    Expr::BinaryOp(
                        Box::new(Expr::Id(Identifier::from("x"))),
                        BinaryOperator::Greater,
                        Box::new(Expr::Constant(Value::I64(0))),
                    ),
                    vec![
                        Statement::Expr(Expr::Call(
                            Identifier::from("print_int"),
                            vec![Expr::BinaryOp(
                                Box::new(Expr::Constant(Value::I64(10))),
                                BinaryOperator::Add,
                                Box::new(Expr::Constant(Value::I64(10))),
                            )],
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
        expected_outputs: VecDeque::from(vec![20, 20, 20]),
    });
}
