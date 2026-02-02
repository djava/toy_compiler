mod infra;
use std::collections::VecDeque;

use cs4999_compiler::{
    ast::*,
    passes::{ASTPass, TypeCheck, remove_complex_operands::RemoveComplexOperands},
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

    println!("AST before RCO: {:#?}", tc.ast);
    let post_run_ast = RemoveComplexOperands.run_pass(tc.ast);
    println!("AST after RCO: {:#?}", post_run_ast);

    let type_checked = TypeCheck.run_pass(post_run_ast);
    println!("Type-check passed after pass");

    let mut outputs = VecDeque::<i64>::new();
    interpret(&type_checked, &mut tc.inputs, &mut outputs);

    assert_eq!(outputs, tc.expected_outputs);
}

#[test]
fn test_remove_complex_operands_add() {
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
fn test_remove_complex_operands_input() {
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
fn test_remove_complex_operands_subinput() {
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
fn test_remove_complex_operands_zero() {
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
fn test_remove_complex_operands_nested() {
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
fn test_remove_complex_operands_mixed() {
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
fn test_remove_complex_operands_simple_assignment() {
    execute_test_case(TestCase {
        ast: Module {
            body: vec![
                Statement::Assign(Identifier::from("x"), Expr::Constant(Value::I64(1000))),
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
fn test_remove_complex_operands_complex_assignment() {
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
                    Identifier::from("foofoo"),
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
fn test_remove_complex_operands_complex_args() {
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
fn test_remove_complex_operands_cascading_assigns() {
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
                    Identifier::from("foo"),
                    Expr::Call(Identifier::from("read_int"), vec![]),
                ),
                Statement::Assign(
                    Identifier::from("bar"),
                    Expr::BinaryOp(
                        Box::new(Expr::Call(Identifier::from("read_int"), vec![])),
                        BinaryOperator::Add,
                        Box::new(Expr::Id(Identifier::from("foo"))),
                    ),
                ),
                Statement::Assign(
                    Identifier::from("baz"),
                    Expr::BinaryOp(
                        Box::new(Expr::Call(Identifier::from("read_int"), vec![])),
                        BinaryOperator::Add,
                        Box::new(Expr::Id(Identifier::from("bar"))),
                    ),
                ),
                Statement::Assign(
                    Identifier::from("bop"),
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
fn test_remove_complex_operands_while_loop_simple() {
    // x = 5
    // while x > 0 {
    //     print_int(x)
    //     x = x - 1
    // }
    execute_test_case(TestCase {
        ast: Module {
            body: vec![
                Statement::Assign(Identifier::from("x"), Expr::Constant(Value::I64(5))),
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
                            Identifier::from("x"),
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
fn test_remove_complex_operands_while_loop_complex_condition() {
    // x = 10
    // while (x - 5) > 0 {
    //     print_int(x)
    //     x = x - 1
    // }
    // The condition should have ephemeral assignment extracted
    execute_test_case(TestCase {
        ast: Module {
            body: vec![
                Statement::Assign(Identifier::from("x"), Expr::Constant(Value::I64(10))),
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
                            Identifier::from("x"),
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
fn test_remove_complex_operands_while_loop_complex_body() {
    // i = 3
    // while i > 0 {
    //     print_int((read_int() + 10) + (20 - 5))
    //     i = i - 1
    // }
    execute_test_case(TestCase {
        ast: Module {
            body: vec![
                Statement::Assign(Identifier::from("i"), Expr::Constant(Value::I64(3))),
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
                            Identifier::from("i"),
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
