mod infra;
use std::collections::VecDeque;
use std::sync::Arc;

use cs4999_compiler::{
    ast::*,
    passes::{
        ASTPass, ASTtoIRPass, ShortCircuiting, TranslateASTtoIR, remove_complex_operands::RemoveComplexOperands
    },
};

use crate::infra::{
    ast_const_int, ast_print_int, ast_read_int, ast_type_check::type_check,
    ir_interpreter::interpret_irprogram,
};

struct TestCase {
    ast: Module,
    inputs: VecDeque<i64>,
    expected_outputs: VecDeque<i64>,
}

fn execute_test_case(mut tc: TestCase) {
    println!("\n==================");

    type_check(&tc.ast);
    println!("Type-check passed on source");

    let post_short_circuiting = ShortCircuiting.run_pass(tc.ast);
    let post_rco_ast = RemoveComplexOperands.run_pass(post_short_circuiting);

    println!("-- AST before ASTtoIR:\n{post_rco_ast:?}");
    let post_translation = TranslateASTtoIR.run_pass(post_rco_ast);
    println!("-- AST after ASTtoIR:\n{post_translation:?}");

    let mut outputs = VecDeque::<i64>::new();
    interpret_irprogram(&post_translation, &mut tc.inputs, &mut outputs);

    assert_eq!(outputs, tc.expected_outputs);
}

#[test]
fn test_ast_to_ir_add() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![Statement::Expr(Expr::Call(
            Identifier::Named(Arc::from("print_int")),
            vec![Expr::BinaryOp(
                Box::new(Expr::Constant(Value::I64(40))),
                BinaryOperator::Add,
                Box::new(Expr::Constant(Value::I64(2))),
            )],
        ))]),
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![42]),
    })
}

#[test]
fn test_ast_to_ir_input() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![Statement::Expr(Expr::Call(
            Identifier::Named(Arc::from("print_int")),
            vec![Expr::Call(Identifier::Named(Arc::from("read_int")), vec![])],
        ))]),
        inputs: VecDeque::from(vec![42]),
        expected_outputs: VecDeque::from(vec![42]),
    })
}

#[test]
fn test_ast_to_ir_subinput() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![Statement::Expr(Expr::Call(
            Identifier::Named(Arc::from("print_int")),
            vec![Expr::BinaryOp(
                Box::new(Expr::Call(Identifier::Named(Arc::from("read_int")), vec![])),
                BinaryOperator::Subtract,
                Box::new(Expr::Call(Identifier::Named(Arc::from("read_int")), vec![])),
            )],
        ))]),
        inputs: VecDeque::from(vec![5, 3]),
        expected_outputs: VecDeque::from(vec![2]),
    });
}

#[test]
fn test_ast_to_ir_zero() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![Statement::Expr(Expr::Call(
            Identifier::Named(Arc::from("print_int")),
            vec![Expr::Constant(Value::I64(0))],
        ))]),
        inputs: VecDeque::from(vec![]),
        expected_outputs: VecDeque::from(vec![0]),
    });
}

#[test]
fn test_ast_to_ir_nested() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![Statement::Expr(Expr::Call(
            Identifier::Named(Arc::from("print_int")),
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
        ))]),
        inputs: VecDeque::from(vec![]),
        expected_outputs: VecDeque::from(vec![84]),
    });
}

#[test]
fn test_ast_to_ir_mixed() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![Statement::Expr(Expr::Call(
            Identifier::Named(Arc::from("print_int")),
            vec![Expr::BinaryOp(
                Box::new(Expr::BinaryOp(
                    Box::new(Expr::Call(Identifier::Named(Arc::from("read_int")), vec![])),
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
        ))]),
        inputs: VecDeque::from(vec![-100]),
        expected_outputs: VecDeque::from(vec![44 - 100]),
    });
}

#[test]
fn test_ast_to_ir_simple_assignment() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![
            Statement::Assign(
                Identifier::Named(Arc::from("x")),
                Expr::Constant(Value::I64(1000)),
            ),
            Statement::Expr(Expr::Call(
                Identifier::Named(Arc::from("print_int")),
                vec![Expr::Id(Identifier::Named(Arc::from("x")))],
            )),
        ]),
        inputs: VecDeque::from(vec![]),
        expected_outputs: VecDeque::from(vec![1000]),
    });
}

#[test]
fn test_ast_to_ir_complex_assignment() {
    // -- Original
    // print((input_int() + 2) + (40 - 2))

    // -- Expected
    // e0 = input_int()
    // e1 = e0 + 2
    // e2 = 40 - 2
    // e3 = e1 + e2
    // print(e3)
    execute_test_case(TestCase {
        ast: Module::Body(vec![
            Statement::Assign(
                Identifier::Named(Arc::from("foofoo")),
                Expr::BinaryOp(
                    Box::new(Expr::BinaryOp(
                        Box::new(Expr::Call(Identifier::Named(Arc::from("read_int")), vec![])),
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
                Identifier::Named(Arc::from("print_int")),
                vec![Expr::Id(Identifier::Named(Arc::from("foofoo")))],
            )),
        ]),
        inputs: VecDeque::from(vec![10]),
        expected_outputs: VecDeque::from(vec![10 + 2 + 40 - 2]),
    });
}

#[test]
fn test_ast_to_ir_complex_args() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![Statement::Expr(Expr::Call(
            Identifier::Named(Arc::from("print_int")),
            vec![Expr::BinaryOp(
                Box::new(Expr::BinaryOp(
                    Box::new(Expr::Call(Identifier::Named(Arc::from("read_int")), vec![])),
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
        ))]),
        inputs: VecDeque::from(vec![10]),
        expected_outputs: VecDeque::from(vec![10 + 2 + 40 - 2]),
    });
}

#[test]
fn test_ast_to_ir_cascading_assigns() {
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
        ast: Module::Body(vec![
            Statement::Assign(
                Identifier::Named(Arc::from("foo")),
                Expr::Call(Identifier::Named(Arc::from("read_int")), vec![]),
            ),
            Statement::Assign(
                Identifier::Named(Arc::from("bar")),
                Expr::BinaryOp(
                    Box::new(Expr::Call(Identifier::Named(Arc::from("read_int")), vec![])),
                    BinaryOperator::Add,
                    Box::new(Expr::Id(Identifier::Named(Arc::from("foo")))),
                ),
            ),
            Statement::Assign(
                Identifier::Named(Arc::from("baz")),
                Expr::BinaryOp(
                    Box::new(Expr::Call(Identifier::Named(Arc::from("read_int")), vec![])),
                    BinaryOperator::Add,
                    Box::new(Expr::Id(Identifier::Named(Arc::from("bar")))),
                ),
            ),
            Statement::Assign(
                Identifier::Named(Arc::from("bop")),
                Expr::BinaryOp(
                    Box::new(Expr::BinaryOp(
                        Box::new(Expr::Id(Identifier::Named(Arc::from("foo")))),
                        BinaryOperator::Add,
                        Box::new(Expr::Id(Identifier::Named(Arc::from("bar")))),
                    )),
                    BinaryOperator::Add,
                    Box::new(Expr::Id(Identifier::Named(Arc::from("baz")))),
                ),
            ),
            Statement::Expr(Expr::Call(
                Identifier::Named(Arc::from("print_int")),
                vec![Expr::BinaryOp(
                    Box::new(Expr::Call(Identifier::Named(Arc::from("read_int")), vec![])),
                    BinaryOperator::Add,
                    Box::new(Expr::Id(Identifier::Named(Arc::from("bop")))),
                )],
            )),
        ]),
        inputs: VecDeque::from(vec![10, 20, 30, 40]),
        expected_outputs: VecDeque::from(vec![10 + (10 + 20) + (10 + 20 + 30) + 40]),
    });
}

#[test]
fn test_ast_to_ir_condition_statement() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![Statement::Conditional(
            Expr::BinaryOp(
                Box::new(ast_read_int()),
                BinaryOperator::Equals,
                Box::new(ast_const_int(1)),
            ),
            vec![Statement::Expr(ast_print_int(ast_const_int(10)))],
            vec![Statement::Expr(ast_print_int(ast_const_int(20)))],
        ),
        Statement::Conditional(
            Expr::BinaryOp(
                Box::new(ast_read_int()),
                BinaryOperator::Equals,
                Box::new(ast_const_int(1)),
            ),
            vec![Statement::Expr(ast_print_int(ast_const_int(10)))],
            vec![Statement::Expr(ast_print_int(ast_const_int(20)))],
        )]),
        inputs: VecDeque::from([1, 2]),
        expected_outputs: VecDeque::from([10, 20]),
    });
}

#[test]
fn test_ast_to_ir_ternary() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![Statement::Expr(Expr::Ternary(
            Box::new(Expr::BinaryOp(
                Box::new(ast_read_int()),
                BinaryOperator::Equals,
                Box::new(ast_const_int(1)),
            )),
            Box::new(ast_print_int(ast_const_int(10))),
            Box::new(ast_print_int(ast_const_int(20))),
        )),
        Statement::Expr(Expr::Ternary(
            Box::new(Expr::BinaryOp(
                Box::new(ast_read_int()),
                BinaryOperator::Equals,
                Box::new(ast_const_int(1)),
            )),
            Box::new(ast_print_int(ast_const_int(10))),
            Box::new(ast_print_int(ast_const_int(20))),
        )),]),
        inputs: VecDeque::from([1, 2]),
        expected_outputs: VecDeque::from([10, 20]),
    });
}

#[test]
fn test_ast_to_ir_ternary_complex() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![Statement::Expr(Expr::Ternary(
            Box::new(Expr::BinaryOp(
                Box::new(Expr::BinaryOp(Box::new(ast_read_int()), BinaryOperator::Equals, Box::new(ast_const_int(1)))),
                BinaryOperator::And,
                Box::new(Expr::BinaryOp(Box::new(ast_read_int()), BinaryOperator::Equals, Box::new(ast_const_int(2)))),
            )),
            Box::new(ast_print_int(Expr::BinaryOp(Box::new(ast_read_int()), BinaryOperator::Add, Box::new(ast_const_int(10))))),
            Box::new(ast_print_int(Expr::BinaryOp(Box::new(ast_read_int()), BinaryOperator::Add, Box::new(ast_read_int())))),
        )),
        Statement::Expr(Expr::Ternary(
            Box::new(Expr::BinaryOp(
                Box::new(Expr::BinaryOp(Box::new(ast_read_int()), BinaryOperator::Equals, Box::new(ast_const_int(1)))),
                BinaryOperator::And,
                Box::new(Expr::BinaryOp(Box::new(ast_read_int()), BinaryOperator::Equals, Box::new(ast_const_int(2)))),
            )),
            Box::new(ast_print_int(Expr::BinaryOp(Box::new(ast_read_int()), BinaryOperator::Add, Box::new(ast_const_int(10))))),
            Box::new(ast_print_int(Expr::BinaryOp(Box::new(ast_read_int()), BinaryOperator::Add, Box::new(ast_read_int())))),
        )),]),
        inputs: VecDeque::from([1, 2, 2, 8, 10, 15]),
        expected_outputs: VecDeque::from([12, 25]),
    });
}

#[test]
fn test_ast_to_ir_while_loop_simple() {
    // x = 5
    // while x > 0 {
    //     print_int(x)
    //     x = x - 1
    // }
    execute_test_case(TestCase {
        ast: Module::Body(vec![
            Statement::Assign(
                Identifier::Named(Arc::from("x")),
                Expr::Constant(Value::I64(5)),
            ),
            Statement::WhileLoop(
                Expr::BinaryOp(
                    Box::new(Expr::Id(Identifier::Named(Arc::from("x")))),
                    BinaryOperator::Greater,
                    Box::new(Expr::Constant(Value::I64(0))),
                ),
                vec![
                    Statement::Expr(Expr::Call(
                        Identifier::Named(Arc::from("print_int")),
                        vec![Expr::Id(Identifier::Named(Arc::from("x")))],
                    )),
                    Statement::Assign(
                        Identifier::Named(Arc::from("x")),
                        Expr::BinaryOp(
                            Box::new(Expr::Id(Identifier::Named(Arc::from("x")))),
                            BinaryOperator::Subtract,
                            Box::new(Expr::Constant(Value::I64(1))),
                        ),
                    ),
                ],
            ),
        ]),
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![5, 4, 3, 2, 1]),
    });
}

#[test]
fn test_ast_to_ir_while_loop_zero_iterations() {
    // x = 0
    // while x > 0 {
    //     print_int(x)
    // }
    // print_int(42)
    execute_test_case(TestCase {
        ast: Module::Body(vec![
            Statement::Assign(
                Identifier::Named(Arc::from("x")),
                Expr::Constant(Value::I64(0)),
            ),
            Statement::WhileLoop(
                Expr::BinaryOp(
                    Box::new(Expr::Id(Identifier::Named(Arc::from("x")))),
                    BinaryOperator::Greater,
                    Box::new(Expr::Constant(Value::I64(0))),
                ),
                vec![Statement::Expr(Expr::Call(
                    Identifier::Named(Arc::from("print_int")),
                    vec![Expr::Id(Identifier::Named(Arc::from("x")))],
                ))],
            ),
            Statement::Expr(Expr::Call(
                Identifier::Named(Arc::from("print_int")),
                vec![Expr::Constant(Value::I64(42))],
            )),
        ]),
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![42]),
    });
}

#[test]
fn test_ast_to_ir_while_loop_accumulator() {
    // sum = 0
    // i = 1
    // while i <= 5 {
    //     sum = sum + i
    //     i = i + 1
    // }
    // print_int(sum)
    execute_test_case(TestCase {
        ast: Module::Body(vec![
            Statement::Assign(
                Identifier::Named(Arc::from("sum")),
                Expr::Constant(Value::I64(0)),
            ),
            Statement::Assign(
                Identifier::Named(Arc::from("i")),
                Expr::Constant(Value::I64(1)),
            ),
            Statement::WhileLoop(
                Expr::BinaryOp(
                    Box::new(Expr::Id(Identifier::Named(Arc::from("i")))),
                    BinaryOperator::LessEquals,
                    Box::new(Expr::Constant(Value::I64(5))),
                ),
                vec![
                    Statement::Assign(
                        Identifier::Named(Arc::from("sum")),
                        Expr::BinaryOp(
                            Box::new(Expr::Id(Identifier::Named(Arc::from("sum")))),
                            BinaryOperator::Add,
                            Box::new(Expr::Id(Identifier::Named(Arc::from("i")))),
                        ),
                    ),
                    Statement::Assign(
                        Identifier::Named(Arc::from("i")),
                        Expr::BinaryOp(
                            Box::new(Expr::Id(Identifier::Named(Arc::from("i")))),
                            BinaryOperator::Add,
                            Box::new(Expr::Constant(Value::I64(1))),
                        ),
                    ),
                ],
            ),
            Statement::Expr(Expr::Call(
                Identifier::Named(Arc::from("print_int")),
                vec![Expr::Id(Identifier::Named(Arc::from("sum")))],
            )),
        ]),
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![15]), // sum of 1..5
    });
}

#[test]
fn test_ast_to_ir_while_loop_nested() {
    // i = 0
    // while i < 2 {
    //     j = 0
    //     while j < 2 {
    //         print_int(i)
    //         print_int(j)
    //         j = j + 1
    //     }
    //     i = i + 1
    // }
    execute_test_case(TestCase {
        ast: Module::Body(vec![
            Statement::Assign(
                Identifier::Named(Arc::from("i")),
                Expr::Constant(Value::I64(0)),
            ),
            Statement::WhileLoop(
                Expr::BinaryOp(
                    Box::new(Expr::Id(Identifier::Named(Arc::from("i")))),
                    BinaryOperator::Less,
                    Box::new(Expr::Constant(Value::I64(2))),
                ),
                vec![
                    Statement::Assign(
                        Identifier::Named(Arc::from("j")),
                        Expr::Constant(Value::I64(0)),
                    ),
                    Statement::WhileLoop(
                        Expr::BinaryOp(
                            Box::new(Expr::Id(Identifier::Named(Arc::from("j")))),
                            BinaryOperator::Less,
                            Box::new(Expr::Constant(Value::I64(2))),
                        ),
                        vec![
                            Statement::Expr(Expr::Call(
                                Identifier::Named(Arc::from("print_int")),
                                vec![Expr::Id(Identifier::Named(Arc::from("i")))],
                            )),
                            Statement::Expr(Expr::Call(
                                Identifier::Named(Arc::from("print_int")),
                                vec![Expr::Id(Identifier::Named(Arc::from("j")))],
                            )),
                            Statement::Assign(
                                Identifier::Named(Arc::from("j")),
                                Expr::BinaryOp(
                                    Box::new(Expr::Id(Identifier::Named(Arc::from("j")))),
                                    BinaryOperator::Add,
                                    Box::new(Expr::Constant(Value::I64(1))),
                                ),
                            ),
                        ],
                    ),
                    Statement::Assign(
                        Identifier::Named(Arc::from("i")),
                        Expr::BinaryOp(
                            Box::new(Expr::Id(Identifier::Named(Arc::from("i")))),
                            BinaryOperator::Add,
                            Box::new(Expr::Constant(Value::I64(1))),
                        ),
                    ),
                ],
            ),
        ]),
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![0, 0, 0, 1, 1, 0, 1, 1]),
    });
}

#[test]
fn test_ast_to_ir_while_loop_with_conditional() {
    // i = 0
    // while i < 5 {
    //     if i == 2 {
    //         print_int(100)
    //     } else {
    //         print_int(i)
    //     }
    //     i = i + 1
    // }
    execute_test_case(TestCase {
        ast: Module::Body(vec![
            Statement::Assign(
                Identifier::Named(Arc::from("i")),
                Expr::Constant(Value::I64(0)),
            ),
            Statement::WhileLoop(
                Expr::BinaryOp(
                    Box::new(Expr::Id(Identifier::Named(Arc::from("i")))),
                    BinaryOperator::Less,
                    Box::new(Expr::Constant(Value::I64(5))),
                ),
                vec![
                    Statement::Conditional(
                        Expr::BinaryOp(
                            Box::new(Expr::Id(Identifier::Named(Arc::from("i")))),
                            BinaryOperator::Equals,
                            Box::new(Expr::Constant(Value::I64(2))),
                        ),
                        vec![Statement::Expr(Expr::Call(
                            Identifier::Named(Arc::from("print_int")),
                            vec![Expr::Constant(Value::I64(100))],
                        ))],
                        vec![Statement::Expr(Expr::Call(
                            Identifier::Named(Arc::from("print_int")),
                            vec![Expr::Id(Identifier::Named(Arc::from("i")))],
                        ))],
                    ),
                    Statement::Assign(
                        Identifier::Named(Arc::from("i")),
                        Expr::BinaryOp(
                            Box::new(Expr::Id(Identifier::Named(Arc::from("i")))),
                            BinaryOperator::Add,
                            Box::new(Expr::Constant(Value::I64(1))),
                        ),
                    ),
                ],
            ),
        ]),
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![0, 1, 100, 3, 4]),
    });
}
