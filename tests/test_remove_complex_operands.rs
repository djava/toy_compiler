mod infra;
use std::collections::VecDeque;

use cs4999_compiler::{
    ast::*,
    passes::{IRPass, remove_complex_operands::RemoveComplexOperands},
};

use crate::infra::{interpreter::interpret, type_check::type_check};

struct TestCase<'a> {
    ast: Module<'a>,
    inputs: VecDeque<i64>,
    expected_outputs: VecDeque<i64>,
}

fn execute_test_case(mut tc: TestCase) {
    println!("\n==================");

    type_check(&tc.ast);
    println!("Type-check passed on source");

    println!("AST before RCO: {:#?}", tc.ast);
    let post_run_ast = RemoveComplexOperands.run_pass(tc.ast);
    println!("AST after RCO: {:#?}", post_run_ast);

    type_check(&post_run_ast);
    println!("Type-check passed after pass");

    let mut outputs = VecDeque::<i64>::new();
    interpret(&post_run_ast, &mut tc.inputs, &mut outputs);

    assert_eq!(outputs, tc.expected_outputs);
}

#[test]
fn test_remove_complex_operands_add() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![Statement::Expr(Expr::Call(
            Identifier::Named("print"),
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
fn test_remove_complex_operands_input() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![Statement::Expr(Expr::Call(
            Identifier::Named("print"),
            vec![Expr::Call(
                Identifier::Named("input_int"),
                vec![],
            )],
        ))]),
        inputs: VecDeque::from(vec![42]),
        expected_outputs: VecDeque::from(vec![42]),
    })
}

#[test]
fn test_remove_complex_operands_subinput() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![Statement::Expr(Expr::Call(
            Identifier::Named("print"),
            vec![Expr::BinaryOp(
                Box::new(Expr::Call(
                    Identifier::Named("input_int"),
                    vec![],
                )),
                BinaryOperator::Subtract,
                Box::new(Expr::Call(
                    Identifier::Named("input_int"),
                    vec![],
                )),
            )],
        ))]),
        inputs: VecDeque::from(vec![5, 3]),
        expected_outputs: VecDeque::from(vec![2]),
    });
}

#[test]
fn test_remove_complex_operands_zero() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![Statement::Expr(Expr::Call(
            Identifier::Named("print"),
            vec![Expr::Constant(Value::I64(0))],
        ))]),
        inputs: VecDeque::from(vec![]),
        expected_outputs: VecDeque::from(vec![0]),
    });
}

#[test]
fn test_remove_complex_operands_nested() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![Statement::Expr(Expr::Call(
            Identifier::Named("print"),
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
fn test_remove_complex_operands_mixed() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![Statement::Expr(Expr::Call(
            Identifier::Named("print"),
            vec![Expr::BinaryOp(
                Box::new(Expr::BinaryOp(
                    Box::new(Expr::Call(
                        Identifier::Named("input_int"),
                        vec![],
                    )),
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
fn test_remove_complex_operands_simple_assignment() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![
            Statement::Assign(
                Identifier::Named("x"),
                Expr::Constant(Value::I64(1000)),
            ),
            Statement::Expr(Expr::Call(
                Identifier::Named("print"),
                vec![Expr::Id(Identifier::Named("x"))],
            )),
        ]),
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
        ast: Module::Body(vec![
            Statement::Assign(
                Identifier::Named("foofoo"),
                Expr::BinaryOp(
                    Box::new(Expr::BinaryOp(
                        Box::new(Expr::Call(
                            Identifier::Named("input_int"),
                            vec![],
                        )),
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
                Identifier::Named("print"),
                vec![Expr::Id(Identifier::Named("foofoo"))],
            )),
        ]),
        inputs: VecDeque::from(vec![10]),
        expected_outputs: VecDeque::from(vec![10 + 2 + 40 - 2]),
    });
}

#[test]
fn test_remove_complex_operands_complex_args() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![Statement::Expr(Expr::Call(
            Identifier::Named("print"),
            vec![Expr::BinaryOp(
                Box::new(Expr::BinaryOp(
                    Box::new(Expr::Call(
                        Identifier::Named("input_int"),
                        vec![],
                    )),
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
        ast: Module::Body(vec![
            Statement::Assign(
                Identifier::Named("foo"),
                Expr::Call(Identifier::Named("input_int"), vec![]),
            ),
            Statement::Assign(
                Identifier::Named("bar"),
                Expr::BinaryOp(
                    Box::new(Expr::Call(
                        Identifier::Named("input_int"),
                        vec![],
                    )),
                    BinaryOperator::Add,
                    Box::new(Expr::Id(Identifier::Named("foo"))),
                ),
            ),
            Statement::Assign(
                Identifier::Named("baz"),
                Expr::BinaryOp(
                    Box::new(Expr::Call(
                        Identifier::Named("input_int"),
                        vec![],
                    )),
                    BinaryOperator::Add,
                    Box::new(Expr::Id(Identifier::Named("bar"))),
                ),
            ),
            Statement::Assign(
                Identifier::Named("bop"),
                Expr::BinaryOp(
                    Box::new(Expr::BinaryOp(
                        Box::new(Expr::Id(Identifier::Named("foo"))),
                        BinaryOperator::Add,
                        Box::new(Expr::Id(Identifier::Named("bar"))),
                    )),
                    BinaryOperator::Add,
                    Box::new(Expr::Id(Identifier::Named("baz"))),
                ),
            ),
            Statement::Expr(Expr::Call(
                Identifier::Named("print"),
                vec![Expr::BinaryOp(
                    Box::new(Expr::Call(
                        Identifier::Named("input_int"),
                        vec![],
                    )),
                    BinaryOperator::Add,
                    Box::new(Expr::Id(Identifier::Named("bop"))),
                )],
            )),
        ]),
        inputs: VecDeque::from(vec![10, 20, 30, 40]),
        expected_outputs: VecDeque::from(vec![10 + (10 + 20) + (10 + 20 + 30) + 40]),
    });
}
