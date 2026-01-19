mod infra;
use std::collections::VecDeque;

use cs4999_compiler::{
    ast::*,
    passes::{partial_eval::PartialEval, IRPass},
};

use crate::infra::{interpreter::interpret, type_check::type_check};

struct TestCase {
    ast: Module,
    inputs: VecDeque<i64>,
    expected_outputs: VecDeque<i64>,
}

fn execute_test_case(mut tc: TestCase) {
    println!("\n==================");

    type_check(&tc.ast);
    println!("Type-check passed on source");

    println!("AST before Partial Eval: {:?}", tc.ast);
    let post_run_ast = PartialEval::run_pass(tc.ast);
    println!("AST after Partial Eval: {:?}", post_run_ast);

    type_check(&post_run_ast);
    println!("Type-check passed after pass");

    let mut outputs = VecDeque::<i64>::new();
    interpret(&post_run_ast, &mut tc.inputs, &mut outputs);

    assert_eq!(outputs, tc.expected_outputs);
}

#[test]
fn test_partial_eval_add() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![Statement::Expr(Expr::Call(
            Identifier::Named(String::from("print")),
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
fn test_partial_eval_input() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![Statement::Expr(Expr::Call(
            Identifier::Named(String::from("print")),
            vec![Expr::Call(Identifier::Named(String::from("input_int")), vec![])],
        ))]),
        inputs: VecDeque::from(vec![42]),
        expected_outputs: VecDeque::from(vec![42]),
    })
}

#[test]
fn test_partial_eval_subinput() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![Statement::Expr(Expr::Call(
            Identifier::Named(String::from("print")),
            vec![Expr::BinaryOp(
                Box::new(Expr::Call(Identifier::Named(String::from("input_int")), vec![])),
                BinaryOperator::Subtract,
                Box::new(Expr::Call(Identifier::Named(String::from("input_int")), vec![])),
            )],
        ))]),
        inputs: VecDeque::from(vec![5, 3]),
        expected_outputs: VecDeque::from(vec![2]),
    });
}

#[test]
fn test_partial_eval_zero() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![Statement::Expr(Expr::Call(
            Identifier::Named(String::from("print")),
            vec![Expr::Constant(Value::I64(0))],
        ))]),
        inputs: VecDeque::from(vec![]),
        expected_outputs: VecDeque::from(vec![0]),
    });
}

#[test]
fn test_partial_eval_nested() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![Statement::Expr(Expr::Call(
            Identifier::Named(String::from("print")),
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
fn test_partial_eval_mixed() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![Statement::Expr(Expr::Call(
            Identifier::Named(String::from("print")),
            vec![Expr::BinaryOp(
                Box::new(Expr::BinaryOp(
                    Box::new(Expr::Call(Identifier::Named(String::from("input_int")), vec![])),
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
