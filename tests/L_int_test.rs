mod common;
use std::collections::VecDeque;

use cs4999_compiler::{ast::*, partial_eval};

use crate::common::interpreter::interpret;

struct TestCase {
    ast: Statement,
    inputs: VecDeque<i64>,
    expected_outputs: VecDeque<i64>,
}

// let mut test_cases = vec![
//
//     ,
//     T,
// ];

#[test]
fn test_partial_eval_add() {
    let mut tc = TestCase {
        ast: Statement::Expr(Expr::Call(
            String::from("print"),
            vec![Expr::BinaryOp(
                Box::new(Expr::Constant(Value::I64(40))),
                BinaryOperator::Add,
                Box::new(Expr::Constant(Value::I64(2))),
            )],
        )),
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![42]),
    };

    println!("AST before Partial Eval: {:?}", tc.ast);
    partial_eval(&mut tc.ast);
    println!("AST after Partial Eval: {:?}", tc.ast);

    let mut outputs = VecDeque::<i64>::new();
    interpret(&tc.ast, &mut tc.inputs, &mut outputs);

    assert_eq!(outputs, tc.expected_outputs);
}

#[test]
fn test_partial_eval_input() {
    let mut tc = TestCase {
        ast: Statement::Expr(Expr::Call(
            String::from("print"),
            vec![Expr::Call(String::from("input_int"), vec![])],
        )),
        inputs: VecDeque::from(vec![42]),
        expected_outputs: VecDeque::from(vec![42]),
    };

    println!("AST before Partial Eval: {:?}", tc.ast);
    partial_eval(&mut tc.ast);
    println!("AST after Partial Eval: {:?}", tc.ast);

    let mut outputs = VecDeque::<i64>::new();
    interpret(&tc.ast, &mut tc.inputs, &mut outputs);

    assert_eq!(outputs, tc.expected_outputs);
}

#[test]
fn test_partial_eval_subinput() {
    let mut tc = TestCase {
        ast: Statement::Expr(Expr::Call(
            String::from("print"),
            vec![Expr::BinaryOp(
                Box::new(Expr::Call(String::from("input_int"), vec![])),
                BinaryOperator::Subtract,
                Box::new(Expr::Call(String::from("input_int"), vec![])),
            )],
        )),
        inputs: VecDeque::from(vec![5, 3]),
        expected_outputs: VecDeque::from(vec![2]),
    };

    println!("AST before Partial Eval: {:?}", tc.ast);
    partial_eval(&mut tc.ast);
    println!("AST after Partial Eval: {:?}", tc.ast);

    let mut outputs = VecDeque::<i64>::new();
    interpret(&tc.ast, &mut tc.inputs, &mut outputs);

    assert_eq!(outputs, tc.expected_outputs);
}

#[test]
fn test_partial_eval_zero() {
    let mut tc = TestCase {
        ast: Statement::Expr(Expr::Call(
            String::from("print"),
            vec![Expr::Constant(Value::I64(0))],
        )),
        inputs: VecDeque::from(vec![]),
        expected_outputs: VecDeque::from(vec![0]),
    };

    println!("AST before Partial Eval: {:?}", tc.ast);
    partial_eval(&mut tc.ast);
    println!("AST after Partial Eval: {:?}", tc.ast);

    let mut outputs = VecDeque::<i64>::new();
    interpret(&tc.ast, &mut tc.inputs, &mut outputs);

    assert_eq!(outputs, tc.expected_outputs);
}
