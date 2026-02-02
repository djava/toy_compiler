mod infra;
use std::collections::VecDeque;

use cs4999_compiler::{
    ast::*,
    passes::{ASTPass, ShortCircuiting, TypeCheck},
};

use crate::infra::{ast_interpreter::interpret};

struct TestCase {
    ast: Module,
    inputs: VecDeque<i64>,
    expected_outputs: VecDeque<i64>,
}

fn assert_expr_no_and_or(e: &Expr) {
    match e {
        Expr::BinaryOp(_, BinaryOperator::And, _) | Expr::BinaryOp(_, BinaryOperator::Or, _) => {
            panic!("short circuiting should have removed and And/Or operators");
        }
        Expr::BinaryOp(left, _, right) => {
            assert_expr_no_and_or(&*left);
            assert_expr_no_and_or(&*right);
        }
        Expr::UnaryOp(_, expr) => {
            assert_expr_no_and_or(&*expr);
        }
        Expr::Call(_, exprs) => {
            exprs.iter().for_each(assert_expr_no_and_or);
        }
        Expr::Ternary(cond, pos, neg) => {
            assert_expr_no_and_or(cond);
            assert_expr_no_and_or(pos);
            assert_expr_no_and_or(neg);
        }
        _ => {}
    }
}

fn assert_statement_no_and_or(s: &Statement) {
    match s {
        Statement::Assign(_, expr) => assert_expr_no_and_or(&expr),
        Statement::Expr(expr) => assert_expr_no_and_or(&expr),
        Statement::Conditional(expr, pos, neg) => {
            assert_expr_no_and_or(&expr);
            pos.iter().for_each(assert_statement_no_and_or);
            neg.iter().for_each(assert_statement_no_and_or);
        },
        Statement::WhileLoop(expr, body) => {
            assert_expr_no_and_or(&expr);
            body.iter().for_each(assert_statement_no_and_or);
        }
    }
}

fn execute_test_case(mut tc: TestCase) {
    println!("\n==================");

    tc.ast = TypeCheck.run_pass(tc.ast);
    println!("Type-check passed on source");

    println!("AST before Short Circuiting: {:?}", tc.ast);
    let post_run_ast = ShortCircuiting.run_pass(tc.ast);
    println!("AST after Short Circuiting: {:?}", post_run_ast);

    let type_checked = TypeCheck.run_pass(post_run_ast);
    println!("Type-check passed after pass");

    let Module::Body(statements) = &type_checked;
    statements.iter().for_each(assert_statement_no_and_or);

    let mut outputs = VecDeque::<i64>::new();
    interpret(&type_checked, &mut tc.inputs, &mut outputs);

    assert!(tc.inputs.is_empty());
    assert_eq!(outputs, tc.expected_outputs);
}

#[test]
fn test_short_circuiting_simple() {
    let tc = TestCase {
        ast: Module::Body(vec![Statement::Expr(Expr::BinaryOp(
            Box::new(Expr::Constant(Value::Bool(true))),
            BinaryOperator::And,
            Box::new(Expr::Constant(Value::Bool(false))),
        ))]),
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::new(),
    };

    execute_test_case(tc);
}

#[test]
fn test_short_circuiting_nested() {
    let tc = TestCase {
        ast: Module::Body(vec![Statement::Expr(Expr::BinaryOp(
            Box::new(Expr::Constant(Value::Bool(true))),
            BinaryOperator::And,
            Box::new(Expr::BinaryOp(
                Box::new(Expr::Constant(Value::Bool(true))),
                BinaryOperator::And,
                Box::new(Expr::BinaryOp(
                    Box::new(Expr::Constant(Value::Bool(true))),
                    BinaryOperator::And,
                    Box::new(Expr::BinaryOp(
                        Box::new(Expr::Constant(Value::Bool(true))),
                        BinaryOperator::And,
                        Box::new(Expr::BinaryOp(
                            Box::new(Expr::Constant(Value::Bool(true))),
                            BinaryOperator::And,
                            Box::new(Expr::Constant(Value::Bool(false))),
                        )),
                    )),
                )),
            )),
        ))]),
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::new(),
    };

    execute_test_case(tc);
}

#[test]
fn test_short_circuiting_comparisons_and() {
    let tc = TestCase {
        ast: Module::Body(vec![
            Statement::Expr(Expr::BinaryOp(
                Box::new(Expr::Constant(Value::Bool(true))),
                BinaryOperator::And,
                Box::new(Expr::BinaryOp(
                    Box::new(Expr::Constant(Value::I64(1))),
                    BinaryOperator::Equals,
                    Box::new(Expr::Call(Identifier::from("read_int"), vec![])),
                )),
            )),
            Statement::Expr(Expr::BinaryOp(
                Box::new(Expr::Constant(Value::Bool(false))),
                BinaryOperator::And,
                Box::new(Expr::BinaryOp(
                    Box::new(Expr::Constant(Value::I64(1))),
                    BinaryOperator::Equals,
                    Box::new(Expr::Call(Identifier::from("read_int"), vec![])),
                )),
            )),
        ]),
        inputs: VecDeque::from([1]),
        expected_outputs: VecDeque::new(),
    };

    execute_test_case(tc);
}

#[test]
fn test_short_circuiting_comparisons_or() {
    let tc = TestCase {
        ast: Module::Body(vec![
            Statement::Expr(Expr::BinaryOp(
                Box::new(Expr::Constant(Value::Bool(true))),
                BinaryOperator::Or,
                Box::new(Expr::BinaryOp(
                    Box::new(Expr::Constant(Value::I64(1))),
                    BinaryOperator::Equals,
                    Box::new(Expr::Call(Identifier::from("read_int"), vec![])),
                )),
            )),
            Statement::Expr(Expr::BinaryOp(
                Box::new(Expr::Constant(Value::Bool(false))),
                BinaryOperator::Or,
                Box::new(Expr::BinaryOp(
                    Box::new(Expr::Constant(Value::I64(1))),
                    BinaryOperator::Equals,
                    Box::new(Expr::Call(Identifier::from("read_int"), vec![])),
                )),
            )),
        ]),
        inputs: VecDeque::from([1]),
        expected_outputs: VecDeque::new(),
    };

    execute_test_case(tc);
}
