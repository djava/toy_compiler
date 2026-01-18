use std::collections::{VecDeque};
use crate::common::ValueEnv;
use cs4999_compiler::ast::*;


fn interpret_expr(e: &Expr, inputs: &mut VecDeque<i64>, env: &mut ValueEnv) -> Option<i64> {
    use Expr::*;

    match e {
        BinaryOp(left, op, right) => {
            let l_val = interpret_expr(&*left, inputs, env)
                .expect(format!("{:?} didn't evaluate to a constant", *left).as_str());
            let r_val = interpret_expr(&*right, inputs, env)
                .expect(format!("{:?} didn't evaluate to a constant", *right).as_str());
            match op {
                BinaryOperator::Add => Some(l_val + r_val),
                BinaryOperator::Subtract => Some(l_val - r_val),
                BinaryOperator::Multiply => Some(l_val * r_val),
                BinaryOperator::Divide => Some(l_val / r_val),
            }
        },
        UnaryOp(UnaryOperator::Minus, v) => {
            Some(-(interpret_expr(&*v, inputs, env).expect(format!("{v:?} didn't evalute to a constant").as_str())))
        },
        UnaryOp(UnaryOperator::Plus, v) => {
            Some(interpret_expr(&*v, inputs, env).expect(format!("{v:?} didn't evalute to a constant").as_str()))
        },
        Constant(Value::I64(v)) => Some(*v),
        Call(name, args) => {
            if name == "input_int" && args.is_empty() {
                Some(inputs.pop_front().expect("Ran out of inputs"))
            } else {
                None
            }
        },
        Id(id) => {
            let Value::I64(ret) = env.get(id).expect(format!("Unknown variable name: {id:?}").as_str());
            Some(*ret)
        }
    }
}

fn interpret_statement(s: &Statement, inputs: &mut VecDeque<i64>, outputs: &mut VecDeque<i64>, remaining_stmts: &[Statement], env: &mut ValueEnv) {
    match s {
        Statement::Expr(Expr::Call(name, args)) => {
            if name == "print" && args.len() == 1 {
                let val = interpret_expr(&args[0], inputs, env).expect(format!("{:?} didn't evaluate to a constant", args[0]).as_str());
                outputs.push_back(val);
            } else {
                unimplemented!("Invalid call statement")
            }
            if !remaining_stmts.is_empty() {
                interpret_statement(&remaining_stmts[0], inputs, outputs, &remaining_stmts[1..], env)
            }
        },
        Statement::Expr(e) => {
            interpret_expr(e, inputs, env);
            if !remaining_stmts.is_empty() {
                interpret_statement(&remaining_stmts[0], inputs, outputs, &remaining_stmts[1..], env)
            }
        },
        _ => {
            if !remaining_stmts.is_empty() {
                interpret_statement(&remaining_stmts[0], inputs, outputs, &remaining_stmts[1..], env)
            }
        }
    };
}

pub fn interpret(m: &Module, inputs: &mut VecDeque<i64>, outputs: &mut VecDeque<i64>) {
    let Module::Body(statements) = m;
    if !statements.is_empty() {
        let mut env = ValueEnv::new();
        interpret_statement(&statements[0], inputs, outputs, &statements[1..], &mut env);
    }
}