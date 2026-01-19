use crate::infra::ValueEnv;
use cs4999_compiler::ast::*;
use std::collections::VecDeque;

fn interpret_expr(
    e: &Expr,
    inputs: &mut VecDeque<i64>,
    outputs: &mut VecDeque<i64>,
    env: &mut ValueEnv,
) -> Option<i64> {
    use Expr::*;

    match e {
        BinaryOp(left, op, right) => {
            let l_val = interpret_expr(&*left, inputs, outputs, env)
                .expect(format!("{:?} didn't evaluate to a constant", *left).as_str());
            let r_val = interpret_expr(&*right, inputs, outputs, env)
                .expect(format!("{:?} didn't evaluate to a constant", *right).as_str());
            match op {
                BinaryOperator::Add => Some(l_val + r_val),
                BinaryOperator::Subtract => Some(l_val - r_val),
                BinaryOperator::Multiply => Some(l_val * r_val),
                BinaryOperator::Divide => Some(l_val / r_val),
            }
        }
        UnaryOp(UnaryOperator::Minus, v) => Some(
            -(interpret_expr(&*v, inputs, outputs, env)
                .expect(format!("{v:?} didn't evalute to a constant").as_str())),
        ),
        UnaryOp(UnaryOperator::Plus, v) => Some(
            interpret_expr(&*v, inputs, outputs, env)
                .expect(format!("{v:?} didn't evalute to a constant").as_str()),
        ),
        Constant(Value::I64(v)) => Some(*v),
        Call(name, args) => {
            if name == &Identifier::Named(String::from("input_int")) && args.is_empty() {
                Some(inputs.pop_front().expect("Ran out of inputs"))
            } else if name == &Identifier::Named(String::from("print")) && args.len() == 1 {
                let val = interpret_expr(&args[0], inputs, outputs, env)
                    .expect(format!("{:?} didn't evaluate to a constant", args[0]).as_str());
                outputs.push_back(val);
                // TODO: Should actually return
                // Expr::Constant(Value::None) but for now, Value is
                // always an I64.
                None
            } else {
                unimplemented!("Invalid call statement")
            }
        }
        Id(id) => {
            let Value::I64(ret) = env
                .get(id)
                .expect(format!("Unknown variable name: {id:?}").as_str());
            Some(*ret)
        }
    }
}

fn interpret_statement(
    s: &Statement,
    inputs: &mut VecDeque<i64>,
    outputs: &mut VecDeque<i64>,
    remaining_stmts: &[Statement],
    env: &mut ValueEnv,
) {
    match s {
        Statement::Expr(e) => {
            interpret_expr(e, inputs, outputs, env);
            if !remaining_stmts.is_empty() {
                interpret_statement(
                    &remaining_stmts[0],
                    inputs,
                    outputs,
                    &remaining_stmts[1..],
                    env,
                )
            }
        }
        Statement::Assign(id, e) => {
            let result = interpret_expr(e, inputs, outputs, env);
            env.insert(id.clone(), Value::I64(result.expect(format!("Expr `{e:?} did not return a value").as_str())));

            if !remaining_stmts.is_empty() {
                interpret_statement(
                    &remaining_stmts[0],
                    inputs,
                    outputs,
                    &remaining_stmts[1..],
                    env,
                )
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
