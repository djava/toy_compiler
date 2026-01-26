use crate::infra::{ValueEnv, interpreter_utils::*};
use cs4999_compiler::ast::*;
use std::collections::VecDeque;
use std::sync::Arc;

fn interpret_expr(
    e: &Expr,
    inputs: &mut VecDeque<i64>,
    outputs: &mut VecDeque<i64>,
    env: &mut ValueEnv,
) -> Option<Value> {
    use Expr::*;

    match e {
        BinaryOp(left, op, right) => {
            if let Some(l_val) = interpret_expr(&*left, inputs, outputs, env)
                && let Some(r_val) = interpret_expr(&*right, inputs, outputs, env)
            {
                op.try_eval(&l_val, &r_val)
            } else {
                None
            }
        }
        UnaryOp(op, expr) => {
            if let Some(v) = interpret_expr(&*expr, inputs, outputs, env) {
                op.try_eval(&v)
            } else {
                None
            }
        }
        Constant(v) => Some((*v).into()),
        Call(name, args) => {
            if name == &Identifier::Named(Arc::from("read_int")) && args.is_empty() {
                Some(Value::I64(inputs.pop_front().expect("Ran out of inputs")))
            } else if name == &Identifier::Named(Arc::from("print_int")) && args.len() == 1 {
                let val = interpret_expr(&args[0], inputs, outputs, env).expect_int();
                outputs.push_back(val);

                Some(Value::None)
            } else {
                unimplemented!("Invalid call statement")
            }
        }
        Id(id) => {
            let val = *env
                .get(id)
                .expect(format!("Unknown variable name: {id:?}").as_str());
            Some(val.into())
        }
        Ternary(cond, pos, neg) => {
            if let Some(Value::Bool(cond_val)) = interpret_expr(&*cond, inputs, outputs, env) {
                if cond_val {
                    interpret_expr(pos, inputs, outputs, env)
                } else {
                    interpret_expr(neg, inputs, outputs, env)
                }
            } else {
                None
            }
        }
        StatementBlock(statements, expr) => {
            if !statements.is_empty() {
                interpret_statement(statements.first().unwrap(), inputs, outputs, &statements[1..], env);
            }

            interpret_expr(expr, inputs, outputs, env)
        },
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
            let result = interpret_expr(e, inputs, outputs, env).expect_int();
            env.insert(id.clone(), Value::I64(result));

            if !remaining_stmts.is_empty() {
                interpret_statement(
                    &remaining_stmts[0],
                    inputs,
                    outputs,
                    &remaining_stmts[1..],
                    env,
                )
            }
        },
        Statement::Conditional(cond, pos, neg) => {
            let result = interpret_expr(cond, inputs, outputs, env).expect_bool();
            
            let exec_next = if result { pos } else { neg };
            for i in exec_next {
                interpret_statement(i, inputs, outputs, remaining_stmts, env);
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
