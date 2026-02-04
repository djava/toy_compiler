use crate::{ValueEnv, interpreter_utils::*};
use compiler::syntax_trees::{shared::*, ast::*};
use std::collections::VecDeque;

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
        Constant(v) => Some(v.clone()),
        Call(name, args) => {
            if name == &Identifier::from("read_int") && args.is_empty() {
                Some(Value::I64(inputs.pop_front().expect("Ran out of inputs")))
            } else if name == &Identifier::from("print_int") && args.len() == 1 {
                let val = interpret_expr(&args[0], inputs, outputs, env).expect_int();
                outputs.push_back(val);

                Some(Value::None)
            } else {
                unimplemented!("Invalid call statement")
            }
        }
        Id(id) => {
            let val = env
                .get(&AssignDest::Id(id.clone()))
                .expect(format!("Unknown variable name: {id:?}").as_str())
                .clone();
            Some(val)
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
                interpret_statement(
                    statements.first().unwrap(),
                    inputs,
                    outputs,
                    &statements[1..],
                    env,
                );
            }

            interpret_expr(expr, inputs, outputs, env)
        }
        Tuple(_exprs) => todo!(),
        Subscript(_expr, _value) => todo!(),
        Allocate(_, _value_type) => todo!(),
        GlobalSymbol(_) => todo!(),
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
            let result = interpret_expr(e, inputs, outputs, env).coerce_int();
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
        }
        Statement::Conditional(cond, pos, neg) => {
            let result = interpret_expr(cond, inputs, outputs, env).coerce_bool();

            let exec_next = if result { pos } else { neg };
            if !exec_next.is_empty() {
                interpret_statement(&exec_next[0], inputs, outputs, &exec_next[1..], env);
            }

            if !remaining_stmts.is_empty() {
                interpret_statement(
                    &remaining_stmts[0],
                    inputs,
                    outputs,
                    &remaining_stmts[1..],
                    env,
                );
            }
        }
        Statement::WhileLoop(cond, body) => {
            let mut iterations = 0;
            while interpret_expr(cond, inputs, outputs, env).expect_bool() {
                if !body.is_empty() {
                    interpret_statement(&body[0], inputs, outputs, &body[1..], env);
                }
                iterations += 1;
                if iterations > 1000 {
                    panic!("infinite loop? iterated 1000 times");
                }
            }

            if !remaining_stmts.is_empty() {
                interpret_statement(
                    &remaining_stmts[0],
                    inputs,
                    outputs,
                    &remaining_stmts[1..],
                    env,
                );
            }
        }
    };
}

pub fn interpret(m: &Module, inputs: &mut VecDeque<i64>, outputs: &mut VecDeque<i64>) {
    if !m.body.is_empty() {
        let mut env = ValueEnv::new();
        interpret_statement(&m.body[0], inputs, outputs, &m.body[1..], &mut env);
    }
}
