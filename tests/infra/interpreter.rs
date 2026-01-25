use crate::infra::ValueEnv;
use cs4999_compiler::ast::*;
use std::collections::VecDeque;
use std::sync::Arc;

trait OptionValueExt {
    fn expect_int(self) -> i64;
}

impl OptionValueExt for Option<Value> {
    fn expect_int(self) -> i64 {
        if let Some(Value::I64(v)) = self {
            v
        } else {
            panic!("{:?} didn't evaluate to a constant int", self);
        }
    }
}

fn interpret_expr(
    e: &Expr,
    inputs: &mut VecDeque<i64>,
    outputs: &mut VecDeque<i64>,
    env: &mut ValueEnv,
) -> Option<Value> {
    use Expr::*;

    match e {
        BinaryOp(left, op, right) => {
            let l_val = interpret_expr(&*left, inputs, outputs, env).expect_int();
            let r_val = interpret_expr(&*right, inputs, outputs, env).expect_int();

            match op {
                BinaryOperator::Add => Some(Value::I64(l_val + r_val)),
                BinaryOperator::Subtract => Some(Value::I64(l_val - r_val)),
            }
        }

        UnaryOp(UnaryOperator::Minus, v) => {
            let val = interpret_expr(&*v, inputs, outputs, env).expect_int();
            Some(Value::I64(val))
        }

        UnaryOp(UnaryOperator::Plus, v) => Some(
            interpret_expr(&*v, inputs, outputs, env)
                .expect(format!("{v:?} didn't evalute to a constant").as_str()),
        ),
        Constant(v) => Some((*v).into()),
        Call(name, args) => {
            if name == &Identifier::Named(Arc::from("read_int")) && args.is_empty() {
                Some(Value::I64(inputs.pop_front().expect("Ran out of inputs")))
            } else if name == &Identifier::Named(Arc::from("print_int")) && args.len() == 1 {
                let val = interpret_expr(&args[0], inputs, outputs, env).expect_int();
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
            let val = *env
                .get(id)
                .expect(format!("Unknown variable name: {id:?}").as_str());
            Some(val.into())
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
            let result = interpret_expr(e, inputs, outputs, env).expect_int();
            env.insert(
                id.clone(),
                Value::I64(result),
            );

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
