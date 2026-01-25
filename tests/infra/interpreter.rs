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

trait BinaryOperatorExt {
    fn try_eval(&self, l: &Value, r: &Value) -> Option<Value>;
}

impl BinaryOperatorExt for BinaryOperator {
    fn try_eval(&self, l: &Value, r: &Value) -> Option<Value> {
        use ValueType::*;

        match (ValueType::from(l), ValueType::from(r)) {
            (IntType, IntType) => {
                if let Value::I64(l_val) = l
                    && let Value::I64(r_val) = r
                {
                    match self {
                        BinaryOperator::Add => Some(Value::I64(l_val + r_val)),
                        BinaryOperator::Subtract => Some(Value::I64(l_val - r_val)),
                        BinaryOperator::Equals => Some(Value::Bool(l_val == r_val)),
                        BinaryOperator::NotEquals => Some(Value::Bool(l_val != r_val)),
                        BinaryOperator::Greater => Some(Value::Bool(l_val > r_val)),
                        BinaryOperator::GreaterEquals => Some(Value::Bool(l_val >= r_val)),
                        BinaryOperator::LessThan => Some(Value::Bool(l_val < r_val)),
                        BinaryOperator::LessThanEquals => Some(Value::Bool(l_val <= r_val)),
                        _ => None,
                    }
                } else {
                    panic!("ValueType didnt match Value variant - bug in ValueType::from()?");
                }
            }
            (BoolType, BoolType) => {
                if let Value::Bool(l_val) = l
                    && let Value::Bool(r_val) = r
                {
                    match self {
                        BinaryOperator::And => Some(Value::Bool(*l_val && *r_val)),
                        BinaryOperator::Or => Some(Value::Bool(*l_val || *r_val)),
                        BinaryOperator::Equals => Some(Value::Bool(*l_val == *r_val)),
                        BinaryOperator::NotEquals => Some(Value::Bool(*l_val != *r_val)),
                        _ => None,
                    }
                } else {
                    panic!("ValueType didnt match Value variant - bug in ValueType::from()?");
                }
            }
            _ => None,
        }
    }
}
trait UnaryOperatorExt {
    fn try_eval(&self, v: &Value) -> Option<Value>;
}

impl UnaryOperatorExt for UnaryOperator {
    fn try_eval(&self, v: &Value) -> Option<Value> {
        use ValueType::*;

        match ValueType::from(v) {
            IntType => {
                if let Value::I64(val) = v {
                    match self {
                        UnaryOperator::Minus => Some(Value::I64(-val)),
                        UnaryOperator::Plus => Some(Value::I64(*val)),
                        _ => None,
                    }
                } else {
                    panic!("ValueType didnt match Value variant - bug in ValueType::from()?");
                }
            }
            BoolType => {
                if let Value::Bool(val) = v {
                    match self {
                        UnaryOperator::Not => Some(Value::Bool(!val)),
                        _ => None,
                    }
                } else {
                    panic!("ValueType didnt match Value variant - bug in ValueType::from()?");
                }
            }
            _ => None,
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
