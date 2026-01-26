use crate::{ast::*, passes::ASTPass};

pub struct PartialEval;

impl ASTPass for PartialEval {
    fn run_pass(self, m: Module) -> Module {
        let Module::Body(mut statements) = m;

        for s in statements.iter_mut() {
            match s {
                Statement::Assign(_dest, e) => partial_eval_expr(e),
                Statement::Expr(e) => partial_eval_expr(e),
                Statement::Conditional(_cond, _pos, _neg) => todo!(),
            }
        }

        Module::Body(statements)
    }
}

fn partial_eval_expr(e: &mut Expr) {
    use Expr::*;

    match e {
        BinaryOp(left, op, right) => {
            // Try and evaluate both operands recursively, then if
            // they're both constants we can evaluate the whole
            // expression
            partial_eval_expr(&mut *left);
            partial_eval_expr(&mut *right);

            if let Constant(l_val) = **left
                && let Constant(r_val) = **right
            {
                *e = Constant(op.eval(&l_val, &r_val))
            }
        }
        UnaryOp(op, expr) => {
            // Try and evaluate the operand recursively, then if its
            // constant we can evaluate the whole expression
            partial_eval_expr(&mut *expr);
            if let Constant(val) = **expr {
                *e = Constant(op.eval(&val));
            }
        }
        Call(_name, args) => {
            // Can't evaluate through function calls right now, but try
            // to evaluate the arguments regardless
            for i in args {
                partial_eval_expr(i);
            }
        }
        Constant(_val) => {
            // Already a constant, nothing to evaluate
        }
        Id(_name) => {
            // Can't do anything
        }
        Ternary(_cond, _pos, _neg) => todo!(),
    }
}

trait BinaryOperatorExt {
    fn eval(&self, l: &Value, r: &Value) -> Value;
}

impl BinaryOperatorExt for BinaryOperator {
    fn eval(&self, l: &Value, r: &Value) -> Value {
        use ValueType::*;

        match (ValueType::from(l), ValueType::from(r)) {
            (IntType, IntType) => {
                if let Value::I64(l_val) = l
                    && let Value::I64(r_val) = r
                {
                    match self {
                        BinaryOperator::Add => Value::I64(l_val + r_val),
                        BinaryOperator::Subtract => Value::I64(l_val - r_val),
                        BinaryOperator::Equals => Value::Bool(l_val == r_val),
                        BinaryOperator::NotEquals => Value::Bool(l_val != r_val),
                        BinaryOperator::Greater => Value::Bool(l_val > r_val),
                        BinaryOperator::GreaterEquals => Value::Bool(l_val >= r_val),
                        BinaryOperator::Less => Value::Bool(l_val < r_val),
                        BinaryOperator::LessEquals => Value::Bool(l_val <= r_val),
                        _ => panic!(
                            "Unsupported operand types (int, int) to BinaryOperator {self:?}"
                        ),
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
                        BinaryOperator::And => Value::Bool(*l_val && *r_val),
                        BinaryOperator::Or => Value::Bool(*l_val || *r_val),
                        BinaryOperator::Equals => Value::Bool(*l_val == *r_val),
                        BinaryOperator::NotEquals => Value::Bool(*l_val != *r_val),
                        _ => panic!(
                            "Unsupported operand types (bool, bool) to BinaryOperator {self:?}"
                        ),
                    }
                } else {
                    panic!("ValueType didnt match Value variant - bug in ValueType::from()?");
                }
            }
            _ => panic!(
                "Unsupported operand type ({:?}, {:?}) to UnaryOperator {self:?}",
                ValueType::from(l),
                ValueType::from(r)
            ),
        }
    }
}
trait UnaryOperatorExt {
    fn eval(&self, v: &Value) -> Value;
}

impl UnaryOperatorExt for UnaryOperator {
    fn eval(&self, v: &Value) -> Value {
        use ValueType::*;

        match ValueType::from(v) {
            IntType => {
                if let Value::I64(val) = v {
                    match self {
                        UnaryOperator::Minus => Value::I64(-val),
                        UnaryOperator::Plus => Value::I64(*val),
                        _ => panic!("Unsupported operand type (int) to UnaryOperator {self:?}"),
                    }
                } else {
                    panic!("ValueType didnt match Value variant - bug in ValueType::from()?");
                }
            }
            BoolType => {
                if let Value::Bool(val) = v {
                    match self {
                        UnaryOperator::Not => Value::Bool(!val),
                        _ => panic!("Unsupported operand type bool to UnaryOperator {self:?}"),
                    }
                } else {
                    panic!("ValueType didnt match Value variant - bug in ValueType::from()?");
                }
            }
            _ => panic!(
                "Unsupported operand type ({:?}) to UnaryOperator {self:?}",
                ValueType::from(v)
            ),
        }
    }
}
