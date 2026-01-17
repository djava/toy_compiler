use crate::ast::*;

pub fn partial_eval(s: &mut Statement) {
    match s {
        Statement::Assign(_dest, e) => partial_eval_expr(e),
        Statement::Expr(e) => partial_eval_expr(e),
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
            if let Constant(Value::I64(l_val)) = **left
                && let Constant(Value::I64(r_val)) = **right
            {
                match op {
                    BinaryOperator::Add => {
                        *e = Constant(Value::I64(l_val + r_val));
                    }
                    BinaryOperator::Subtract => {
                        *e = Constant(Value::I64(l_val - r_val));
                    }
                    BinaryOperator::Multiply => {
                        *e = Constant(Value::I64(l_val * r_val));
                    }
                    BinaryOperator::Divide => {
                        *e = Constant(Value::I64(l_val / r_val));
                    }
                }
            }
        },
        UnaryOp(op, expr) => {
            // Try and evaluate the operand recursively, then if its
            // constant we can evaluate the whole expression
            partial_eval_expr(&mut *expr);
            if let Constant(Value::I64(val)) = **expr {
                match op {
                    UnaryOperator::Minus => { *e = Constant(Value::I64(-val)) },
                    UnaryOperator::Plus => { *e = Constant(Value::I64(val)) },
                }
            }
        },
        Call(_name, args) => {
            // Can't evaluate through function calls right now, but try
            // to evaluate the arguments regardless
            for i in args {
                partial_eval_expr(i);
            }
        },
        Constant(_val) => {
            // Already a constant, nothing to evaluate
        },
    }
}
