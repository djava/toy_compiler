use std::collections::VecDeque;

use cs4999_compiler::ast::*;

fn interpret_expr(e: &Expr, inputs: &mut VecDeque<i64>) -> Option<i64> {
    use Expr::*;

    match e {
        BinaryOp(left, BinaryOperator::Add, right) => {
            let l_val = interpret_expr(&*left, inputs)
                .expect(format!("{:?} didn't evaluate to a constant", *left).as_str());
            let r_val = interpret_expr(&*right, inputs)
                .expect(format!("{:?} didn't evaluate to a constant", *right).as_str());
            Some(l_val + r_val)
        },
        BinaryOp(left, BinaryOperator::Subtract, right) => {
            let l_val = interpret_expr(&*left, inputs)
                .expect(format!("{:?} didn't evaluate to a constant", *left).as_str());
            let r_val = interpret_expr(&*right, inputs)
                .expect(format!("{:?} didn't evaluate to a constant", *right).as_str());
            Some(l_val - r_val)
        },

        UnaryOp(UnaryOperator::Minus, v) => {
            Some(-(interpret_expr(&*v, inputs).expect(format!("{v:?} didn't evalute to a constant").as_str())))
        },
        UnaryOp(UnaryOperator::Plus, v) => {
            Some(interpret_expr(&*v, inputs).expect(format!("{v:?} didn't evalute to a constant").as_str()))
        },
        Constant(Value::I64(v)) => Some(*v),
        Call(name, args) => {
            if name == "input_int" && args.is_empty() {
                Some(inputs.pop_front().expect("Ran out of inputs"))
            } else {
                None
            }
        },
        _ => None
    }
}

fn interpret_statement(s: &Statement, inputs: &mut VecDeque<i64>, outputs: &mut VecDeque<i64>) {
    match s {
        Statement::Expr(Expr::Call(name, args)) => {
            if name == "print" && args.len() == 1 {
                let val = interpret_expr(&args[0], inputs).expect(format!("{:?} didn't evaluate to a constant", args[0]).as_str());
                outputs.push_back(val);
            } else {
                unimplemented!("Invalid call statement")
            }
        },
        Statement::Expr(e) => { interpret_expr(e, inputs); },
        _ => {}
    };
}

pub fn interpret(m: &Statement, inputs: &mut VecDeque<i64>, outputs: &mut VecDeque<i64>) {
    interpret_statement(m, inputs, outputs);
}