use crate::{ast::*, passes::ASTPass};

pub struct ShortCircuiting;

impl ASTPass for ShortCircuiting {
    fn run_pass(self, mut m: Module) -> Module {
        m.body.iter_mut().for_each(shortcircuit_statement);

        m
    }
}

fn shortcircuit_statement(s: &mut Statement) {
    match s {
        Statement::Assign(_, expr)
        | Statement::AssignSubscript(_, _, expr)
        | Statement::Expr(expr)
        | Statement::Conditional(expr, _, _)
        | Statement::WhileLoop(expr, _) => shortcircuit_expr(expr),
    }
}

fn shortcircuit_expr(e: &mut Expr) {
    // Recurse sub-exprs
    match e {
        Expr::BinaryOp(e1, _, e2) => {
            shortcircuit_expr(&mut *e1);
            shortcircuit_expr(&mut *e2);
        }
        Expr::UnaryOp(_, e) => {
            shortcircuit_expr(&mut *e);
        }
        Expr::Call(_, es) => {
            es.into_iter().for_each(shortcircuit_expr);
        }
        Expr::Ternary(e1, e2, e3) => {
            shortcircuit_expr(&mut *e1);
            shortcircuit_expr(&mut *e2);
            shortcircuit_expr(&mut *e3);
        }
        Expr::StatementBlock(ss, e) => {
            ss.iter_mut().for_each(shortcircuit_statement);
            shortcircuit_expr(&mut *e);
        }
        Expr::Constant(_) | Expr::Id(_) => {}
        Expr::Tuple(elems) => {
            elems.iter_mut().for_each(shortcircuit_expr);
        },
        Expr::Subscript(tup, _idx) => {
            shortcircuit_expr(tup);
        },
        Expr::Allocate(_, _) => {},
        Expr::GlobalSymbol(_) => {},
    }

    // Apply transformation, only applies to expressions with And/Or
    // BinaryOperator
    if let Expr::BinaryOp(left, BinaryOperator::And, right) = e {
        // (A && B) is equivalent to (A ? B : false)
        *e = Expr::Ternary(
            left.clone(),
            right.clone(),
            Box::new(Expr::Constant(Value::Bool(false))),
        );
    } else if let Expr::BinaryOp(left, BinaryOperator::Or, right) = e {
        // (A || B) is equivalent to (A ? true : B)
        *e = Expr::Ternary(
            left.clone(),
            Box::new(Expr::Constant(Value::Bool(false))),
            right.clone(),
        );
    }
}
