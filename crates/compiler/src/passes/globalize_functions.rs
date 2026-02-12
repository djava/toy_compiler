use crate::{
    passes::ASTPass,
    syntax_trees::{ast::*, shared::*},
};

pub struct GlobalizeFunctions;

impl ASTPass for GlobalizeFunctions {
    fn run_pass(self, mut m: Program) -> Program {
        let func_types = &m.function_types;
        for f in m.functions.iter_mut() {
            for s in f.body.iter_mut() {
                globalize_for_statement(s, func_types);
            }
        }

        m
    }
}
fn globalize_for_statement(s: &mut Statement, func_types: &TypeEnv) {
    match s {
        Statement::Assign(_, expr) | Statement::Expr(expr) | Statement::Return(expr) => {
            globalize_for_expr(expr, func_types);
        }
        Statement::Conditional(cond, pos_body, neg_body) => {
            globalize_for_expr(cond, func_types);
            for s in pos_body {
                globalize_for_statement(s, func_types);
            }
            for s in neg_body {
                globalize_for_statement(s, func_types);
            }
        }
        Statement::WhileLoop(cond, body) => {
            globalize_for_expr(cond, func_types);
            for s in body {
                globalize_for_statement(s, func_types);
            }
        }
    }
}

fn globalize_for_expr(e: &mut Expr, func_types: &TypeEnv) {
    match e {
        Expr::Id(id) => {
            if func_types.contains_key(id) {
                *e = Expr::GlobalSymbol(id.clone());
            }
        }

        Expr::Call(func, args) => {
            for a in args.iter_mut() {
                globalize_for_expr(a, func_types);
            }

            globalize_for_expr(func, func_types);
        }
        Expr::BinaryOp(l, _, r) => {
            globalize_for_expr(l, func_types);
            globalize_for_expr(r, func_types);
        }
        Expr::UnaryOp(_, expr) => {
            globalize_for_expr(expr, func_types);
        }
        Expr::Ternary(cond, pos, neg) => {
            globalize_for_expr(cond, func_types);
            globalize_for_expr(pos, func_types);
            globalize_for_expr(neg, func_types);
        }
        Expr::StatementBlock(statements, expr) => {
            for s in statements {
                globalize_for_statement(s, func_types);
            }
            globalize_for_expr(expr, func_types);
        }
        Expr::Tuple(elems) => {
            for e in elems {
                globalize_for_expr(e, func_types);
            }
        }
        Expr::Subscript(expr, _) => {
            globalize_for_expr(expr, func_types);
        }

        Expr::Allocate(_, _) | Expr::Constant(_) | Expr::GlobalSymbol(_) => {}
    }
}
