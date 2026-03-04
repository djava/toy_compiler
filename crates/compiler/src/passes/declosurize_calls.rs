//! `DeclosurizeCalls` Pass
//!
//! Rewrites calls through closure variables (`Expr::Call` with an
//! `Expr::Id `callee) into the two-step closure-calling protocol:
//! extract the function pointer from index 0 of the closure tuple, then
//! call it with the closure tuple prepended as the first argument.
//!
//! It is mandatory to run this pass
//!
//! Pre-conditions:
//! - `GlobalizeIdentifiers`
//! - `ClosurizeFunctions`
//! - `ClosurizeLambdas`
//! - `InjectAllocations` (or else the allocations for closures will
//!                        never get added)
//!
//! Post-conditions:
//! - All calls through `Expr::Id` closure variables are replaced with
//!   `StatementBlocks` that extract and call the underlying function
//!   pointer

use crate::{
    passes::ASTPass,
    syntax_trees::{ast::*, shared::*},
};

#[derive(Debug)]
pub struct DeclosurizeCalls;

impl ASTPass for DeclosurizeCalls {
    fn run_pass(self, mut m: Program) -> Program {
        for f in m.functions.iter_mut() {
            for s in f.body.iter_mut() {
                declosurize_for_statement(s);
            }
        }
        m
    }
}

fn declosurize_for_statement(s: &mut Statement) {
    match s {
        Statement::Assign(_, expr, _) => declosurize_for_expr(expr),
        Statement::Expr(expr) => declosurize_for_expr(expr),
        Statement::Conditional(expr, statements, statements1) => {
            declosurize_for_expr(expr);
            for s in statements {
                declosurize_for_statement(s);
            }
            for s in statements1 {
                declosurize_for_statement(s);
            }
        }
        Statement::WhileLoop(expr, statements) => {
            declosurize_for_expr(expr);
            for s in statements {
                declosurize_for_statement(s);
            }
        }
        Statement::Return(expr) => {
            declosurize_for_expr(expr);
        }
    }
}

fn declosurize_for_expr(e: &mut Expr) {
    match e {
        Expr::Call(func, args) => {
            declosurize_for_expr(func);
            for a in args.iter_mut() {
                declosurize_for_expr(a);
            }

            // If it's externed, this will be a GlobalSymbol instead of
            // an ID
            if let Expr::Id(clos_id) = &**func {
                let fn_ptr_id = Identifier::new_ephemeral();
                let result_id = Identifier::new_ephemeral();
                let mut args_with_capture = vec![Expr::Id(clos_id.clone())];
                args_with_capture.extend(args.clone());

                *e = Expr::StatementBlock(
                    vec![
                        Statement::Assign(
                            AssignDest::Id(fn_ptr_id.clone()),
                            Expr::Subscript(
                                Box::new(Expr::Id(clos_id.clone())),
                                Box::new(Expr::Constant(Value::I64(0))),
                            ),
                            None,
                        ),
                        Statement::Assign(
                            AssignDest::Id(result_id.clone()),
                            Expr::Call(Box::new(Expr::Id(fn_ptr_id.clone())), args_with_capture),
                            None,
                        ),
                    ],
                    Box::new(Expr::Id(result_id.clone())),
                );
            }
        }

        Expr::BinaryOp(expr, _, expr1) => {
            declosurize_for_expr(expr);
            declosurize_for_expr(expr1);
        }
        Expr::UnaryOp(_, expr) => {
            declosurize_for_expr(expr);
        }
        Expr::Ternary(expr, expr1, expr2) => {
            declosurize_for_expr(expr);
            declosurize_for_expr(expr1);
            declosurize_for_expr(expr2);
        }
        Expr::StatementBlock(statements, expr) => {
            declosurize_for_expr(expr);
            for s in statements {
                declosurize_for_statement(s);
            }
        }
        Expr::Tuple(exprs) | Expr::Array(exprs) => {
            for e in exprs {
                declosurize_for_expr(e);
            }
        }
        Expr::Subscript(expr, _) => {
            declosurize_for_expr(expr);
        }

        Expr::Constant(..)
        | Expr::Id(..)
        | Expr::Allocate(..)
        | Expr::GlobalSymbol(..)
        | Expr::Lambda(..)
        | Expr::Closure(..) => {}
    }
}
