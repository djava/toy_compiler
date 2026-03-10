use crate::{
    passes::ASTPass,
    syntax_trees::{ast::*, shared::*},
};

/// `DeclosurizeCalls` Pass
///
/// Rewrites calls through closure variables (`Expr::Call` with an
/// `Expr::Id `callee) into the two-step closure-calling protocol:
/// extract the function pointer from index 0 of the closure tuple, then
/// call it with the closure tuple prepended as the first argument.
///
/// It is mandatory to run this pass
///
/// Pre-conditions:
/// - `GlobalizeIdentifiers`
/// - `ClosurizeFunctions`
/// - `ClosurizeLambdas`
/// - `InjectAllocations` (or else the allocations for closures will
///                        never get added)
///
/// Post-conditions:
/// - All calls through `Expr::Id` closure variables are replaced with
///   `StatementBlocks` that extract and call the underlying function
///   pointer
#[derive(Debug)]
pub struct DeclosurizeCalls;

impl ASTPass for DeclosurizeCalls {
    fn run_pass(self, mut m: Program) -> Program {
        for f in m.functions.iter_mut() {
            for s in f.body.iter_mut() {
                declosurize_for_statement(s, &mut f.types);
            }
        }
        m
    }
}

fn declosurize_for_statement(s: &mut Statement, env: &mut TypeEnv) {
    match s {
        Statement::Assign(_, expr, _) => declosurize_for_expr(expr, env),
        Statement::Expr(expr) => declosurize_for_expr(expr, env),
        Statement::Conditional(expr, statements, statements1) => {
            declosurize_for_expr(expr, env);
            for s in statements {
                declosurize_for_statement(s, env);
            }
            for s in statements1 {
                declosurize_for_statement(s, env);
            }
        }
        Statement::WhileLoop(expr, statements) => {
            declosurize_for_expr(expr, env);
            for s in statements {
                declosurize_for_statement(s, env);
            }
        }
        Statement::Return(expr) => {
            declosurize_for_expr(expr, env);
        }
    }
}

fn declosurize_for_expr(e: &mut Expr, env: &mut TypeEnv) {
    match e {
        Expr::Call(func, args) => {
            declosurize_for_expr(func, env);
            for a in args.iter_mut() {
                declosurize_for_expr(a, env);
            }

            // If it's externed, this will be a GlobalSymbol instead of
            // an ID
            if let Expr::Id(clos_id) = &**func {
                let clos_type = env[clos_id].clone();
                let fn_ptr_id = Identifier::new_ephemeral();
                env.insert(
                    fn_ptr_id.clone(),
                    ValueType::PointerType(Box::new(clos_type.clone())),
                );

                let result_id = Identifier::new_ephemeral();
                if let ValueType::TupleType(elems) = &clos_type
                    && let ValueType::FunctionType(_, ret_type) = &elems[0]
                {
                    env.insert(result_id.clone(), *ret_type.clone());
                } else {
                    panic!("Closure function type was not function ({clos_type:?})");
                }

                let mut args_with_capture = vec![Expr::Id(clos_id.clone())];
                args_with_capture.extend(args.clone());

                let mut statements = vec![Statement::Assign(
                    AssignDest::Id(fn_ptr_id.clone()),
                    Expr::Subscript(
                        Box::new(Expr::Id(clos_id.clone())),
                        Box::new(Expr::Constant(Value::I64(0))),
                    ),
                    None,
                )];

                if env[&result_id] == ValueType::NoneType {
                    // Zero-size types can't be assigned to because they
                    // have size 0 (so no width), so just use a
                    // statement instead.
                    statements.push(Statement::Expr(Expr::Call(
                        Box::new(Expr::Id(fn_ptr_id.clone())),
                        args_with_capture,
                    )));
                } else {
                    statements.push(Statement::Assign(
                        AssignDest::Id(result_id.clone()),
                        Expr::Call(Box::new(Expr::Id(fn_ptr_id.clone())), args_with_capture),
                        None,
                    ));
                }

                *e = Expr::StatementBlock(statements, Box::new(Expr::Id(result_id.clone())));
            }
        }

        Expr::BinaryOp(expr, _, expr1) => {
            declosurize_for_expr(expr, env);
            declosurize_for_expr(expr1, env);
        }
        Expr::UnaryOp(_, expr) => {
            declosurize_for_expr(expr, env);
        }
        Expr::Ternary(expr, expr1, expr2) => {
            declosurize_for_expr(expr, env);
            declosurize_for_expr(expr1, env);
            declosurize_for_expr(expr2, env);
        }
        Expr::StatementBlock(statements, expr) => {
            declosurize_for_expr(expr, env);
            for s in statements {
                declosurize_for_statement(s, env);
            }
        }
        Expr::Tuple(exprs) | Expr::Array(exprs) => {
            for e in exprs {
                declosurize_for_expr(e, env);
            }
        }
        Expr::Subscript(expr, _) => {
            declosurize_for_expr(expr, env);
        }

        Expr::Constant(..)
        | Expr::Id(..)
        | Expr::Allocate(..)
        | Expr::GlobalSymbol(..)
        | Expr::Lambda(..)
        | Expr::Closure(..) => {}
    }
}
