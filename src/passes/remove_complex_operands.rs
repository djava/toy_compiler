use crate::{ast::*, passes::IRPass};

pub struct RemoveComplexOperands;

struct ExprTransformation {
    new_expr: Expr,
    ephemeral_assigns: Vec<(Identifier, Expr)>,
}

impl IRPass for RemoveComplexOperands {
    fn run_pass(m: Module) -> Module {
        let Module::Body(old_body) = m;

        let mut new_body: Vec<Statement> = vec![];

        for i in old_body {
            match i {
                Statement::Assign(id, expr) => {
                    let transform = rco_expr(&expr, false);

                    // Take all the ephemeral transforms that happen
                    // inside the expression and add them before this
                    // statement
                    let ephemeral_assign_stmts = transform
                        .ephemeral_assigns
                        .iter()
                        .map(|(id, expr)| Statement::Assign(id.clone(), expr.clone()));
                    new_body.extend(ephemeral_assign_stmts);

                    // Add the updated version of this statement with
                    // the new expression to the body
                    new_body.push(Statement::Assign(id, transform.new_expr));
                }
                Statement::Expr(expr) => {
                    // The expression statement itself doesn't need to
                    // be atomic, because its not actually  being used
                    // by anything, so theres no dependence on what
                    // happens with the output
                    let transform = rco_expr(&expr, false);

                    // Take all the ephemeral transforms that happen
                    // inside the expression and add them before this
                    // statement
                    let ephemeral_assign_stmts = transform
                        .ephemeral_assigns
                        .iter()
                        .map(|(id, expr)| Statement::Assign(id.clone(), expr.clone()));
                    new_body.extend(ephemeral_assign_stmts);

                    // Add the updated version of this statement with
                    // the new expression to the body
                    new_body.push(Statement::Expr(transform.new_expr));
                }
            }
        }

        Module::Body(new_body)
    }
}

fn rco_expr(e: &Expr, needs_atomicity: bool) -> ExprTransformation {
    match e {
        Expr::BinaryOp(left, op, right) => {
            // Get the transformed versions of the operands first
            let left_transform = rco_expr(&*left, true);
            let right_transform = rco_expr(&*right, true);

            // The ephermal assigns first need to include the ones for
            // the left and right operands, in that order
            let mut ephemeral_assigns = vec![];
            ephemeral_assigns.extend(left_transform.ephemeral_assigns);
            ephemeral_assigns.extend(right_transform.ephemeral_assigns);

            // This same operation, but with the transformed operands
            let transformed_op = Expr::BinaryOp(
                Box::new(left_transform.new_expr),
                *op,
                Box::new(right_transform.new_expr),
            );

            // If *this* expression needs to be atomic, extract it into an
            // assignment and an id-expr. Otherwise, just use it directly.
            let new_expr = if needs_atomicity {
                let id = Identifier::new_ephemeral();
                ephemeral_assigns.push((id.clone(), transformed_op));
                Expr::Id(id)
            } else {
                transformed_op
            };

            ExprTransformation {
                new_expr,
                ephemeral_assigns,
            }
        }
        Expr::UnaryOp(op, val) => {
            let val_transform = rco_expr(&*val, true);

            // The ephermal assigns first need to include the ones for
            // the operand
            let mut ephemeral_assigns = val_transform.ephemeral_assigns;

            let transformed_op = Expr::UnaryOp(*op, Box::new(val_transform.new_expr));

            // If *this* expression needs to be atomic, extract it into an
            // assignment and an id-expr. Otherwise, just use it directly.
            let new_expr = if needs_atomicity {
                let id = Identifier::new_ephemeral();
                ephemeral_assigns.push((id.clone(), transformed_op));
                Expr::Id(id)
            } else {
                transformed_op
            };

            ExprTransformation {
                new_expr,
                ephemeral_assigns,
            }
        }
        Expr::Constant(_) => ExprTransformation {
            new_expr: e.clone(),
            ephemeral_assigns: vec![],
        },
        Expr::Id(_) => ExprTransformation {
            new_expr: e.clone(),
            ephemeral_assigns: vec![],
        },
        Expr::Call(name, args) => {
            let mut ephemeral_assigns = vec![];
            let mut new_args = vec![];
            // Each arg must be transformed, and all the ephermal
            // assignments must be inserted before this call happens
            for arg in args {
                let arg_transform = rco_expr(&*arg, true);
                ephemeral_assigns.extend(arg_transform.ephemeral_assigns);
                new_args.push(arg_transform.new_expr);
            }

            let transformed_call = Expr::Call(name.clone(), new_args);

            let new_expr = if needs_atomicity {
                let id = Identifier::new_ephemeral();
                ephemeral_assigns.push((id.clone(), transformed_call));
                Expr::Id(id)
            } else {
                transformed_call
            };

            ExprTransformation {
                new_expr,
                ephemeral_assigns,
            }
        }
    }
}
