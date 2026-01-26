use crate::{ast::*, passes::ASTPass};

pub struct RemoveComplexOperands;

impl ASTPass for RemoveComplexOperands {
    fn run_pass(self, m: Module) -> Module {
        let Module::Body(old_body) = m;

        let mut new_body: Vec<Statement> = vec![];

        for i in old_body {
            rco_statement(&i, &mut new_body);
        }

        Module::Body(new_body)
    }
}

fn rco_statement(s: &Statement, new_body: &mut Vec<Statement>) {
    match s {
        Statement::Assign(id, expr) => {
            let transform = rco_expr(&expr, false);

            // Add the updated version of this statement with
            // the new expression to the body
            new_body.push(Statement::Assign(id.clone(), transform));
        }
        Statement::Expr(expr) => {
            // The expression statement itself doesn't need to
            // be atomic, because its not actually  being used
            // by anything, so theres no dependence on what
            // happens with the output
            let transform = rco_expr(&expr, false);

            // Add the updated version of this statement with
            // the new expression to the body
            new_body.push(Statement::Expr(transform));
        }
        Statement::Conditional(cond, pos, neg) => {
            // Condition must be non-atomic to enable good codegen
            let cond_transform = rco_expr(cond, false);

            let mut pos_new_body = Vec::new();
            pos.iter().for_each(|s| rco_statement(s, &mut pos_new_body));

            let mut neg_new_body = Vec::new();
            neg.iter().for_each(|s| rco_statement(s, &mut neg_new_body));

            new_body.push(Statement::Conditional(cond_transform, pos_new_body, neg_new_body));
        },
    }
}

fn rco_expr(e: &Expr, needs_atomicity: bool) -> Expr {
    match e {
        Expr::BinaryOp(left, op, right) => {
            // Get the transformed versions of the operands first
            let left_transform = rco_expr(&*left, true);
            let right_transform = rco_expr(&*right, true);

            // This same operation, but with the transformed operands
            let transformed_op =
                Expr::BinaryOp(Box::new(left_transform), *op, Box::new(right_transform));

            // If *this* expression needs to be atomic, extract it into
            // a statement-block and an id-expr. Otherwise, just use it
            // directly.
            let new_expr = if needs_atomicity {
                let id = Identifier::new_ephemeral();
                let ephemeral_assign = Statement::Assign(id.clone(), transformed_op);
                Expr::StatementBlock(vec![ephemeral_assign], Box::new(Expr::Id(id)))
            } else {
                transformed_op
            };

            new_expr
        }
        Expr::UnaryOp(op, val) => {
            let val_transform = rco_expr(&*val, true);

            let transformed_op = Expr::UnaryOp(*op, Box::new(val_transform));

            // If *this* expression needs to be atomic, extract it into an
            // assignment and an id-expr. Otherwise, just use it directly.
            let new_expr = if needs_atomicity {
                let id = Identifier::new_ephemeral();
                let ephemeral_assign = Statement::Assign(id.clone(), transformed_op);
                Expr::StatementBlock(vec![ephemeral_assign], Box::new(Expr::Id(id)))
            } else {
                transformed_op
            };

            new_expr
        }
        Expr::Constant(_) => e.clone(),
        Expr::Id(_) => e.clone(),
        Expr::Call(name, args) => {
            let new_args = args.iter().map(|a| rco_expr(a, true)).collect();

            let transformed_call = Expr::Call(name.clone(), new_args);

            let new_expr = if needs_atomicity {
                let id = Identifier::new_ephemeral();
                let ephemeral_assign = Statement::Assign(id.clone(), transformed_call);

                Expr::StatementBlock(vec![ephemeral_assign], Box::new(Expr::Id(id)))
            } else {
                transformed_call
            };

            new_expr
        }
        Expr::StatementBlock(statements, expr) => {
            let mut new_body = vec![];
            statements
                .iter()
                .for_each(|s| rco_statement(s, &mut new_body));

            let transformed_expr = rco_expr(expr, true);

            let transformed_block = Expr::StatementBlock(new_body, Box::new(transformed_expr));

            let new_expr = if needs_atomicity {
                let id = Identifier::new_ephemeral();
                let ephemeral_assign = Statement::Assign(id.clone(), transformed_block);

                Expr::StatementBlock(vec![ephemeral_assign], Box::new(Expr::Id(id)))
            } else {
                transformed_block
            };

            new_expr
        }
        Expr::Ternary(cond, pos, neg) => {
            // Condition must be non-atomic to enable good codegen
            let transformed_cond = rco_expr(cond, false);
            let transformed_pos = rco_expr(pos, true);
            let transformed_neg = rco_expr(neg, true);

            let transformed_ternary = Expr::Ternary(
                Box::new(transformed_cond),
                Box::new(transformed_pos),
                Box::new(transformed_neg),
            );

            let new_expr = if needs_atomicity {
                let id = Identifier::new_ephemeral();
                let ephemeral_assign = Statement::Assign(id.clone(), transformed_ternary);

                Expr::StatementBlock(vec![ephemeral_assign], Box::new(Expr::Id(id)))
            } else {
                transformed_ternary
            };

            new_expr
        }
    }
}
