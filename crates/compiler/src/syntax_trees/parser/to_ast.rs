use super::parse_tree as pt;
use crate::constants::LABEL_MAIN;
use crate::syntax_trees::{ast, shared::*};
use crate::utils::id;
use std::iter::Peekable;
use std::vec::IntoIter;

fn to_ast_expr(pte: pt::Expr) -> ast::Expr {
    match pte {
        pt::Expr::Int(val) => ast::Expr::Constant(Value::I64(val)),
        pt::Expr::Bool(val) => ast::Expr::Constant(Value::Bool(val)),
        pt::Expr::Id(name) => ast::Expr::Id(id!(name)),
        pt::Expr::Unary(op, val) => {
            let ast_val = Box::new(to_ast_expr(*val));
            let ast_op = match op {
                pt::Operator::Minus => UnaryOperator::Minus,
                pt::Operator::Plus => UnaryOperator::Plus,
                pt::Operator::Not => UnaryOperator::Not,
                _ => panic!("{op:?} should never be in a unary expression"),
            };

            ast::Expr::UnaryOp(ast_op, ast_val)
        }
        pt::Expr::Parens(sub_expr) => to_ast_expr(*sub_expr),
        pt::Expr::Binary(left, op, right) => {
            let ast_left = Box::new(to_ast_expr(*left));
            let ast_right = Box::new(to_ast_expr(*right));
            let ast_op = match op {
                pt::Operator::Minus => BinaryOperator::Subtract,
                pt::Operator::Plus => BinaryOperator::Add,
                pt::Operator::And => BinaryOperator::And,
                pt::Operator::Or => BinaryOperator::Or,
                pt::Operator::Equals => BinaryOperator::Equals,
                pt::Operator::NotEquals => BinaryOperator::NotEquals,
                pt::Operator::Greater => BinaryOperator::Greater,
                pt::Operator::GreaterEquals => BinaryOperator::GreaterEquals,
                pt::Operator::Less => BinaryOperator::Less,
                pt::Operator::LessEquals => BinaryOperator::LessEquals,
                pt::Operator::Is => BinaryOperator::Is,
                pt::Operator::Asterisk => BinaryOperator::Multiply,
                pt::Operator::Not => {
                    panic!("pt::Operator::Not should never be in a Binary expression")
                }
            };

            ast::Expr::BinaryOp(ast_left, ast_op, ast_right)
        }
        pt::Expr::Call(id, args) => {
            let ast_args = args.into_iter().map(to_ast_expr).collect();

            ast::Expr::Call(id!(id), ast_args)
        }
        pt::Expr::Ternary(cond, pos, neg) => {
            let ast_cond = to_ast_expr(*cond);
            let ast_pos = to_ast_expr(*pos);
            let ast_neg = to_ast_expr(*neg);

            ast::Expr::Ternary(Box::new(ast_cond), Box::new(ast_pos), Box::new(ast_neg))
        }
        pt::Expr::Tuple(elems) => {
            let ast_elems = elems.into_iter().map(to_ast_expr).collect();

            ast::Expr::Tuple(ast_elems)
        }
        pt::Expr::Subscript(expr, idx) => ast::Expr::Subscript(Box::new(to_ast_expr(*expr)), idx),
    }
}

pub fn to_ast_statement<'a>(
    iter: &mut Peekable<IntoIter<pt::Statement<'a>>>,
) -> Option<ast::Statement> {
    match iter.next() {
        Some(pt::Statement::Expr(pte)) => Some(ast::Statement::Expr(to_ast_expr(pte))),
        Some(pt::Statement::Assign(name, pte)) => Some(ast::Statement::Assign(
            AssignDest::Id(id!(name)),
            to_ast_expr(pte),
        )),
        Some(pt::Statement::SubscriptAssign(name, idx, pte)) => Some(ast::Statement::Assign(
            AssignDest::Subscript(id!(name), idx),
            to_ast_expr(pte),
        )),
        Some(pt::Statement::If(cond, body)) => {
            // There could be many stacked else-if statements that we
            // need to consume all of here to combine them into one big
            // nested statement.
            let mut stacked_ast_neg_bodies = Vec::new();

            // For each successive else[-if] statement after this one,
            // add its condition and body to stacked_ast_neg_bodies, so
            // that we can nest them all
            loop {
                if let Some(pt::Statement::ElseIf(sub_cond, sub_body)) =
                    iter.next_if(|v| matches!(v, pt::Statement::ElseIf(_, _)))
                {
                    stacked_ast_neg_bodies
                        .push((Some(to_ast_expr(sub_cond)), to_ast_statements(sub_body)));
                } else if let Some(pt::Statement::Else(sub_body)) =
                    iter.next_if(|v| matches!(v, pt::Statement::Else(_)))
                {
                    stacked_ast_neg_bodies.push((None, to_ast_statements(sub_body)));
                    // The `else` must be the end of the chain, so let's
                    // just break here
                    break;
                } else {
                    break;
                }
            }

            if !matches!(stacked_ast_neg_bodies.last(), Some((None, _))) {
                // If there's no trailing unconditional-`else` clause, add an empty one
                stacked_ast_neg_bodies.push((None, vec![]));
            }

            // Nest the negative-case bodies recursively
            let mut ast_neg_body = stacked_ast_neg_bodies.pop().unwrap().1; // We know the cond is None
            for (opt_cond, sub_body) in stacked_ast_neg_bodies.into_iter().rev() {
                if let Some(cond) = opt_cond {
                    ast_neg_body = vec![ast::Statement::Conditional(cond, sub_body, ast_neg_body)];
                } else {
                    ast_neg_body = sub_body;
                }
            }

            let ast_cond = to_ast_expr(cond);
            let ast_body = to_ast_statements(body);

            Some(ast::Statement::Conditional(
                ast_cond,
                ast_body,
                ast_neg_body,
            ))
        }
        Some(pt::Statement::ElseIf(_, _)) | Some(pt::Statement::Else(_)) => {
            // Any else/if or else pt::Statements should've been
            // consumed in the if branch, assuming the input is valid.
            panic!(
                "Unexpected Else statement - either in wrong place or bug in to_ast's If branch"
            );
        }
        Some(pt::Statement::While(cond, body)) => Some(ast::Statement::WhileLoop(
            to_ast_expr(cond),
            to_ast_statements(body),
        )),
        None => None,
    }
}

fn to_ast_statements(body: Vec<pt::Statement>) -> Vec<ast::Statement> {
    let mut pt_iter = body.into_iter().peekable();

    let mut statements = vec![];
    while let Some(s) = to_ast_statement(&mut pt_iter) {
        statements.push(s);
    }

    statements
}

pub fn to_ast(ptm: pt::Module) -> ast::Program {
    ast::Program {
        functions: vec![ast::Function {
            name: id!(LABEL_MAIN),
            body: to_ast_statements(ptm.statements),
            types: TypeEnv::new(),
        }],
    }
}
