use super::parse_tree as pt;
use crate::syntax_trees::{ast, shared::*};
use crate::utils::{global, local};
use std::iter::Peekable;
use std::vec::IntoIter;

fn to_ast_expr(pte: pt::Expr, func_id: &Identifier) -> ast::Expr {
    match pte {
        pt::Expr::Int(val) => ast::Expr::Constant(Value::I64(val)),
        pt::Expr::Bool(val) => ast::Expr::Constant(Value::Bool(val)),
        pt::Expr::Id(name) => ast::Expr::Id(local!(name, func_id.clone())),
        pt::Expr::Unary(op, val) => {
            let ast_val = Box::new(to_ast_expr(*val, func_id));
            let ast_op = match op {
                pt::Operator::Minus => UnaryOperator::Minus,
                pt::Operator::Plus => UnaryOperator::Plus,
                pt::Operator::Not => UnaryOperator::Not,
                _ => panic!("{op:?} should never be in a unary expression"),
            };

            ast::Expr::UnaryOp(ast_op, ast_val)
        }
        pt::Expr::Parens(sub_expr) => to_ast_expr(*sub_expr, func_id),
        pt::Expr::Binary(left, op, right) => {
            let ast_left = Box::new(to_ast_expr(*left, func_id));
            let ast_right = Box::new(to_ast_expr(*right, func_id));
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
                pt::Operator::LeftShift => BinaryOperator::LeftShift,
                pt::Operator::RightShift => BinaryOperator::RightShift,
                pt::Operator::Divide => BinaryOperator::Divide,
                pt::Operator::Not => {
                    panic!("pt::Operator::Not should never be in a Binary expression")
                }
            };

            ast::Expr::BinaryOp(ast_left, ast_op, ast_right)
        }
        pt::Expr::Call(func, args) => {
            let ast_args = args.into_iter().map(|a| to_ast_expr(a, func_id)).collect();

            ast::Expr::Call(Box::new(to_ast_expr(*func, func_id)), ast_args)
        }
        pt::Expr::Ternary(cond, pos, neg) => {
            let ast_cond = to_ast_expr(*cond, func_id);
            let ast_pos = to_ast_expr(*pos, func_id);
            let ast_neg = to_ast_expr(*neg, func_id);

            ast::Expr::Ternary(Box::new(ast_cond), Box::new(ast_pos), Box::new(ast_neg))
        }
        pt::Expr::Tuple(elems) => {
            let ast_elems = elems.into_iter().map(|e| to_ast_expr(e, func_id)).collect();

            ast::Expr::Tuple(ast_elems)
        }
        pt::Expr::Array(elems) => {
            let ast_elems = elems.into_iter().map(|e| to_ast_expr(e, func_id)).collect();

            ast::Expr::Array(ast_elems)
        }
        pt::Expr::Subscript(expr, idx) => ast::Expr::Subscript(
            Box::new(to_ast_expr(*expr, func_id)),
            Box::new(to_ast_expr(*idx, func_id)),
        ),
        pt::Expr::Lambda(args, body) => {
            let lambda_id = Identifier::new_lambda_name();

            let ast_params = args
                .into_iter()
                .map(|a| (local!(a, lambda_id.clone()), ValueType::Indeterminate))
                .collect();
            let ast_body = to_ast_statements(body, &lambda_id);

            let func = ast::Function {
                name: lambda_id.clone(),
                body: ast_body,
                types: TypeEnv::new(),
                params: ast_params,
                return_type: ValueType::Indeterminate,
            };

            ast::Expr::Lambda(func)
        }
        pt::Expr::StringLiteral(s) => {
            let c: Vec<_> = s
                .chars()
                .chain(std::iter::once('\0'))
                .map(|c| ast::Expr::Constant(Value::Char(c)))
                .collect();

            ast::Expr::Array(c)
        }
    }
}

pub fn to_ast_statement<'a>(
    iter: &mut Peekable<IntoIter<pt::Statement<'a>>>,
    func_name: &Identifier,
    ast_statements: &mut Vec<ast::Statement>,
) {
    match iter.next() {
        Some(pt::Statement::Expr(pte)) => {
            ast_statements.push(ast::Statement::Expr(to_ast_expr(pte, func_name)));
        }
        Some(pt::Statement::Assign(name, pte, type_hint)) => {
            ast_statements.push(ast::Statement::Assign(
                AssignDest::Id(local!(name, func_name.clone())),
                to_ast_expr(pte, func_name),
                type_hint,
            ));
        }
        Some(pt::Statement::SubscriptAssign(container, idx, pte)) => {
            ast_statements.push(ast::Statement::Assign(
                AssignDest::ComplexSubscript(ComplexSubscript {
                    container: to_ast_expr(container, func_name),
                    index: to_ast_expr(idx, func_name),
                }),
                to_ast_expr(pte, func_name),
                None,
            ));
        }
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
                    stacked_ast_neg_bodies.push((
                        Some(to_ast_expr(sub_cond, func_name)),
                        to_ast_statements(sub_body, func_name),
                    ));
                } else if let Some(pt::Statement::Else(sub_body)) =
                    iter.next_if(|v| matches!(v, pt::Statement::Else(_)))
                {
                    stacked_ast_neg_bodies.push((None, to_ast_statements(sub_body, func_name)));
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

            let ast_cond = to_ast_expr(cond, func_name);
            let ast_body = to_ast_statements(body, func_name);

            ast_statements.push(ast::Statement::Conditional(
                ast_cond,
                ast_body,
                ast_neg_body,
            ));
        }
        Some(pt::Statement::ElseIf(_, _)) | Some(pt::Statement::Else(_)) => {
            // Any else/if or else pt::Statements should've been
            // consumed in the if branch, assuming the input is valid.
            panic!(
                "Unexpected Else statement - either in wrong place or bug in to_ast's If branch"
            );
        }
        Some(pt::Statement::While(cond, body)) => {
            ast_statements.push(ast::Statement::WhileLoop(
                to_ast_expr(cond, func_name),
                to_ast_statements(body, func_name),
            ));
        }
        Some(pt::Statement::Return(opt_val)) => {
            let val = opt_val
                .map(|v| to_ast_expr(v, func_name))
                .unwrap_or(ast::Expr::Constant(Value::None));

            ast_statements.push(ast::Statement::Return(val));
        }
        Some(pt::Statement::For(init, cond, incr, mut body)) => {
            body.push(*incr);

            ast_statements.extend(to_ast_statements(vec![*init], func_name));
            ast_statements.push(ast::Statement::WhileLoop(
                to_ast_expr(cond, func_name),
                to_ast_statements(body, func_name),
            ));
        }
        None => {}
    }
}

fn to_ast_statements(body: Vec<pt::Statement>, func_name: &Identifier) -> Vec<ast::Statement> {
    let mut pt_iter = body.into_iter().peekable();

    let mut statements = vec![];
    while let Some(_) = pt_iter.peek() {
        to_ast_statement(&mut pt_iter, func_name, &mut statements);
    }

    statements
}

pub fn to_ast_function(ptf: pt::Function) -> ast::Function {
    let func_id = global!(ptf.name);
    let ast_params = ptf
        .params
        .into_iter()
        .map(|(name, typ)| (local!(name, func_id.clone()), typ))
        .collect();

    let ast_body = to_ast_statements(ptf.statements, &func_id);
    ast::Function {
        name: func_id,
        body: ast_body,
        params: ast_params,
        return_type: ptf.return_type,
        types: TypeEnv::new(),
    }
}

pub fn to_ast(ptm: pt::Module) -> ast::Program {
    ast::Program {
        functions: ptm.functions.into_iter().map(to_ast_function).collect(),
        global_types: TypeEnv::new(),
    }
}
