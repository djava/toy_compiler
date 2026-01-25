use crate::ast;
use crate::parser::parse_tree as pt;
use std::sync::Arc;

fn to_ast_expr(pte: pt::Expr) -> ast::Expr {
    match pte {
        pt::Expr::Int(val) => ast::Expr::Constant(ast::Value::I64(val)),
        pt::Expr::Id(name) => ast::Expr::Id(ast::Identifier::Named(Arc::from(name))),
        pt::Expr::Unary(op, val) => {
            let ast_val = Box::new(to_ast_expr(*val));
            let ast_op = match op {
                pt::Operator::Minus => ast::UnaryOperator::Minus,
                pt::Operator::Plus => ast::UnaryOperator::Plus,
            };

            ast::Expr::UnaryOp(ast_op, ast_val)
        }
        pt::Expr::Parens(sub_expr) => to_ast_expr(*sub_expr),
        pt::Expr::Binary(left, op, right) => {
            let ast_left = Box::new(to_ast_expr(*left));
            let ast_right = Box::new(to_ast_expr(*right));
            let ast_op = match op {
                pt::Operator::Minus => ast::BinaryOperator::Subtract,
                pt::Operator::Plus => ast::BinaryOperator::Add,
            };

            ast::Expr::BinaryOp(ast_left, ast_op, ast_right)
        }
        pt::Expr::Call(id, args) => {
            let ast_args = args.into_iter().map(to_ast_expr).collect();

            ast::Expr::Call(ast::Identifier::Named(Arc::from(id)), ast_args)
        }
    }
}

fn to_ast_statement(pts: pt::Statement) -> ast::Statement {
    match pts {
        pt::Statement::Expr(pte) => ast::Statement::Expr(to_ast_expr(pte)),
        pt::Statement::Assign(name, pte) => {
            ast::Statement::Assign(ast::Identifier::Named(Arc::from(name)), to_ast_expr(pte))
        }
    }
}

pub fn to_ast(ptm: pt::Module) -> ast::Module {
    ast::Module::Body(ptm.statements.into_iter().map(to_ast_statement).collect())
}
