#![allow(dead_code)]

use compiler::syntax_trees::{ast, shared::*};
use std::collections::HashMap;

pub use compiler;

pub mod ast_interpreter;
pub mod ir_interpreter;
pub mod x86_interpreter;

mod interpreter_utils;
use interpreter_utils::id;

type ValueEnv = HashMap<Identifier, Value>;

pub fn ast_read_int() -> ast::Expr {
    ast::Expr::Call(Box::new(ast::Expr::GlobalSymbol(id!("read_int"))), vec![])
}

pub fn ast_print_int(e: ast::Expr) -> ast::Expr {
    ast::Expr::Call(Box::new(ast::Expr::GlobalSymbol(id!("print_int"))), vec![e])
}

pub fn ast_const_int(i: i64) -> ast::Expr {
    ast::Expr::Constant(Value::I64(i))
}

pub fn ast_call(name: &str, args: Vec<ast::Expr>) -> ast::Expr {
    ast::Expr::Call(Box::new(ast::Expr::GlobalSymbol(id!(name))), args)
}
