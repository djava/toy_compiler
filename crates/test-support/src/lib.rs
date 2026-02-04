#![allow(dead_code)]

use compiler::ast::{self, AssignDest, Identifier, Value};
use std::collections::HashMap;

pub mod ast_interpreter;
pub mod ir_interpreter;
pub mod x86_interpreter;

pub(crate) mod interpreter_utils;

type ValueEnv = HashMap<AssignDest, Value>;

pub fn ast_read_int() -> ast::Expr {
    ast::Expr::Call(Identifier::from("read_int"), vec![])
}

pub fn ast_print_int(e: ast::Expr) -> ast::Expr {
    ast::Expr::Call(Identifier::from("print_int"), vec![e])
}

pub fn ast_const_int(i: i64) -> ast::Expr {
    ast::Expr::Constant(ast::Value::I64(i))
}
