use std::sync::Arc;

use crate::ast::{AssignDest, TypeEnv, ValueType};
pub use crate::ast::{BinaryOperator, Identifier, UnaryOperator, Value};
use indexmap::IndexMap;

#[derive(Debug, Clone)]
pub enum Atom {
    Constant(Value),
    Variable(Identifier),
    GlobalSymbol(Arc<str>)
}

#[derive(Debug, Clone)]
pub enum Expr {
    Atom(Atom),
    UnaryOp(UnaryOperator, Atom),
    BinaryOp(Atom, BinaryOperator, Atom),
    Call(Identifier, Vec<Atom>),
    Allocate(usize, ValueType),
    Subscript(Atom, i64)
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expr(Expr),
    Assign(AssignDest, Expr),
    Return(Atom),
    Goto(Identifier),
    If(Expr, Identifier, Identifier),
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
}

pub type BlockMap = IndexMap<Identifier, Block>;

#[derive(Debug, Clone)]
pub struct IRProgram {
    pub blocks: BlockMap,
    pub types: TypeEnv,
}
