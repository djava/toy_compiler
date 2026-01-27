pub use crate::ast::{BinaryOperator, Identifier, UnaryOperator, Value};
use indexmap::IndexMap;

#[derive(Debug, Clone)]
pub enum Atom {
    Constant(Value),
    Variable(Identifier),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Atom(Atom),
    UnaryOp(UnaryOperator, Atom),
    BinaryOp(Atom, BinaryOperator, Atom),
    Call(Identifier, Vec<Atom>),
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expr(Expr),
    Assign(Identifier, Expr),
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
    pub blocks: BlockMap
}
