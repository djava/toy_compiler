use crate::ast::{BinaryOperator, Identifier, UnaryOperator, Value};
use indexmap::IndexMap;

pub enum Atom {
    Constant(Value),
    Variable(Identifier),
    Call(Identifier, Vec<Atom>),
}

pub enum Expr {
    Atom(Atom),
    UnaryOp(UnaryOperator, Atom),
    BinaryOp(Atom, BinaryOperator, Atom),
}

pub enum Statement {
    Expr(Expr),
    Assign(Identifier, Expr)
}

pub enum Tail {
    Return(Expr),
    Goto(Identifier),
    If(Expr, Identifier, Option<Identifier>),
    None
}

pub struct Block {
    pub label: Identifier,
    pub statements: Vec<Statement>,
    pub tail: Tail
}

pub struct IRProgram {
    pub blocks: IndexMap<Identifier, Block>
}
