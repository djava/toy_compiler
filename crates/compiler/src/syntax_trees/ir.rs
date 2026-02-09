use std::sync::Arc;

use super::shared::*;
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
    TailCall(Identifier, Vec<Atom>),
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
}

pub type BlockMap = IndexMap<Identifier, Block>;

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Identifier,
    pub params: IndexMap<Identifier, ValueType>,
    pub blocks: BlockMap,
    pub entry_block: Identifier,
    pub types: TypeEnv,
}

#[derive(Debug, Clone)]
pub struct IRProgram {
    pub functions: Vec<Function>
}