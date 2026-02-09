use indexmap::IndexMap;

use super::shared::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Constant(Value),
    BinaryOp(Box<Expr>, BinaryOperator, Box<Expr>),
    UnaryOp(UnaryOperator, Box<Expr>),
    Call(Identifier, Vec<Expr>),
    Id(Identifier),
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
    StatementBlock(Vec<Statement>, Box<Expr>),
    Tuple(Vec<Expr>),
    Subscript(Box<Expr>, i64),
    Allocate(usize, ValueType),
    GlobalSymbol(Identifier)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Assign(AssignDest, Expr),
    Expr(Expr),
    Conditional(Expr, Vec<Statement>, Vec<Statement>),
    WhileLoop(Expr, Vec<Statement>),
    Return(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: Identifier,
    pub body: Vec<Statement>,
    pub types: TypeEnv,
    pub params: IndexMap<Identifier, ValueType>,
    pub return_type: ValueType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub functions: Vec<Function>,
    pub function_types: TypeEnv,
}
