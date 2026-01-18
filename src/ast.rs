use std::sync::atomic::{AtomicU64, Ordering};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ValueType {
    IntType,
    FunctionType(u16),
    None
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    I64(i64),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOperator {
    Plus,
    Minus,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Identifier {
    Ephemeral(u64),
    Named(String)
}

impl Identifier {
    pub fn new_ephemeral() -> Identifier {
        static COUNTER: AtomicU64 = AtomicU64::new(0);
        
        let current_counter = COUNTER.load(Ordering::Relaxed);
        COUNTER.store(current_counter + 1, Ordering::Relaxed);
        Identifier::Ephemeral(current_counter)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Constant(Value),
    BinaryOp(Box<Expr>, BinaryOperator, Box<Expr>),
    UnaryOp(UnaryOperator, Box<Expr>),
    Call(Identifier, Vec<Expr>),
    Id(Identifier),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Assign(Identifier, Expr),
    Expr(Expr)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Module {
    Body(Vec<Statement>)
}