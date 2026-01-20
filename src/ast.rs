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
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOperator {
    Plus,
    Minus,
}

#[derive(Debug, Clone, Copy,PartialEq, Eq, Hash)]
pub enum Identifier<'a> {
    Ephemeral(u64),
    Named(&'a str)
}

impl<'a> Identifier<'a> {
    pub fn new_ephemeral() -> Identifier<'a> {
        static COUNTER: AtomicU64 = AtomicU64::new(0);
        
        let current_counter = COUNTER.load(Ordering::Relaxed);
        COUNTER.store(current_counter + 1, Ordering::Relaxed);
        Identifier::Ephemeral(current_counter)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'a> {
    Constant(Value),
    BinaryOp(Box<Expr<'a>>, BinaryOperator, Box<Expr<'a>>),
    UnaryOp(UnaryOperator, Box<Expr<'a>>),
    Call(Identifier<'a>, Vec<Expr<'a>>),
    Id(Identifier<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<'a> {
    Assign(Identifier<'a>, Expr<'a>),
    Expr(Expr<'a>)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Module<'a> {
    Body(Vec<Statement<'a>>)
}