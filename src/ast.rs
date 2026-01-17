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

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Constant(Value),
    BinaryOp(Box<Expr>, BinaryOperator, Box<Expr>),
    UnaryOp(UnaryOperator, Box<Expr>),
    Call(String, Vec<Expr>),
    Identifier(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Assign(String, Expr),
    Expr(Expr)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Module {
    Body(Vec<Statement>)
}