use std::collections::HashMap;
use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};

pub type TypeEnv = HashMap<AssignDest, ValueType>;

#[derive(Debug, Clone, PartialEq)]
pub enum ValueType {
    IntType,
    FunctionType(Vec<ValueType>),
    BoolType,
    TupleType(Vec<ValueType>),
    PointerType(Box<ValueType>),
    NoneType,
}

impl From<&Value> for ValueType {
    fn from(value: &Value) -> Self {
        match value {
            Value::I64(_) => Self::IntType,
            Value::Bool(_) => Self::BoolType,
            Value::Tuple(elems) => Self::TupleType(elems.iter().map(ValueType::from).collect()),
            Value::None => Self::NoneType,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    I64(i64),
    Bool(bool),
    Tuple(Vec<Value>),
    None,
}

impl Into<i64> for Value {
    fn into(self) -> i64 {
        match self {
            Value::I64(val) => val,
            Value::Bool(val) => val as _,
            Value::Tuple(_) => panic!("Cannot convert Value::Pointer into i64"),
            Value::None => panic!("Cannot convert Value::None into i64"),
        }
    }
}

impl Into<bool> for Value {
    fn into(self) -> bool {
        match self {
            Value::I64(val) => val != 0,
            Value::Bool(val) => val,
            Value::Tuple(elems) => !elems.is_empty(),
            Value::None => panic!("Cannot convert Value::None into bool"),
        }
    }
}

impl Into<bool> for &Value {
    fn into(self) -> bool {
        match self {
            Value::I64(val) => *val != 0,
            Value::Bool(val) => *val,
            Value::Tuple(elems) => !elems.is_empty(),
            Value::None => panic!("Cannot convert Value::None into bool"),
        }
    }
}

impl From<&Value> for i64 {
    fn from(value: &Value) -> Self {
        match value {
            Value::I64(val) => *val,
            Value::Bool(val) => *val as _,
            Value::Tuple(_) => panic!("Cannot convert Value::Tuple into i64"),
            Value::None => panic!("Cannot convert Value::None into i64"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOperator {
    Add,
    Subtract,
    And,
    Or,
    Equals,
    NotEquals,
    Greater,
    GreaterEquals,
    Less,
    LessEquals,
    Is,
}

impl BinaryOperator {
    pub fn type_of(&self, l: &ValueType, r: &ValueType) -> Option<ValueType> {
        use BinaryOperator::*;
        use ValueType::*;

        match (l, r) {
            (IntType, IntType) => match self {
                Add => Some(IntType),
                Subtract => Some(IntType),
                Equals => Some(BoolType),
                NotEquals => Some(BoolType),
                Greater => Some(BoolType),
                GreaterEquals => Some(BoolType),
                Less => Some(BoolType),
                LessEquals => Some(BoolType),
                _ => None,
            },
            (BoolType, BoolType) => match self {
                And => Some(BoolType),
                Or => Some(BoolType),
                Equals => Some(BoolType),
                NotEquals => Some(BoolType),
                _ => None,
            },
            (TupleType(_), TupleType(_)) => match self {
                Equals => Some(BoolType),
                NotEquals => Some(BoolType),
                Is => Some(BoolType),
                _ => None
            }
            (_, _) => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOperator {
    Plus,
    Minus,
    Not,
}

impl UnaryOperator {
    pub fn type_of(&self, v: &ValueType) -> Option<ValueType> {
        use UnaryOperator::*;
        use ValueType::*;

        match v {
            IntType => match self {
                Plus => Some(IntType),
                Minus => Some(IntType),
                _ => None,
            },
            BoolType => match self {
                Not => Some(BoolType),
                _ => None,
            },
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Identifier {
    Ephemeral(u64),
    Named(Arc<str>),
}

impl Identifier {
    pub fn new_ephemeral() -> Identifier {
        static COUNTER: AtomicU64 = AtomicU64::new(0);

        let current_counter = COUNTER.load(Ordering::Relaxed);
        COUNTER.store(current_counter + 1, Ordering::Relaxed);
        Identifier::Ephemeral(current_counter)
    }
}

impl From<&str> for Identifier {
    fn from(value: &str) -> Self {
        Identifier::Named(Arc::from(value))
    }
}

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
    GlobalSymbol(Arc<str>)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AssignDest {
    Id(Identifier),
    Subscript(Identifier, i64)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Assign(AssignDest, Expr),
    Expr(Expr),
    Conditional(Expr, Vec<Statement>, Vec<Statement>),
    WhileLoop(Expr, Vec<Statement>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub body: Vec<Statement>,
    pub types: TypeEnv,
}
