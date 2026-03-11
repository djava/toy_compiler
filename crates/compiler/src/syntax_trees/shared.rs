use bitfield_struct::bitfield;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use crate::constants::POINTER_SIZE;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Identifier {
    Ephemeral(u64),
    Global(Arc<str>),
    Local(Arc<str>, Box<Identifier>),
}

impl Identifier {
    pub fn new_ephemeral() -> Identifier {
        static COUNTER: Mutex<u64> = Mutex::new(0);

        let id = {
            let mut c = COUNTER.lock().unwrap();
            let id = *c;
            *c += 1;
            id
        };

        Identifier::Ephemeral(id)
    }

    pub fn new_lambda_name() -> Identifier {
        static COUNTER: Mutex<u64> = Mutex::new(0);

        let id = {
            let mut c = COUNTER.lock().unwrap();
            let id = *c;
            *c += 1;
            id
        };

        Identifier::Global(Arc::from(format!("__LAMBDA_{id}")))
    }
}

impl From<&str> for Identifier {
    fn from(value: &str) -> Self {
        Identifier::Global(Arc::from(value))
    }
}

pub type TypeEnv = HashMap<Identifier, ValueType>;

#[derive(Debug, Clone)]
pub enum ValueType {
    IntType,
    FunctionType(Vec<ValueType>, Box<ValueType>),
    BoolType,
    TupleType(Vec<ValueType>),
    ArrayType(Box<ValueType>, usize),
    PointerType(Box<ValueType>),
    NoneType,
    Indeterminate,
}

impl ValueType {
    pub fn size(&self) -> usize {
        match self {
            ValueType::IntType => 8,
            ValueType::FunctionType(_, _) => POINTER_SIZE as _,
            ValueType::BoolType => 1,
            ValueType::TupleType(_) => POINTER_SIZE as _,
            ValueType::ArrayType(_, _) => POINTER_SIZE as _,
            ValueType::PointerType(_) => POINTER_SIZE as _,
            ValueType::NoneType => 0,
            ValueType::Indeterminate => panic!("Size of indeterminate value type"),
        }
    }
}

impl From<&Value> for ValueType {
    fn from(value: &Value) -> Self {
        match value {
            Value::I64(_) => Self::IntType,
            Value::Bool(_) => Self::BoolType,
            Value::Tuple(elems) => Self::TupleType(elems.iter().map(ValueType::from).collect()),
            Value::Array(elems) => Self::ArrayType(
                Box::new(
                    elems
                        .get(0)
                        .map(ValueType::from)
                        .unwrap_or(ValueType::Indeterminate),
                ),
                elems.len(),
            ),
            Value::Function(_, arg_types, ret_type) => {
                Self::FunctionType(arg_types.clone(), Box::new(ret_type.clone()))
            }
            Value::None => Self::NoneType,
        }
    }
}

impl PartialEq for ValueType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::TupleType(l_elems), Self::TupleType(r_elems)) => {
                if let Some(ValueType::FunctionType(l_args, l_ret)) = l_elems.get(0)
                    && let Some(ValueType::TupleType(_)) = l_args.get(0)
                    && let Some(ValueType::FunctionType(r_args, r_ret)) = r_elems.get(0)
                    && let Some(ValueType::TupleType(_)) = r_args.get(0)
                {
                    // This is a closure
                    l_args[1..] == r_args[1..] && l_ret == r_ret
                } else {
                    l_elems == r_elems
                }
            }
            (Self::TupleType(clos_elems), Self::FunctionType(args, ret))
            | (Self::FunctionType(args, ret), Self::TupleType(clos_elems)) => {
                if let Some(ValueType::FunctionType(clos_args, clos_ret)) = clos_elems.get(0)
                    && let Some(ValueType::TupleType(_)) = clos_args.get(0)
                {
                    clos_args == args && clos_ret == ret
                } else {
                    false
                }
            }
            (Self::FunctionType(l0, l1), Self::FunctionType(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::PointerType(l0), Self::PointerType(r0)) => l0 == r0,
            (Self::ArrayType(typ, len), Self::ArrayType(typ1, len1)) => typ == typ1 && len == len1,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    I64(i64),
    Bool(bool),
    Tuple(Vec<Value>),
    Array(Vec<Value>),
    Function(Identifier, Vec<ValueType>, ValueType),
    None,
}

impl Into<i64> for Value {
    fn into(self) -> i64 {
        match self {
            Value::I64(val) => val,
            Value::Bool(val) => val as _,
            Value::Tuple(_) => panic!("Cannot convert Value::Tuple into i64"),
            Value::Array(_) => panic!("Cannot convert Value::Array into i64"),
            Value::Function(_, _, _) => panic!("Cannot convert Value::Function into i64"),
            Value::None => panic!("Cannot convert Value::None into i64"),
        }
    }
}

impl Into<bool> for Value {
    fn into(self) -> bool {
        match self {
            Value::I64(val) => val != 0,
            Value::Bool(val) => val,
            Value::Tuple(elems) | Value::Array(elems) => !elems.is_empty(),
            Value::Function(_, _, _) => panic!("Cannot convert Value::Function into bool"),
            Value::None => panic!("Cannot convert Value::None into bool"),
        }
    }
}

impl Into<bool> for &Value {
    fn into(self) -> bool {
        match self {
            Value::I64(val) => *val != 0,
            Value::Bool(val) => *val,
            Value::Tuple(elems) | Value::Array(elems) => !elems.is_empty(),
            Value::Function(_, _, _) => panic!("Cannot convert Value::Function into bool"),
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
            Value::Array(_) => panic!("Cannot convert Value::Array into i64"),
            Value::Function(_, _, _) => panic!("Cannot convert Value::Function into i64"),
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
    Multiply,
    LeftShift,
    RightShift,
    Divide,
}

impl BinaryOperator {
    pub fn type_of(&self, l: &ValueType, r: &ValueType) -> Option<ValueType> {
        use BinaryOperator::*;
        use ValueType::*;

        match (l, r) {
            (IntType, IntType) => match self {
                Add => Some(IntType),
                Subtract => Some(IntType),
                Multiply => Some(IntType),
                LeftShift => Some(IntType),
                RightShift => Some(IntType),
                Divide => Some(IntType),
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
                _ => None,
            },
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
pub struct ComplexSubscript<E> {
    pub container: E,
    pub index: E,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AssignDest<E> {
    Id(Identifier),
    Subscript(Identifier, i64),
    ComplexSubscript(ComplexSubscript<E>)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SizedAssignDest<E> {
    pub value: AssignDest<E>,
    pub size: usize
}

#[bitfield(u64, order = Lsb)]
pub struct TupleTag {
    pub forwarding: bool,
    #[bits(6)]
    pub length: u8,
    #[bits(50)]
    pub pointer_mask: u64,
    #[bits(5)]
    __: u8,
    #[bits(1, default = false)]
    pub is_array: bool,
    #[bits(1)]
    __: u8,
}

#[bitfield(u64, order = Lsb)]
pub struct ArrayTag {
    pub forwarding: bool,
    pub pointer_mask: bool,
    #[bits(3)]
    pub elem_size: usize,
    #[bits(57)]
    pub length: u64,
    #[bits(1, default = true)]
    pub is_array: bool,
    #[bits(1)]
    __: u8,
}

impl From<i64> for ArrayTag {
    fn from(value: i64) -> Self {
        let bits: u64 = u64::from_le_bytes(value.to_le_bytes());
        Self::from_bits(bits)
    }
}