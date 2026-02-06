use compiler::syntax_trees::shared::*;

pub trait ValueExt {
    fn expect_int(self) -> i64;
    fn expect_bool(self) -> bool;
    fn coerce_int(self) -> i64;
    fn coerce_bool(self) -> bool;
}

impl ValueExt for Value {
    fn expect_int(self) -> i64 {
        if let Value::I64(v) = self {
            v
        } else {
            panic!("{:?} didn't evaluate to a constant int", self);
        }
    }

    fn expect_bool(self) -> bool {
        if let Value::Bool(v) = self {
            v
        } else {
            panic!("{:?} didn't evaluate to a constant bool", self);
        }
    }

    fn coerce_int(self) -> i64 {
        self.into()
    }

    fn coerce_bool(self) -> bool {
        self.into()
    }
}

impl ValueExt for Option<Value> {
    fn expect_int(self) -> i64 {
        if let Some(Value::I64(v)) = self {
            v
        } else {
            panic!("{:?} didn't evaluate to a constant int", self);
        }
    }

    fn expect_bool(self) -> bool {
        if let Some(Value::Bool(v)) = self {
            v
        } else {
            panic!("{:?} didn't evaluate to a constant bool", self);
        }
    }

    fn coerce_int(self) -> i64 {
        if let Some(v) = self {
            v.into()
        } else {
            panic!("No value to coerce to bool");
        }
    }

    fn coerce_bool(self) -> bool {
        if let Some(v) = self {
            v.into()
        } else {
            panic!("No value to coerce to bool");
        }
    }
}

pub trait BinaryOperatorExt {
    fn try_eval(&self, l: &Value, r: &Value) -> Option<Value>;
}

impl BinaryOperatorExt for BinaryOperator {
    fn try_eval(&self, l: &Value, r: &Value) -> Option<Value> {
        use ValueType::*;

        match (ValueType::from(l), ValueType::from(r)) {
            (IntType, IntType) => {
                if let Value::I64(l_val) = l
                    && let Value::I64(r_val) = r
                {
                    match self {
                        BinaryOperator::Add => Some(Value::I64(l_val + r_val)),
                        BinaryOperator::Subtract => Some(Value::I64(l_val - r_val)),
                        BinaryOperator::Multiply => Some(Value::I64(l_val * r_val)),
                        BinaryOperator::Equals => Some(Value::Bool(l_val == r_val)),
                        BinaryOperator::NotEquals => Some(Value::Bool(l_val != r_val)),
                        BinaryOperator::Greater => Some(Value::Bool(l_val > r_val)),
                        BinaryOperator::GreaterEquals => Some(Value::Bool(l_val >= r_val)),
                        BinaryOperator::Less => Some(Value::Bool(l_val < r_val)),
                        BinaryOperator::LessEquals => Some(Value::Bool(l_val <= r_val)),
                        _ => None,
                    }
                } else {
                    panic!("ValueType didnt match Value variant - bug in ValueType::from()?");
                }
            }
            (BoolType, BoolType) => {
                if let Value::Bool(l_val) = l
                    && let Value::Bool(r_val) = r
                {
                    match self {
                        BinaryOperator::And => Some(Value::Bool(*l_val && *r_val)),
                        BinaryOperator::Or => Some(Value::Bool(*l_val || *r_val)),
                        BinaryOperator::Equals => Some(Value::Bool(*l_val == *r_val)),
                        BinaryOperator::NotEquals => Some(Value::Bool(*l_val != *r_val)),
                        _ => None,
                    }
                } else {
                    panic!("ValueType didnt match Value variant - bug in ValueType::from()?");
                }
            }
            _ => None,
        }
    }
}
pub trait UnaryOperatorExt {
    fn try_eval(&self, v: &Value) -> Option<Value>;
}

impl UnaryOperatorExt for UnaryOperator {
    fn try_eval(&self, v: &Value) -> Option<Value> {
        use ValueType::*;

        match ValueType::from(v) {
            IntType => {
                if let Value::I64(val) = v {
                    match self {
                        UnaryOperator::Minus => Some(Value::I64(-val)),
                        UnaryOperator::Plus => Some(Value::I64(*val)),
                        _ => None,
                    }
                } else {
                    panic!("ValueType didnt match Value variant - bug in ValueType::from()?");
                }
            }
            BoolType => {
                if let Value::Bool(val) = v {
                    match self {
                        UnaryOperator::Not => Some(Value::Bool(!val)),
                        _ => None,
                    }
                } else {
                    panic!("ValueType didnt match Value variant - bug in ValueType::from()?");
                }
            }
            _ => None,
        }
    }
}

macro_rules! id {
    ($name:expr) => {
        compiler::syntax_trees::shared::Identifier::Named(std::sync::Arc::from($name))
    };
}
pub(crate) use id;

macro_rules! label {
    ($name:expr) => {
        compiler::syntax_trees::x86::Directive::Label(compiler::syntax_trees::shared::Identifier::Named(
            std::sync::Arc::from($name),
        ))
    };
}
pub(crate) use label;