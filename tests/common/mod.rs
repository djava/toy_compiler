use std::collections::HashMap;
use cs4999_compiler::ast::{Value, ValueType};

pub mod interpreter;
pub mod type_check;

type ValueEnv = HashMap<String, Value>;
type TypeEnv = HashMap<String, ValueType>;
