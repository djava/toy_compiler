use std::collections::HashMap;
use cs4999_compiler::ast::{Identifier, Value, ValueType};

pub mod interpreter;
pub mod type_check;

type ValueEnv = HashMap<Identifier, Value>;
type TypeEnv = HashMap<Identifier, ValueType>;
