use std::collections::HashMap;
use cs4999_compiler::ast::{Identifier, Value, ValueType};

pub mod interpreter;
pub mod type_check;
pub mod x86_interpreter;

type ValueEnv = HashMap<Identifier, Value>;
type TypeEnv = HashMap<Identifier, ValueType>;
