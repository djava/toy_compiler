#![allow(dead_code)]

use cs4999_compiler::ast::{Identifier, Value, ValueType};
use std::collections::HashMap;

pub mod ast_interpreter;
pub mod ir_interpreter;
pub mod ast_type_check;
pub mod x86_interpreter;

mod interpreter_utils;

type ValueEnv = HashMap<Identifier, Value>;
type TypeEnv = HashMap<Identifier, ValueType>;
