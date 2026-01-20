#![allow(dead_code)]

use cs4999_compiler::ast::{Identifier, Value, ValueType};
use std::collections::HashMap;

pub mod interpreter;
pub mod type_check;
pub mod x86_interpreter;

type ValueEnv<'a> = HashMap<Identifier<'a>, Value>;
type TypeEnv<'a> = HashMap<Identifier<'a>, ValueType>;
