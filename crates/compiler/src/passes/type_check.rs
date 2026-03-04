//! `TypeCheck` Pass
//! 
//! Runs the type checker, ensures that the types of every expression is
//! correct, and populates the type of every identifier into the type
//! envs, for the locals of each function in the program and also in
//! global_types for the whole program
//!
//! It is mandatory to run this pass, both before and after
//! `ClosurizeFunctions`
//!
//! Pre-conditions:
//! - `GlobalizeIdentifiers`
//! 
//! Post-conditions:
//! - All the `TypeEnv` members within the `Program` have been populated
//! - The `Program` is guaranteed to be type-correct

use crate::{passes::ASTPass, syntax_trees::ast::Program};

#[derive(Debug)]
pub struct TypeCheck;

impl ASTPass for TypeCheck {
    fn run_pass(self, mut m: Program) -> Program {
        m.type_check();

        m
    }
}
