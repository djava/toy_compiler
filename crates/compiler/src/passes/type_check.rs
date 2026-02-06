use crate::{syntax_trees::ast::Program, passes::ASTPass};

pub struct TypeCheck;

impl ASTPass for TypeCheck {
    fn run_pass(self, mut m: Program) -> Program {
        m.type_check();

        m
    }
}
