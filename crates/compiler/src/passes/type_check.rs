use crate::{syntax_trees::ast::Module, passes::ASTPass};

pub struct TypeCheck;

impl ASTPass for TypeCheck {
    fn run_pass(self, mut m: Module) -> Module {
        m.type_check();

        m
    }
}
