use crate::{syntax_trees::ast::Module, passes::ASTPass, utils::type_check_ast_statements};

pub struct TypeCheck;

impl ASTPass for TypeCheck {
    fn run_pass(self, mut m: Module) -> Module {
        type_check_ast_statements(&m.body[..], &mut m.types);

        m
    }
}
