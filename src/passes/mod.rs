use super::ast;

pub mod partial_eval;
pub mod remove_complex_operands;

pub trait Pass {
    fn run_pass(m: ast::Module) -> ast::Module;
}