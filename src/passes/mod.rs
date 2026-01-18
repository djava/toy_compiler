use super::ast;

pub mod partial_eval;

pub trait Pass {
    fn run_pass(m: &mut ast::Module);
}