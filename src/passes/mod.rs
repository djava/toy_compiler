use super::ast;
use super::x86_ast;

pub mod partial_eval;
pub mod remove_complex_operands;
pub mod select_instructions;
pub mod register_allocation;

pub trait IRPass {
    fn run_pass(m: ast::Module) -> ast::Module;
}

pub trait IRToX86Pass {
    fn run_pass(m: ast::Module) -> x86_ast::X86Program;
}

pub trait X86Pass {
    fn run_pass(m: x86_ast::X86Program) -> x86_ast::X86Program;
}