use super::ast;
use super::x86_ast;
use enum_dispatch::enum_dispatch;

pub mod partial_eval;
pub use partial_eval::PartialEval;
pub mod patch_instructions;
pub use patch_instructions::PatchInstructions;
pub mod prelude_conclusion;
pub use prelude_conclusion::PreludeConclusion;
pub mod register_allocation;
pub use register_allocation::RegisterAllocation;
pub mod remove_complex_operands;
pub use remove_complex_operands::RemoveComplexOperands;
pub mod select_instructions;
pub use select_instructions::SelectInstructions;

#[enum_dispatch]
pub trait IRPass {
    fn run_pass(self, m: ast::Module) -> ast::Module;
}

#[enum_dispatch(IRPass)]
pub enum IRtoIR {
    PartialEval,
    RemoveComplexOperands,
}

#[enum_dispatch]
pub trait IRToX86Pass {
    fn run_pass(self, m: ast::Module) -> x86_ast::X86Program;
}

#[enum_dispatch(IRToX86Pass)]
pub enum IRtoX86 {
    SelectInstructions,
}

#[enum_dispatch]
pub trait X86Pass {
    fn run_pass(self, m: x86_ast::X86Program) -> x86_ast::X86Program;
}

#[enum_dispatch(X86Pass)]
pub enum X86toX86 {
    RegisterAllocation,
    PatchInstructions,
    PreludeConclusion,
}
