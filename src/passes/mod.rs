use crate::ir;

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
pub mod translate_ir_to_x86;
pub use translate_ir_to_x86::TranslateIRtoX86;
pub mod translate_ast_to_ir;
pub use translate_ast_to_ir::TranslateASTtoIR;
pub mod short_circuiting;
pub use short_circuiting::ShortCircuiting;
pub mod remove_jumps;
pub use remove_jumps::RemoveJumps;

#[enum_dispatch]
pub trait ASTPass {
    fn run_pass(self, m: ast::Module) -> ast::Module;
}

#[enum_dispatch(ASTPass)]
pub enum ASTtoAST {
    ShortCircuiting,
    PartialEval,
    RemoveComplexOperands,
}

#[enum_dispatch]
pub trait ASTtoIRPass {
    fn run_pass(self, m: ast::Module) -> ir::IRProgram;
}

#[enum_dispatch(ASTtoIRPass)]
pub enum ASTtoIR {
    TranslateASTtoIR
}

#[enum_dispatch]
pub trait IRtoIRPass {
    fn run_pass(self, p: ir::IRProgram) -> ir::IRProgram;
}

#[enum_dispatch(IRtoIRPass)]
pub enum IRtoIR {

}

#[enum_dispatch]
pub trait IRtoX86Pass {
    fn run_pass(self, m: ir::IRProgram) -> x86_ast::X86Program;
}

#[enum_dispatch(IRtoX86Pass)]
pub enum IRtoX86 {
    TranslateIRtoX86,
}

#[enum_dispatch]
pub trait X86Pass {
    fn run_pass(self, m: x86_ast::X86Program) -> x86_ast::X86Program;
}

#[enum_dispatch(X86Pass)]
pub enum X86toX86 {
    RegisterAllocation,
    RemoveJumps,
    PatchInstructions,
    PreludeConclusion,
}
