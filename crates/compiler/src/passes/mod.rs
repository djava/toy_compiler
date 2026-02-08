use crate::syntax_trees::*;
use enum_dispatch::enum_dispatch;

mod partial_eval;
pub use partial_eval::PartialEval;
mod patch_instructions;
pub use patch_instructions::PatchInstructions;
mod prelude_conclusion;
pub use prelude_conclusion::PreludeConclusion;
mod register_allocation;
pub use register_allocation::RegisterAllocation;
mod remove_complex_operands;
pub use remove_complex_operands::RemoveComplexOperands;
mod translate_ir_to_x86;
pub use translate_ir_to_x86::TranslateIRtoX86;
mod translate_ast_to_ir;
pub use translate_ast_to_ir::TranslateASTtoIR;
mod short_circuiting;
pub use short_circuiting::ShortCircuiting;
mod remove_jumps;
pub use remove_jumps::RemoveJumps;
mod type_check;
pub use type_check::TypeCheck;
mod inject_allocations;
pub use inject_allocations::InjectAllocations;
mod tupleize_excess_args;
pub use tupleize_excess_args::TupleizeExcessArgs;

#[enum_dispatch]
pub trait ASTPass {
    fn run_pass(self, m: ast::Program) -> ast::Program;
}

#[enum_dispatch(ASTPass)]
pub enum ASTtoAST {
    TypeCheck,
    ShortCircuiting,
    PartialEval,
    RemoveComplexOperands,
    InjectAllocations,
    TupleizeExcessArgs,
}

#[enum_dispatch]
pub trait ASTtoIRPass {
    fn run_pass(self, m: ast::Program) -> ir::IRProgram;
}

#[enum_dispatch(ASTtoIRPass)]
pub enum ASTtoIR {
    TranslateASTtoIR,
}

#[enum_dispatch]
pub trait IRtoIRPass {
    fn run_pass(self, p: ir::IRProgram) -> ir::IRProgram;
}

#[enum_dispatch(IRtoIRPass)]
pub enum IRtoIR {}

#[enum_dispatch]
pub trait IRtoX86Pass {
    fn run_pass(self, m: ir::IRProgram) -> x86::X86Program;
}

#[enum_dispatch(IRtoX86Pass)]
pub enum IRtoX86 {
    TranslateIRtoX86,
}

#[enum_dispatch]
pub trait X86Pass {
    fn run_pass(self, m: x86::X86Program) -> x86::X86Program;
}

#[enum_dispatch(X86Pass)]
pub enum X86toX86 {
    RegisterAllocation,
    RemoveJumps,
    PatchInstructions,
    PreludeConclusion,
}
