use crate::{syntax_trees::*, passes::*};

pub struct Pipeline {
    pub ast_passes: Vec<ASTtoAST>,
    pub ast_to_ir_pass: ASTtoIR,
    pub ir_passes: Vec<IRtoIR>,
    pub ir_to_x86_pass: IRtoX86,
    pub x86_passes: Vec<X86toX86>,
}

impl Pipeline {
    pub fn run(self, program: ast::Module) -> x86::X86Program {
        let final_ast = self
            .ast_passes
            .into_iter()
            .fold(program, |p, pass| pass.run_pass(p));

        let initial_ir = self.ast_to_ir_pass.run_pass(final_ast);

        let final_ir = self
            .ir_passes
            .into_iter()
            .fold(initial_ir, |p, pass| pass.run_pass(p));

        let initial_x86 = self.ir_to_x86_pass.run_pass(final_ir);

        let final_x86 = self
            .x86_passes
            .into_iter()
            .fold(initial_x86, |p, pass| pass.run_pass(p));

        final_x86
    }

    pub fn run_ast_only(self, program: ast::Module) -> ast::Module {
        self.ast_passes
            .into_iter()
            .fold(program, |p, pass| pass.run_pass(p))
    }

    pub fn run_up_to_ir_only(self, program: ast::Module) -> ir::IRProgram {
        let final_ast = self
            .ast_passes
            .into_iter()
            .fold(program, |p, pass| pass.run_pass(p));
        self.ast_to_ir_pass.run_pass(final_ast)
    }

    pub fn run_x86_only(self, program: x86::X86Program) -> x86::X86Program {
        self.x86_passes
            .into_iter()
            .fold(program, |p, pass| pass.run_pass(p))
    }

    pub fn make_opt() -> Self {
        Self {
            ast_passes: vec![
                ASTtoAST::from(TypeCheck),
                ASTtoAST::from(ShortCircuiting),
                ASTtoAST::from(PartialEval),
                ASTtoAST::from(RemoveComplexOperands),
                ASTtoAST::from(TypeCheck),
                ASTtoAST::from(InjectAllocations),
            ],
            ast_to_ir_pass: ASTtoIR::from(TranslateASTtoIR),
            ir_passes: vec![],
            ir_to_x86_pass: IRtoX86::from(TranslateIRtoX86),
            x86_passes: vec![
                X86toX86::from(RegisterAllocation),
                X86toX86::from(RemoveJumps),
                X86toX86::from(PatchInstructions),
                X86toX86::from(PreludeConclusion),
            ],
        }
    }

    pub fn make_no_opt() -> Self {
        Self {
            ast_passes: vec![
                ASTtoAST::from(TypeCheck),
                ASTtoAST::from(ShortCircuiting),
                ASTtoAST::from(RemoveComplexOperands),
                ASTtoAST::from(TypeCheck),
                ASTtoAST::from(InjectAllocations),
            ],
            ast_to_ir_pass: ASTtoIR::from(TranslateASTtoIR),
            ir_passes: vec![],
            ir_to_x86_pass: IRtoX86::from(TranslateIRtoX86),
            x86_passes: vec![
                X86toX86::from(RegisterAllocation),
                X86toX86::from(PatchInstructions),
                X86toX86::from(PreludeConclusion),
            ],
        }
    }
}
