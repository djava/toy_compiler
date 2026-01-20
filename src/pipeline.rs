use crate::{ast, passes::*, x86_ast};

pub struct Pipeline {
    pub ir_passes: Vec<IRtoIR>,
    pub ir_to_x86_pass: IRtoX86,
    pub x86_passes: Vec<X86toX86>,
}

impl Pipeline {
    pub fn run(self, program: ast::Module) -> x86_ast::X86Program {
        let final_ir = self
            .ir_passes
            .into_iter()
            .fold(program, |p, pass| pass.run_pass(p));

        let initial_x86 = self.ir_to_x86_pass.run_pass(final_ir);

        let final_x86 = self
            .x86_passes
            .into_iter()
            .fold(initial_x86, |p, pass| pass.run_pass(p));

        final_x86
    }

    pub fn run_ir_only(self, program: ast::Module) -> ast::Module {
        self.ir_passes
            .into_iter()
            .fold(program, |p, pass| pass.run_pass(p))
    }

    pub fn run_x86_only(self, program: x86_ast::X86Program) -> x86_ast::X86Program {
        self.x86_passes
            .into_iter()
            .fold(program, |p, pass| pass.run_pass(p))
    }

    pub fn make_full() -> Self {
        Self {
            ir_passes: vec![
                IRtoIR::from(PartialEval),
                IRtoIR::from(RemoveComplexOperands),
            ],
            ir_to_x86_pass: IRtoX86::from(SelectInstructions),
            x86_passes: vec![
                X86toX86::from(VariableAllocation),
                X86toX86::from(PatchInstructions),
                X86toX86::from(PreludeConclusion),
            ],
        }
    }

    pub fn make_no_opt() -> Self {
        Self {
            ir_passes: vec![IRtoIR::from(RemoveComplexOperands)],
            ir_to_x86_pass: IRtoX86::from(SelectInstructions),
            x86_passes: vec![
                X86toX86::from(VariableAllocation),
                X86toX86::from(PatchInstructions),
                X86toX86::from(PreludeConclusion),
            ],
        }
    }
}
