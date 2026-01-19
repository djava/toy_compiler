use crate::{passes::X86Pass, x86_ast::*};

pub struct PreludeConclusion;

impl X86Pass for PreludeConclusion {
    fn run_pass(m: X86Program) -> X86Program {
        const PRELUDE: [Instr; 2] = [
            Instr::pushq(Arg::Reg(Register::rbp)),
            Instr::movq(Arg::Reg(Register::rsp), Arg::Reg(Register::rbp)),
        ];
        const CONCLUSION: [Instr; 3] = [
            Instr::movq(Arg::Deref(Register::rbp, -48), Arg::Reg(Register::rax)),
            Instr::popq(Arg::Reg(Register::rbp)),
            Instr::retq
        ];

        let mut instrs = vec![];
        instrs.extend(PRELUDE);
        instrs.push(Instr::subq(Arg::Immediate(m.stack_size as _), Arg::Reg(Register::rsp)));
        instrs.extend(m.instrs);
        instrs.push(Instr::addq(Arg::Immediate(m.stack_size as _), Arg::Reg(Register::rsp)));
        instrs.extend(CONCLUSION);
        
        X86Program { instrs, stack_size: m.stack_size }
    }
}