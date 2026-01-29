use crate::{passes::X86Pass, x86_ast::*};
use std::sync::Arc;

pub struct PreludeConclusion;

impl X86Pass for PreludeConclusion {
    fn run_pass(self, mut m: X86Program) -> X86Program {
        let prelude_directives: [Directive; 2] = [
            Directive::AttSyntax,
            Directive::Globl(Identifier::Named(Arc::from("main"))),
        ];

        let prelude_instrs: [Instr; 4] = [
            Instr::pushq(Arg::Reg(Register::rbp)),
            Instr::movq(Arg::Reg(Register::rsp), Arg::Reg(Register::rbp)),
            Instr::subq(Arg::Immediate(m.stack_size as _), Arg::Reg(Register::rsp)),
            Instr::jmp(Identifier::Named(Arc::from("user_entry")))
        ];
        let conclusion_instrs: [Instr; 3] = [
            Instr::addq(Arg::Immediate(m.stack_size as _), Arg::Reg(Register::rsp)),
            Instr::popq(Arg::Reg(Register::rbp)),
            Instr::retq,
        ];

        m.header = Vec::from(prelude_directives);

        let main_block = Block {
            label: Directive::Label(Identifier::Named(Arc::from("main"))),
            instrs: prelude_instrs.to_vec(),
        };

        let exit_block = Block {
            label: Directive::Label(Identifier::Named(Arc::from("exit"))),
            instrs: conclusion_instrs.to_vec()
        };

        m.blocks.push(main_block);
        m.blocks.push(exit_block);

        m
    }
}
