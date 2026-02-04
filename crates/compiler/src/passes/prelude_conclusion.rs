use std::sync::Arc;

use crate::{passes::X86Pass, x86_ast::*};

pub struct PreludeConclusion;

const GC_STACK_SIZE: i64 = 0x8000;
const GC_HEAP_SIZE: i64 = 0x8000;

impl X86Pass for PreludeConclusion {
    fn run_pass(self, mut m: X86Program) -> X86Program {
        let prelude_directives: [Directive; 2] = [
            Directive::AttSyntax,
            Directive::Globl(Identifier::from("main")),
        ];

        let mut prelude_instrs = vec![
            // Initialize stack
            Instr::pushq(Arg::Reg(Register::rbp)),
            Instr::movq(Arg::Reg(Register::rsp), Arg::Reg(Register::rbp)),
            Instr::subq(Arg::Immediate(m.stack_size as _), Arg::Reg(Register::rsp)),

            // Initialize GC Stack/Heap
            Instr::movq(Arg::Immediate(GC_STACK_SIZE), Arg::Reg(Register::rdi)),
            Instr::movq(Arg::Immediate(GC_HEAP_SIZE), Arg::Reg(Register::rsi)),
            Instr::callq(Identifier::from("__gc_initialize"), 2),
            Instr::movq(Arg::Global(Arc::from("__gc_rootstack_begin")), Arg::Reg(Register::r15)),
        ];

        // Zero out GC Stack
        prelude_instrs.extend((0..(m.gc_stack_size / 8) as _).map(|i| {
            Instr::movq(Arg::Immediate(0), Arg::Deref(Register::r15, 8 * i))
        }));
        // Allocate space for GC stack
        prelude_instrs.push(Instr::addq(Arg::Immediate(m.gc_stack_size as _), Arg::Reg(Register::r15)));

        // Jump to user program
        prelude_instrs.push(Instr::jmp(Identifier::from("user_entry")));

        let conclusion_instrs = vec![
            Instr::subq(Arg::Immediate(m.gc_stack_size as _), Arg::Reg(Register::r15)),
            Instr::addq(Arg::Immediate(m.stack_size as _), Arg::Reg(Register::rsp)),
            Instr::popq(Arg::Reg(Register::rbp)),
            Instr::retq,
        ];

        m.header = Vec::from(prelude_directives);

        let main_block = Block {
            label: Directive::Label(Identifier::from("main")),
            instrs: prelude_instrs,
        };

        let exit_block = Block {
            label: Directive::Label(Identifier::from("exit")),
            instrs: conclusion_instrs,
        };

        m.blocks.push(main_block);
        m.blocks.push(exit_block);

        m
    }
}
