use crate::{passes::X86Pass, x86_ast::*};
use std::sync::Arc;

pub struct PatchInstructions;

impl X86Pass for PatchInstructions {
    fn run_pass(self, mut m: X86Program) -> X86Program {
        let main_instrs = &mut m
            .blocks
            .iter_mut()
            .find(|block| block.label == Directive::Label(Identifier::Named(Arc::from("user_entry"))))
            .expect("Didn't find an entry function")
            .instrs;

        let mut new_instrs = vec![];
        // Reserve for worst case because why not
        new_instrs.reserve(main_instrs.len() * 2);

        for i in main_instrs.iter_mut() {
            match &i {
                // If both args to an instr are derefs, we need to add a
                // patch instruction
                Instr::addq(s, d) | Instr::subq(s, d) | Instr::movq(s, d)
                    if matches!(s, Arg::Deref(_, _)) && matches!(d, Arg::Deref(_, _)) =>
                {
                    new_instrs.push(Instr::movq(s.clone(), Arg::Reg(Register::rax)));
                    new_instrs.push(match &i {
                        Instr::addq(_, dest) => Instr::addq(Arg::Reg(Register::rax), dest.clone()),
                        Instr::subq(_, dest) => Instr::subq(Arg::Reg(Register::rax), dest.clone()),
                        Instr::movq(_, dest) => Instr::movq(Arg::Reg(Register::rax), dest.clone()),
                        _ => unreachable!(),
                    });
                }

                // If the instruction has an immediate > 32 bits and
                // also accesses memory, need a patch instr
                Instr::addq(s, d) | Instr::subq(s, d) | Instr::movq(s, d)
                    if matches!(s, Arg::Immediate(v) if i32::try_from(*v).is_err())
                        && matches!(d, Arg::Deref(_, _)) =>
                {
                    new_instrs.push(Instr::movq(s.clone(), Arg::Reg(Register::rax)));
                    new_instrs.push(match &i {
                        Instr::addq(_, dest) => Instr::addq(Arg::Reg(Register::rax), dest.clone()),
                        Instr::subq(_, dest) => Instr::subq(Arg::Reg(Register::rax), dest.clone()),
                        Instr::movq(_, dest) => Instr::movq(Arg::Reg(Register::rax), dest.clone()),
                        _ => unreachable!(),
                    });
                }

                Instr::movq(s, d) if s == d => {
                    // Trival mov to itself, don't keep this instruction
                }

                Instr::cmpq(s, imm @Arg::Immediate(_)) =>{
                    // Second arg of cmpq can't be an immediate
                    new_instrs.push(Instr::movq(imm.clone(), Arg::Reg(Register::rax)));
                    new_instrs.push(Instr::cmpq(s.clone(), Arg::Reg(Register::rax)));
                }

                _ => new_instrs.push(i.clone()),
            }
        }

        *main_instrs = new_instrs;
        m
    }
}
