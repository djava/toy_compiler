use crate::{passes::X86Pass, x86_ast::*};

pub struct PatchInstructions;

impl X86Pass for PatchInstructions {
    fn run_pass(m: X86Program) -> X86Program {
        let mut new_instrs = vec![];
        // Reserve for worst case because why not
        new_instrs.reserve(m.instrs.len() * 2);

        for i in m.instrs {
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
                _ => new_instrs.push(i),
            }
        }

        X86Program {
            instrs: new_instrs,
            stack_size: m.stack_size,
        }
    }
}
