use crate::{passes::X86Pass, x86_ast::*};

pub struct PreludeConclusion;

impl X86Pass for PreludeConclusion {
    fn run_pass(mut m: X86Program) -> X86Program {
        let prelude_directives: [Directive; 2] = [
            Directive::AttSyntax,
            Directive::Globl(String::from("main")),
        ];

        let prelude_instrs: [Instr; 3] = [
            Instr::subq(Arg::Immediate(m.stack_size as _), Arg::Reg(Register::rsp)),
            Instr::pushq(Arg::Reg(Register::rbp)),
            Instr::movq(Arg::Reg(Register::rsp), Arg::Reg(Register::rbp)),
        ];
        let conclusion_instrs: [Instr; 4] = [
            Instr::addq(Arg::Immediate(m.stack_size as _), Arg::Reg(Register::rsp)),
            Instr::movq(Arg::Deref(Register::rbp, -48), Arg::Reg(Register::rax)),
            Instr::popq(Arg::Reg(Register::rbp)),
            Instr::retq,
        ];

        for (idx, d) in prelude_directives.iter().enumerate() {
            m.functions.insert(idx, (d.clone(), vec![]));
        }

        let main_func = m
            .functions
            .iter_mut()
            .find(|x| x.0 == Directive::Label(String::from("main")))
            .expect("No main function found");

        let mut wrapped_main_instrs: Vec<Instr> = vec![];
        wrapped_main_instrs.extend(prelude_instrs);
        wrapped_main_instrs.extend(main_func.1.clone());
        wrapped_main_instrs.extend(conclusion_instrs);
        main_func.1 = wrapped_main_instrs;

        m
    }
}
