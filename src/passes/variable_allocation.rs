use std::collections::HashMap;

use crate::{ast::Identifier, passes::X86Pass, x86_ast};
use x86_ast::*;

pub struct VariableAllocation;

impl X86Pass for VariableAllocation {
    fn run_pass(self, mut m: X86Program) -> X86Program {
        let mut curr_offset = 0i32;
        let mut var_map = HashMap::<Identifier, i32>::new();

        let main_instrs = &mut m
            .functions
            .iter_mut()
            .find(|(d, _)| d == &Directive::Label("main"))
            .expect("Didn't find a main function")
            .1;

        for i in main_instrs {
            match i {
                Instr::addq(s, d) | Instr::subq(s, d) | Instr::movq(s, d) => {
                    for arg in [s, d] {
                        stack_allocate(arg, &mut curr_offset, &mut var_map);
                    }
                }
                Instr::negq(a) | Instr::pushq(a) | Instr::popq(a) => {
                    stack_allocate(a, &mut curr_offset, &mut var_map);
                }
                _ => {}
            }
        }

        // Offset is negative because stack grows down. Negate it to get
        // the stack size.
        m.stack_size = (-curr_offset) as _;

        m
    }
}

fn stack_allocate<'a>(arg: &mut Arg<'a>, curr_offset: &mut i32, var_map: &mut HashMap<Identifier<'a>, i32>) {
    if let Arg::Variable(id) = arg {
        if !var_map.contains_key(&id) {
            // If the variable hasn't been allocated yet, bump offset by
            // 8 and add it to the map.
            *curr_offset -= 8;

            var_map.insert(*id, *curr_offset);
        }

        *arg = Arg::Deref(Register::rbp, *var_map.get(&id).unwrap());
    }
}
