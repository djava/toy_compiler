use std::collections::HashMap;

use crate::{ast::Identifier, passes::X86Pass, x86_ast};
use x86_ast::*;

pub struct RegisterAllocation;

impl X86Pass for RegisterAllocation {
    fn run_pass(mut m: X86Program) -> X86Program {
        let mut curr_offset = 0i32;
        let mut var_map = HashMap::<Identifier, i32>::new();

        for i in &mut m.instrs {
            match i {
                Instr::addq(s, d) | Instr::subq(s, d) | Instr::movq(s, d) => {
                    for arg in [s, d] {
                        stack_allocate(arg, &mut curr_offset, &mut var_map);
                    }
                },
                Instr::negq(a) | Instr::pushq(a) | Instr::popq(a) => {
                    stack_allocate(a, &mut curr_offset, &mut var_map);
                },
                _ => {}
            }
        }

        // Offset is negative because stack grows down. Negate it to get
        // the stack size.
        m.stack_size = (-curr_offset) as _;

        m
    }
}

fn stack_allocate(arg: &mut Arg, curr_offset: &mut i32, var_map: &mut HashMap<Identifier, i32>) {
    if let Arg::Variable(id) = arg {
        if !var_map.contains_key(&id) {
            // If the variable hasn't been allocated yet, bump offset by
            // 8 and add it to the map.
            *curr_offset -= 8;

            var_map.insert(id.clone(), *curr_offset);
        }

        *arg = Arg::Deref(Register::rbp, *var_map.get(&id).unwrap());
    }
}
