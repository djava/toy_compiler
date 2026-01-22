use std::{collections::HashSet, hash::Hash};

use crate::{ast::Identifier, x86_ast::*};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
enum Location<'a> {
    Id(Identifier<'a>),
    Reg(Register),
}

impl<'a> Location<'a> {
    fn try_from_arg(arg: &'a Arg) -> Option<Self> {
        match arg {
            Arg::Deref(reg, _) => Some(Location::Reg(*reg)),
            Arg::Variable(id) => Some(Location::Id(*id)),
            Arg::Reg(reg) => Some(Location::Reg(*reg)),
            Arg::Immediate(_) => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct LivenessMap<'a> {
    instrs: &'a [&'a Instr<'a>],
    alive_befores: Vec<HashSet<Location<'a>>>,
}

impl<'a> LivenessMap<'a> {
    pub fn from_instrs(instrs: &'a [&'a Instr<'a>]) -> Self {
        let alive_befores = LivenessMap::make_alive_befores(instrs);

        Self { instrs, alive_befores }
    }

    fn make_alive_befores(instrs: &'a [&'a Instr<'a>]) -> Vec<HashSet<Location<'a>>> {
        // Implements the textbook's equation for liveness analysis:
        //      `L_before(k) = (L_after(k) - W(k)) union R(k)`
        //
        // Reverse the list of instructions, then go one-by-one and
        // remove writtten ones and add read ones. Iter::scan() carries
        // the previous iteration's state into the next one, so we
        // always start with the L_before(k) and do the set operations
        // to get the L_after based on locs_written(i) and locs_read(i)
        let mut alive_befores: Vec<_> = instrs
            .iter()
            .rev()
            .scan(HashSet::new(), |alive_after, instr| {
                let mut alive_before = alive_after.clone();
                let written = locs_written(*instr);
                alive_before.extract_if(|l| written.contains(l));
                alive_before.extend(&locs_read(*instr));
                Some(alive_before)
            })
            .collect();

        alive_befores.reverse();
        alive_befores
    }
}

fn locs_read<'a>(i: &'a Instr) -> Vec<Location<'a>> {
    let mut locations = Vec::new();
    match i {
        // These ones read from both s and d
        Instr::addq(s, d) | Instr::subq(s, d) => {
            locations = [s, d]
                .into_iter()
                .filter_map(Location::try_from_arg)
                .collect();
        }

        // These ones each read from one arg
        Instr::negq(r) | Instr::movq(r, _) | Instr::pushq(r) => {
            if let Some(loc) = Location::try_from_arg(r) {
                locations.push(loc);
            }
        }

        // A function call reads from as many variables as are being
        // passed
        Instr::callq(_, num_args) => {
            const NUM_ARG_REGISTERS: u16 = 6;
            const ARG_REGISTERS: [Register; NUM_ARG_REGISTERS as _] = [
                Register::rdi,
                Register::rsi,
                Register::rdx,
                Register::rcx,
                Register::r8,
                Register::r9,
            ];

            if *num_args >= NUM_ARG_REGISTERS {
                unimplemented!("Spilling args onto stack not implemented");
            }

            locations.extend(
                ARG_REGISTERS
                    .iter()
                    .take(*num_args as _)
                    .map(|r| Location::Reg(*r)),
            );
        }

        Instr::popq(_) | Instr::retq => {} // These ones never read variables
    };

    locations
}

fn locs_written<'a>(i: &'a Instr) -> Vec<Location<'a>> {
    let mut locations = Vec::new();
    match i {
        // These ones each write to one arg
        Instr::addq(_, r)
        | Instr::subq(_, r)
        | Instr::negq(r)
        | Instr::movq(_, r)
        | Instr::popq(r) => {
            if let Some(loc) = Location::try_from_arg(r) {
                locations.push(loc);
            }
        }

        // Any caller-saved register should be presumed written during a
        // function call
        Instr::callq(_, _) => {
            const CALLER_SAVED_REGISTERS: [Register; 9] = [
                Register::rax,
                Register::rcx,
                Register::rdx,
                Register::rsi,
                Register::rdi,
                Register::r8,
                Register::r9,
                Register::r10,
                Register::r11,
            ];

            locations.extend(CALLER_SAVED_REGISTERS.iter().map(|r| Location::Reg(*r)));
        }

        Instr::pushq(_) | Instr::retq => {} // These ones never read variables
    };

    locations
}
