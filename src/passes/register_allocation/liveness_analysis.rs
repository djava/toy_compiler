use petgraph::graph::{NodeIndex, UnGraph};
use std::collections::{HashMap, HashSet};

use crate::{passes::register_allocation::Location, x86_ast::*};

#[derive(Debug, Clone)]
pub struct LivenessMap<'a> {
    pub interference_graph: UnGraph<Location<'a>, ()>,
}

impl<'a> LivenessMap<'a> {
    pub fn from_instrs(instrs: &'a [Instr<'a>]) -> Self {
        let alive_befores = LivenessMap::make_alive_befores(instrs);
        dbg!(&alive_befores);
        let all_locations: HashSet<_> = instrs.iter().map(locs_written).flatten().collect();
        let interference_graph = LivenessMap::make_interference_graph(instrs, &alive_befores, &all_locations);

        Self { interference_graph }
    }

    fn make_alive_befores(instrs: &'a [Instr<'a>]) -> Vec<HashSet<Location<'a>>> {
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
                let alive_before = {
                    let written = locs_written(instr);
                    alive_after.extract_if(|l| written.contains(l));
                    alive_after.extend(&locs_read(instr));
                    alive_after.clone()
                };
                Some(alive_before)
            })
            .collect();

        alive_befores.reverse();
        alive_befores

    }

    fn make_interference_graph(
        instrs: &'a [Instr<'a>],
        alive_befores: &[HashSet<Location<'a>>],
        all_locations: &HashSet<Location<'a>>
    ) -> UnGraph<Location<'a>, ()> {
        let mut graph = UnGraph::<Location<'a>, ()>::new_undirected();
        let loc_to_node: HashMap<Location<'_>, NodeIndex> = all_locations
            .into_iter()
            .map(|loc| (*loc, graph.add_node(*loc)))
            .collect();
        dbg!(&loc_to_node);

        // Extra l_after for the last instruction, since it wouldn't
        // have one because these after's are being converted from before's.
        let last_l_after = &HashSet::new();
        let alive_afters = alive_befores.iter().skip(1).chain([last_l_after]);

        // Following algoirthm from textbook
        for (i, l_after) in instrs.iter().zip(alive_afters) {
            if let Instr::movq(s_arg, d_arg) = i
                && let Some(s) = Location::try_from_arg(s_arg)
                && let Some(d) = Location::try_from_arg(d_arg)
            {
                // If instruction is movq, for each v in L_after, if v
                // is neither s nor d, add edge (d, v)
                for v in l_after {
                    if *v != s && *v != d {
                        graph.add_edge(loc_to_node[&d], loc_to_node[v], ());
                    }
                }
            } else {
                // If instruction is not movq, for each d in W(k) and v
                // in L_after, if v != d, add edge (d,v)
                for d in locs_written(i) {
                    for v in l_after {
                        if *v != d {
                            graph.add_edge(loc_to_node[&d], loc_to_node[v], ());
                        }
                    }
                }
            }
        }

        graph
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
