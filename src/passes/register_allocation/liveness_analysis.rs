use petgraph::graph::UnGraph;
use std::collections::{HashMap, HashSet};

use crate::{passes::register_allocation::Location, x86_ast::*};

#[derive(Debug, Clone)]
pub struct LivenessMap {
    pub interference_graph: UnGraph<Location, ()>,
}

impl LivenessMap {
    pub fn from_instrs(instrs: &[Instr]) -> Self {
        let alive_befores = LivenessMap::make_alive_befores(instrs);
        let all_locations: HashSet<_> = instrs.iter().map(locs_written).flatten().collect();
        let interference_graph =
            LivenessMap::make_interference_graph(instrs, &alive_befores, &all_locations);

        Self { interference_graph }
    }

    fn make_alive_befores(instrs: &[Instr]) -> Vec<HashSet<Location>> {
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
                    alive_after.retain(|l| !written.contains(l));
                    alive_after.extend(locs_read(instr));
                    alive_after.clone()
                };
                Some(alive_before)
            })
            .collect();

        alive_befores.reverse();
        alive_befores
    }

    fn make_interference_graph(
        instrs: &[Instr],
        alive_befores: &[HashSet<Location>],
        all_locations: &HashSet<Location>,
    ) -> UnGraph<Location, ()> {
        let mut graph = UnGraph::new_undirected();
        let loc_to_node: HashMap<_, _> = all_locations
            .into_iter()
            .map(|loc| (loc, graph.add_node(loc.clone())))
            .collect();

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
                    if v != &s && v != &d {
                        graph.add_edge(loc_to_node[&d], loc_to_node[v], ());
                    }
                }
            } else {
                // If instruction is not movq, for each d in W(k) and v
                // in L_after, if v != d, add edge (d,v)
                for d in locs_written(i) {
                    for v in l_after {
                        if v != &d {
                            graph.add_edge(loc_to_node[&d], loc_to_node[v], ());
                        }
                    }
                }
            }
        }

        graph
    }
}

fn locs_read(i: &Instr) -> Vec<Location> {
    let mut locations = Vec::new();
    match i {
        Instr::addq(s, d) | Instr::subq(s, d) | Instr::xorq(s, d) | Instr::cmpq(s, d) => {
            locations = [s, d]
                .into_iter()
                .filter_map(Location::try_from_arg)
                .collect();
        }
        Instr::negq(r) | Instr::movq(r, _) | Instr::pushq(r) | Instr::movzbq(r, _) => {
            if let Some(loc) = Location::try_from_arg(r) {
                locations.push(loc);
            }
        }
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

        Instr::popq(_) | Instr::retq | Instr::set(_, _) | Instr::jmp(_) | Instr::jmpcc(_, _) => {}
    };
    locations
}

fn locs_written(i: &Instr) -> Vec<Location> {
    let mut locations = Vec::new();
    match i {
        Instr::addq(_, r)
        | Instr::subq(_, r)
        | Instr::negq(r)
        | Instr::movq(_, r)
        | Instr::movzbq(_, r)
        | Instr::popq(r)
        | Instr::xorq(_, r) => {
            if let Some(loc) = Location::try_from_arg(r) {
                locations.push(loc);
            }
        }
        Instr::set(_, r) => {
            if let Some(loc) = Location::try_from_arg(&Arg::Reg(r.to_underlying())) {
                locations.push(loc);
            }
        }
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
        Instr::pushq(_)
        | Instr::retq
        | Instr::cmpq(_, _)
        | Instr::jmp(_)
        | Instr::jmpcc(_, _) => {}
    };

    locations
}
