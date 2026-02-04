use petgraph::{
    Direction,
    graph::{DiGraph, NodeIndex, UnGraph},
    visit::EdgeRef,
};
use std::collections::{HashMap, HashSet, VecDeque};

use crate::{
    passes::register_allocation::Location,
    syntax_trees::{shared::*, x86::*},
    utils::x86_block_adj_graph,
};

#[derive(Debug, Clone)]
pub struct LivenessMap {
    pub interference_graph: UnGraph<Location, ()>,
}

impl LivenessMap {
    pub fn from_program(m: &X86Program) -> LivenessMap {
        let blocks = &m.blocks;
        let block_adj_graph = x86_block_adj_graph(blocks);

        let alive_after_instrs = Self::analyze_dataflow(block_adj_graph);
        let all_locations: HashSet<_> = blocks
            .iter()
            .map(|b| &b.instrs)
            .flatten()
            .map(locs_written)
            .flatten()
            .collect();

        let alive_after_instrs = {
            let instrs = alive_after_instrs.iter().map(|(b, _)| &b.instrs).flatten();

            let alive_afters = alive_after_instrs
                .iter()
                .map(|(_, v_hashset)| v_hashset)
                .flatten();

            instrs.zip(alive_afters)
        };

        let interference_graph =
            LivenessMap::make_interference_graph(alive_after_instrs, &all_locations, &m.types);

        Self { interference_graph }
    }

    fn analyze_dataflow(
        block_graph: DiGraph<&Block, ()>,
    ) -> HashMap<&Block, Vec<HashSet<Location>>> {
        let mut alive_before_blocks: HashMap<NodeIndex, HashSet<Location>> = block_graph
            .node_indices()
            .map(|b| (b, HashSet::new()))
            .collect();

        let mut work_list = VecDeque::from_iter(block_graph.node_indices());

        let mut final_alive_afters = HashMap::new();

        let mut iters: usize = 0;
        while !work_list.is_empty() {
            iters += 1;
            if iters > block_graph.node_count() * 20 {
                panic!("probably stuck with alive_after_blocks: {alive_before_blocks:?}");
            }

            let curr_node = work_list.pop_front().unwrap();
            let input = block_graph
                .edges_directed(curr_node, Direction::Outgoing)
                .map(|edge| alive_before_blocks[&edge.target()].clone())
                .fold(HashSet::new(), |mut acc: HashSet<Location>, this_set| {
                    acc.extend(this_set.clone());
                    acc
                });

            let curr_block = block_graph.node_weight(curr_node).unwrap();
            let (alive_afters, alive_before) =
                Self::get_alive_befores_and_after(&curr_block.instrs, input.clone());

            if alive_before_blocks[&curr_node] == alive_before {
                final_alive_afters.insert(*curr_block, alive_afters);
            } else {
                alive_before_blocks.insert(curr_node, alive_before);
                work_list.extend(
                    block_graph
                        .edges_directed(curr_node, Direction::Incoming)
                        .map(|e| e.source()),
                );
                work_list.push_back(curr_node);
            }
        }

        final_alive_afters
    }

    fn get_alive_befores_and_after(
        instrs: &[Instr],
        alive_after_block: HashSet<Location>,
    ) -> (Vec<HashSet<Location>>, HashSet<Location>) {
        // Implements the textbook's equation for liveness analysis:
        //      `L_before(k) = (L_after(k) - W(k)) union R(k)`
        //
        // Returns alive_AFTER for each instruction (needed for interference graph)
        // alive_after(instr_i) = alive_before(instr_i+1) = alive_before(block) for first instr

        let mut alive_befores_and_after: Vec<_> = instrs
            .iter()
            .rev()
            .scan(alive_after_block.clone(), |alive_after, instr| {
                let alive_before = {
                    let written = locs_written(instr);
                    alive_after.retain(|l| !written.contains(l));
                    alive_after.extend(locs_read(instr));
                    alive_after.clone()
                };
                Some(alive_before)
            })
            .collect();

        let alive_before = alive_befores_and_after.pop().unwrap();

        alive_befores_and_after.reverse();

        // Add alive_after(block) to get alive_after for the last instruction
        alive_befores_and_after.push(alive_after_block);

        (alive_befores_and_after, alive_before)
    }

    fn make_interference_graph<'a>(
        alive_before_instrs: impl Iterator<Item = (&'a Instr, &'a HashSet<Location>)>,
        all_locations: &HashSet<Location>,
        types: &TypeEnv,
    ) -> UnGraph<Location, ()> {
        let mut graph = UnGraph::new_undirected();
        let loc_to_node: HashMap<_, _> = all_locations
            .into_iter()
            .map(|loc| (loc, graph.add_node(loc.clone())))
            .collect();

        // Following algoirthm from textbook
        for (i, l_after) in alive_before_instrs {
            if let Instr::movq(s_arg, d_arg) = i
                && let Some(s) = Location::try_from_arg(s_arg)
                && let Some(d) = Location::try_from_arg(d_arg)
            {
                // If instruction is movq, for each v in L_after, if v
                // is neither s nor d, add edge (d, v)
                for v in l_after {
                    if !loc_to_node.contains_key(v) {
                        panic!(
                            "Couldn't find location node for {v:?} - most likely, it is read but never written to"
                        );
                    }
                    if v != &s && v != &d {
                        graph.add_edge(loc_to_node[&d], loc_to_node[v], ());
                    }
                }
            } else if let Instr::movzbq(s_reg, d_arg) = i
                && let Some(s) = Location::try_from_arg(&Arg::ByteReg(*s_reg))
                && let Some(d) = Location::try_from_arg(d_arg)
            {
                // If instruction is movzbq, for each v in L_after, if v
                // is neither s nor d, add edge (d, v)
                for v in l_after {
                    if v != &s && v != &d {
                        if !loc_to_node.contains_key(v) {
                            panic!(
                                "Couldn't find location node for {v:?} - most likely, it is read but never written to"
                            );
                        }
                        graph.add_edge(loc_to_node[&d], loc_to_node[v], ());
                    }
                }
            } else {
                // If instruction is not movq, for each d in W(k) and v
                // in L_after, if v != d, add edge (d,v)
                for d in locs_written(i) {
                    for v in l_after {
                        if v != &d {
                            if !loc_to_node.contains_key(v) {
                                panic!(
                                    "Couldn't find location node for {v:?} - most likely, it is read but never written to"
                                );
                            }
                            graph.add_edge(loc_to_node[&d], loc_to_node[v], ());
                        }
                    }
                }
            }
        }

        let callee_saved_graph_nodes: Vec<_> = graph
            .node_indices()
            .filter(|idx| {
                let loc = graph.node_weight(*idx).unwrap();
                matches!(loc, Location::Reg(r) if CALLEE_SAVED_REGISTERS.contains(&r))
            })
            .collect();

        // All tuple-typed locations need to have interference edges
        // with all callee-saved registers - this will guarantee that
        // they are spilled during any call to collect() so that they
        // are visible to the GC.
        for node_idx in graph.node_indices() {
            let loc = graph.node_weight(node_idx).unwrap();
            if let Location::Id(id) = loc
                && let Some(ValueType::TupleType(_)) = types.get(&AssignDest::Id(id.clone()))
            {
                for r in &callee_saved_graph_nodes {
                    graph.add_edge(*r, node_idx, ());
                }
            }
        }

        graph
    }
}

fn locs_read(i: &Instr) -> Vec<Location> {
    let mut locations = Vec::new();
    match i {
        Instr::addq(s, d)
        | Instr::subq(s, d)
        | Instr::xorq(s, d)
        | Instr::cmpq(s, d)
        | Instr::andq(s, d)
        | Instr::salq(s, d)
        | Instr::sarq(s, d) => {
            locations = [s, d]
                .into_iter()
                .filter_map(Location::try_from_arg)
                .collect();
        }
        Instr::negq(r) | Instr::movq(r, _) | Instr::pushq(r) => {
            if let Some(loc) = Location::try_from_arg(r) {
                locations.push(loc);
            }
        }
        Instr::movzbq(r, _) => {
            if let Some(loc) = Location::try_from_arg(&Arg::Reg(r.to_underlying())) {
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
        | Instr::xorq(_, r)
        | Instr::andq(_, r)
        | Instr::sarq(_, r)
        | Instr::salq(_, r) => {
            if let Some(loc) = Location::try_from_arg(r) {
                locations.push(loc);
            }
        }
        Instr::set(_, r) => {
            if let Some(loc) = Location::try_from_arg(&Arg::Reg(r.to_underlying())) {
                locations.push(loc);
            }
        }
        Instr::callq(func_id, _) => {
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

            // Consider r15 to be written by a call to __gc_collect()
            // because it might do the GC copy and change the gc stack ptr
            if let Identifier::Named(name) = func_id
                && &**name == "__gc_collect"
            {
                locations.push(Location::Reg(Register::r15));
            }
        }
        Instr::pushq(_) | Instr::retq | Instr::cmpq(_, _) | Instr::jmp(_) | Instr::jmpcc(_, _) => {}
    };

    locations
}
