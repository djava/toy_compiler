use petgraph::{
    Direction,
    graph::{DiGraph, NodeIndex, UnGraph},
    visit::EdgeRef,
};
use std::collections::{HashMap, HashSet, VecDeque};

use crate::{
    constants::*,
    passes::register_allocation::Location,
    syntax_trees::{shared::*, x86::*},
    utils::{JumpType, x86_block_adj_graph},
};

#[derive(Debug, Clone)]
pub struct DataflowAnalysis {
    pub interference: UnGraph<Location, ()>,
    pub move_relations: UnGraph<Location, ()>,
    pub use_count: HashMap<Location, u32>,
}

impl DataflowAnalysis {
    pub fn from_function(f: &Function) -> DataflowAnalysis {
        let blocks = &f.blocks;
        let block_adj_graph = x86_block_adj_graph(blocks);

        let alive_after_instrs = Self::analyze_dataflow(block_adj_graph);
        let all_locations: Vec<_> = blocks
            .iter()
            .flat_map(|b| &b.instrs)
            .flat_map(locs_written)
            .chain(CALL_ARG_REGISTERS.iter().map(|r| Location::Reg(*r)))
            .collect::<HashSet<_>>()
            .into_iter()
            .collect();

        let alive_after_instrs = {
            let instrs = alive_after_instrs.iter().map(|(b, _)| &b.instrs).flatten();

            let alive_afters = alive_after_instrs
                .iter()
                .map(|(_, v_hashset)| v_hashset)
                .flatten();

            instrs.zip(alive_afters)
        };

        let interference =
            Self::make_interference_graph(alive_after_instrs, &all_locations, &f.types);

        let all_instrs = blocks.iter().flat_map(|b| &b.instrs);
        let move_relations = Self::make_move_relation_graph(all_instrs.clone(), &all_locations);

        let use_count = Self::make_use_count(all_instrs, &all_locations);

        Self {
            interference,
            move_relations,
            use_count,
        }
    }

    fn analyze_dataflow(
        block_graph: DiGraph<&Block, JumpType>,
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

        if instrs.is_empty() {
            // No instrs to fill the vec with, but the same locations
            // are alive before this block as are alive after it
            return (vec![], alive_after_block);
        }

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
        all_locations: &Vec<Location>,
        types: &TypeEnv,
    ) -> UnGraph<Location, ()> {
        // Must use this function to ensure that the graph nodes are
        // identical to the move relations graph.
        let (mut graph, loc_to_node) = make_empty_loc_graph(all_locations);

        // Following algoirthm from textbook
        for (i, l_after) in alive_before_instrs {
            if let Instr::mov(s_arg, d_arg) = i
                && let Some(s) = Location::try_from_arg(s_arg)
                && let Some(d) = Location::try_from_arg(d_arg)
            {
                // If instruction is movq, for each v in L_after, if v
                // is neither s nor d, add edge (d, v)
                for v in l_after {
                    if v != &s && v != &d && !matches!(v, Location::Id(Identifier::Global(_))) {
                        if !loc_to_node.contains_key(v) {
                            panic!(
                                "Couldn't find location node for {v:?} - most likely, it is read but never written to"
                            );
                        }
                        graph.add_edge(loc_to_node[&d], loc_to_node[v], ());
                    }
                }
            } else if let Instr::movzx(s_arg, d_arg) = i
                && let Some(s) = Location::try_from_arg(s_arg)
                && let Some(d) = Location::try_from_arg(d_arg)
            {
                // If instruction is movzx, for each v in L_after, if v
                // is neither s nor d, add edge (d, v)
                for v in l_after {
                    if v != &s && v != &d && !matches!(v, Location::Id(Identifier::Global(_))) {
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
                        if v != &d && !matches!(v, Location::Id(Identifier::Global(_))) {
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

        let tup_interference_graph_nodes: Vec<_> = graph
            .node_indices()
            .filter(|idx| {
                let loc = graph.node_weight(*idx).unwrap();
                matches!(loc, Location::Reg(r) if CALLEE_SAVED_REGISTERS.contains(&r) || CALLER_SAVED_REGISTERS.contains(&r))
            })
            .collect();

        // All tuple-typed locations need to have interference edges
        // with all callee- and caller- saved registers - this will
        // guarantee that they are spilled during any call to collect()
        // so that they are visible to the GC.
        for node_idx in graph.node_indices() {
            let loc = graph.node_weight(node_idx).unwrap();
            if let Location::Id(id) = loc
                && let Some(ValueType::TupleType(_)) = types.get(id)
            {
                for r in &tup_interference_graph_nodes {
                    graph.add_edge(*r, node_idx, ());
                }
            }
        }

        graph
    }

    fn make_move_relation_graph<'a>(
        instrs: impl Iterator<Item = &'a Instr>,
        all_locations: &Vec<Location>,
    ) -> UnGraph<Location, ()> {
        // Must use this function to ensure that the graph nodes are
        // identical to the interference graph.
        let (mut graph, loc_to_node) = make_empty_loc_graph(all_locations);

        for i in instrs {
            match i {
                Instr::mov(s, d) => {
                    if let Some(s_loc) = Location::try_from_arg(s)
                        && !matches!(s_loc, Location::Id(Identifier::Global(_)))
                        && let Some(d_loc) = Location::try_from_arg(d)
                        && !matches!(d_loc, Location::Id(Identifier::Global(_)))
                    {
                        if !loc_to_node.contains_key(&s_loc) {
                            panic!(
                                "Couldn't find location node for {s_loc:?} - most likely, it is read but never written to"
                            );
                        }
                        if !loc_to_node.contains_key(&d_loc) {
                            panic!(
                                "Couldn't find location node for {d_loc:?} - most likely, it is read but never written to"
                            );
                        }

                        let s_node = loc_to_node.get(&s_loc).unwrap();
                        let d_node = loc_to_node.get(&d_loc).unwrap();
                        graph.add_edge(*s_node, *d_node, ());
                    }
                }
                Instr::movzx(s, d) => {
                    let s_loc = if let ArgValue::Reg(s_reg) = &s.value {
                        Location::Reg(s_reg.to_quad())
                    } else {
                        unreachable!()
                    };
                    if let Some(d_loc) = Location::try_from_arg(d)
                        && !matches!(d_loc, Location::Id(Identifier::Global(_)))
                    {
                        if !loc_to_node.contains_key(&d_loc) {
                            panic!(
                                "Couldn't find location node for {d_loc:?} - most likely, it is read but never written to"
                            );
                        }

                        let s_node = loc_to_node.get(&s_loc).unwrap();
                        let d_node = loc_to_node.get(&d_loc).unwrap();
                        graph.add_edge(*s_node, *d_node, ());
                    }
                }
                _ => {}
            }
        }

        graph
    }

    fn make_use_count<'a>(
        instrs: impl Iterator<Item = &'a Instr>,
        all_locations: &Vec<Location>,
    ) -> HashMap<Location, u32> {
        let mut count_map = HashMap::from_iter(all_locations.iter().map(|loc| (loc.clone(), 0u32)));
        let mut count_for_arg = |a: &Arg| {
            if let Some(loc) = Location::try_from_arg(a)
                && !matches!(loc, Location::Id(Identifier::Global(_)))
            {
                let count = count_map
                    .get_mut(&loc)
                    .expect("Location wasn't in all_locations");
                *count += 1;
            }
        };

        for i in instrs {
            match i {
                Instr::add(arg, arg1)
                | Instr::sub(arg, arg1)
                | Instr::mov(arg, arg1)
                | Instr::xor(arg, arg1)
                | Instr::cmp(arg, arg1)
                | Instr::sar(arg, arg1)
                | Instr::sal(arg, arg1)
                | Instr::and(arg, arg1)
                | Instr::imul(arg, arg1)
                | Instr::lea(arg, arg1) => {
                    count_for_arg(arg);
                    count_for_arg(arg1);
                }

                Instr::neg(arg)
                | Instr::push(arg)
                | Instr::pop(arg)
                | Instr::call_ind(arg, _)
                | Instr::movzx(_, arg)
                | Instr::jmp_tail(arg, _)
                | Instr::idiv(arg) => {
                    count_for_arg(arg);
                }

                Instr::set(_, _)
                | Instr::jmp(_)
                | Instr::call(_, _)
                | Instr::jmpcc(_, _)
                | Instr::ret
                | Instr::cqto => {}
            }
        }

        count_map
    }
}

fn make_empty_loc_graph(
    all_locations: &Vec<Location>,
) -> (UnGraph<Location, ()>, HashMap<&Location, NodeIndex>) {
    let mut graph = UnGraph::new_undirected();
    let loc_to_node = all_locations
        .into_iter()
        .map(|loc| (loc, graph.add_node(loc.clone())))
        .collect();

    (graph, loc_to_node)
}

fn locs_read(i: &Instr) -> Vec<Location> {
    let mut locations = Vec::new();
    match i {
        Instr::add(s, d)
        | Instr::sub(s, d)
        | Instr::imul(s, d)
        | Instr::xor(s, d)
        | Instr::cmp(s, d)
        | Instr::and(s, d)
        | Instr::sal(s, d)
        | Instr::sar(s, d) => {
            locations = [s, d]
                .into_iter()
                .filter_map(Location::try_from_arg)
                .collect();
        }
        Instr::neg(r)
        | Instr::mov(r, _)
        | Instr::push(r)
        | Instr::lea(r, _) => {
            if let Some(loc) = Location::try_from_arg(r) {
                locations.push(loc);
            }
        }
        Instr::movzx(r, _) => {
            if let Some(loc) = Location::try_from_arg(r) {
                locations.push(loc);
            }
        }
        Instr::call(_, num_args) => {
            if *num_args > MAX_REGISTER_ARGS as u16 {
                unimplemented!("Spilling args onto stack not implemented");
            }

            locations.extend(
                CALL_ARG_REGISTERS
                    .iter()
                    .take(*num_args as _)
                    .map(|r| Location::Reg(*r)),
            );
        }
        Instr::call_ind(func, num_args) | Instr::jmp_tail(func, num_args) => {
            if *num_args > MAX_REGISTER_ARGS as u16 {
                unimplemented!("Spilling args onto stack not implemented");
            }

            if let Some(loc) = Location::try_from_arg(func) {
                locations.push(loc);
            }

            locations.extend(
                CALL_ARG_REGISTERS
                    .iter()
                    .take(*num_args as _)
                    .map(|r| Location::Reg(*r)),
            );
        }
        Instr::idiv(divisor) => {
            if let Some(divisor_loc) = Location::try_from_arg(divisor) {
                locations.push(divisor_loc)
            }

            locations.extend([Location::Reg(Register::rdx), Location::Reg(Register::rax)])
        }
        Instr::cqto => locations.push(Location::Reg(Register::rax)),

        Instr::pop(_) | Instr::ret | Instr::set(_, _) | Instr::jmp(_) | Instr::jmpcc(_, _) => {}
    };
    locations
}

fn locs_written(i: &Instr) -> Vec<Location> {
    let mut locations = Vec::new();
    match i {
        Instr::add(_, r)
        | Instr::sub(_, r)
        | Instr::imul(_, r)
        | Instr::neg(r)
        | Instr::mov(_, r)
        | Instr::movzx(_, r)
        | Instr::pop(r)
        | Instr::xor(_, r)
        | Instr::and(_, r)
        | Instr::sar(_, r)
        | Instr::sal(_, r)
        | Instr::lea(_, r)
        | Instr::set(_, r) => {
            if let Some(loc) = Location::try_from_arg(r) {
                locations.push(loc);
            }
        }
        Instr::call(func_id, _) => {
            locations.extend(CALLER_SAVED_REGISTERS.iter().map(|r| Location::Reg(*r)));

            // Consider r15 to be written by a call to __gc_collect()
            // because it might do the GC copy and change the gc stack ptr
            if let ArgValue::Global(Identifier::Global(name)) = &func_id.value
                && &**name == FN_GC_COLLECT
            {
                locations.push(Location::Reg(Register::r15));
            }
        }
        Instr::call_ind(_, _) | Instr::jmp_tail(_, _) => {
            locations.extend(CALLER_SAVED_REGISTERS.iter().map(|r| Location::Reg(*r)));
        }
        Instr::idiv(_) => {
            locations.extend([Location::Reg(Register::rax), Location::Reg(Register::rdx)])
        }
        Instr::cqto => {
            locations.push(Location::Reg(Register::rdx))
        }
        Instr::push(_) | Instr::ret | Instr::cmp(_, _) | Instr::jmp(_) | Instr::jmpcc(_, _) => {}
    };

    locations
}
