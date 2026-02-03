use petgraph::graph::{NodeIndex, UnGraph};
use std::collections::{BinaryHeap, HashMap, HashSet};

use crate::{
    passes::register_allocation::{Location, Storage},
    x86_ast::Register,
};

pub(super) const COLOR_TO_REG_STORAGE: [(i32, Storage); 16] = [
    // Reserved registers are negative so they don't get assigned to
    (-1, Storage::Reg(Register::rax)),
    (-2, Storage::Reg(Register::rbp)),
    (-3, Storage::Reg(Register::rsp)),
    (-4, Storage::Reg(Register::r11)),
    (-5, Storage::Reg(Register::r15)),
    (1, Storage::Reg(Register::rcx)),
    (2, Storage::Reg(Register::rdx)),
    (3, Storage::Reg(Register::rsi)),
    (4, Storage::Reg(Register::rdi)),
    (5, Storage::Reg(Register::r8)),
    (6, Storage::Reg(Register::r9)),
    (7, Storage::Reg(Register::r10)),
    (8, Storage::Reg(Register::rbx)),
    (9, Storage::Reg(Register::r12)),
    (10, Storage::Reg(Register::r13)),
    (11, Storage::Reg(Register::r14)),
];

pub(super) fn reg_to_color(r: &Register) -> i32 {
    match r {
        Register::rax => -1,
        Register::rbp => -2,
        Register::rsp => -3,
        Register::r11 => -4,
        Register::r15 => -5,
        Register::rcx => 1,
        Register::rdx => 2,
        Register::rsi => 3,
        Register::rdi => 4,
        Register::r8 => 5,
        Register::r9 => 6,
        Register::r10 => 7,
        Register::rbx => 8,
        Register::r12 => 9,
        Register::r13 => 10,
        Register::r14 => 11,
    }
}

pub fn color_location_graph<'a>(graph: &'a UnGraph<Location, ()>) -> HashMap<&'a Location, i32> {
    let mut unavailable_colors: HashMap<NodeIndex, HashSet<i32>> =
        HashMap::with_capacity(graph.node_count());
    let mut colors = HashMap::with_capacity(graph.node_count());

    for idx in graph.node_indices() {
        let location = graph.node_weight(idx).unwrap();
        if let Location::Reg(reg) = location {
            colors.insert(idx, reg_to_color(reg));
        } else {
            unavailable_colors.insert(idx, HashSet::new());
        }
    }

    // Update unavailable_colors for initial register connections
    for edge_idx in graph.edge_indices() {
        let (node_idx1, node_idx2) = graph.edge_endpoints(edge_idx).unwrap();
        if let Some(&color1) = colors.get(&node_idx1) {
            if let Some(set) = unavailable_colors.get_mut(&node_idx2) {
                set.insert(color1);
            }
        }
        if let Some(&color2) = colors.get(&node_idx2) {
            if let Some(set) = unavailable_colors.get_mut(&node_idx1) {
                set.insert(color2);
            }
        }
    }

    // Build heap with current priorities (max-heap by constraint count)
    let mut queue: BinaryHeap<(usize, NodeIndex)> = unavailable_colors
        .iter()
        .map(|(&idx, set)| (set.len(), idx))
        .collect();

    while let Some((_, idx)) = queue.pop() {
        // Skip if already colored (stale entry)
        if colors.contains_key(&idx) {
            continue;
        }

        let unavail_set = &unavailable_colors[&idx];
        let color = (1..).find(|c| !unavail_set.contains(c)).unwrap();
        colors.insert(idx, color);

        // Update neighbors and re-push them with new priorities
        for neighbor in graph.neighbors(idx) {
            if let Some(neighbor_set) = unavailable_colors.get_mut(&neighbor) {
                if !colors.contains_key(&neighbor) && neighbor_set.insert(color) {
                    queue.push((neighbor_set.len(), neighbor));
                }
            }
        }
    }

    colors
        .into_iter()
        .map(|(k, v)| (graph.node_weight(k).unwrap(), v))
        .collect()
}
