use petgraph::{
    graph::{NodeIndex, UnGraph},
    visit::EdgeRef,
};
use std::collections::{HashMap, HashSet};

use crate::{
    passes::register_allocation::{Location, Storage, dataflow_analysis::DataflowAnalysis},
    syntax_trees::x86::Register,
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

pub fn color_location_graph<'a>(dataflow: &'a DataflowAnalysis) -> HashMap<&'a Location, i32> {
    let interference = &dataflow.interference;
    let move_rel = &dataflow.move_relations;

    let mut unavailable_colors_map: HashMap<NodeIndex, HashSet<i32>> =
        HashMap::with_capacity(interference.node_count());
    let mut color_map = HashMap::with_capacity(interference.node_count());

    for idx in interference.node_indices() {
        let location = interference.node_weight(idx).unwrap();
        if let Location::Reg(reg) = location {
            color_map.insert(idx, reg_to_color(reg));
        } else {
            unavailable_colors_map.insert(idx, HashSet::new());
        }
    }

    // Update unavailable_colors for initial register connections
    for edge_idx in interference.edge_indices() {
        let (node_idx1, node_idx2) = interference.edge_endpoints(edge_idx).unwrap();
        if let Some(&color1) = color_map.get(&node_idx1) {
            if let Some(set) = unavailable_colors_map.get_mut(&node_idx2) {
                set.insert(color1);
            }
        }
        if let Some(&color2) = color_map.get(&node_idx2) {
            if let Some(set) = unavailable_colors_map.get_mut(&node_idx1) {
                set.insert(color2);
            }
        }
    }

    while let Some(sat) = get_max_biased_sat(&unavailable_colors_map, move_rel, &color_map) {
        let idx = sat.node_idx;

        let unavail_set = &unavailable_colors_map[&idx];
        let selected_color = if let Some(pref_node) = sat.avail_move_related_node_idx
            && let Some(pref_color) = color_map.get(&pref_node)
            && !unavail_set.contains(pref_color)
        {
            // If there is a move-related node which already has its
            // color determined (and we make extra super sure that it
            // it's available and valid) then use that one, to cut down
            // on unnecessary moves
            *pref_color
        } else {
            (1..).find(|c| !unavail_set.contains(c)).unwrap()
        };

        color_map.insert(idx, selected_color);

        for neighbor in interference.neighbors(idx) {
            if let Some(neighbor_set) = unavailable_colors_map.get_mut(&neighbor) {
                neighbor_set.insert(selected_color);
            }
        }
    }

    color_map
        .into_iter()
        .map(|(k, v)| (interference.node_weight(k).unwrap(), v))
        .collect()
}

fn get_max_biased_sat(
    unavailable_colors_map: &HashMap<NodeIndex, HashSet<i32>>,
    move_rel: &UnGraph<Location, ()>,
    color_map: &HashMap<NodeIndex, i32>,
) -> Option<BiasedSaturation> {
    let mut max_bsat = None;

    for (idx, set) in unavailable_colors_map {
        if !color_map.contains_key(idx) {
            let bsat = BiasedSaturation::calculate(*idx, set, move_rel, &color_map);

            if max_bsat.is_none() || bsat > max_bsat.unwrap() {
                max_bsat = Some(bsat);
            }
        }
    }

    max_bsat
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct BiasedSaturation {
    node_idx: NodeIndex,
    num_unavail: usize,
    avail_move_related_node_idx: Option<NodeIndex>,
}

impl BiasedSaturation {
    fn calculate(
        node_idx: NodeIndex,
        unavailable_colors: &HashSet<i32>,
        move_rel: &UnGraph<Location, ()>,
        color_map: &HashMap<NodeIndex, i32>,
    ) -> BiasedSaturation {
        let num_unavail = unavailable_colors.len();

        // Note: move_rel uses the same node_idx's as interference
        // because they are added in the exact same way

        // We want to figure out if there is another move-related node
        // who already has its color assigned and doesn't interfere with
        // this one. This will give better reg-alloc performance by
        // making trivial moves that can be eliminated more common.
        let avail_move_related_node_idx = {
            let mut idx = None;
            for e in move_rel.edges(node_idx) {
                let other_node = if e.source() != node_idx {
                    e.source()
                } else {
                    e.target()
                };
                if let Some(color) = color_map.get(&other_node)
                    && !unavailable_colors.contains(color)
                    && *color > 0 // Avoid using allocating to a reserved register
                {
                    idx = Some(other_node);
                    break;
                }
            }

            idx
        };

        BiasedSaturation {
            node_idx,
            num_unavail,
            avail_move_related_node_idx,
        }
    }
}

impl PartialOrd for BiasedSaturation {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.num_unavail == other.num_unavail {
            match (
                self.avail_move_related_node_idx,
                other.avail_move_related_node_idx,
            ) {
                (Some(_), Some(_)) => Some(std::cmp::Ordering::Equal),
                (Some(_), None) => Some(std::cmp::Ordering::Greater),
                (None, Some(_)) => Some(std::cmp::Ordering::Less),
                (None, None) => Some(std::cmp::Ordering::Equal),
            }
        } else {
            Some(self.num_unavail.cmp(&other.num_unavail))
        }
    }
}

impl Ord for BiasedSaturation {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // partial_cmp is always Some
        self.partial_cmp(other).unwrap()
    }
}
