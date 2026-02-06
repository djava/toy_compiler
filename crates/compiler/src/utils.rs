use std::collections::HashMap;

use crate::syntax_trees::x86::{Block, Directive, Instr};
use petgraph::graph::DiGraph;

pub fn x86_block_adj_graph<'a>(blocks: &'a [Block]) -> DiGraph<&'a Block, ()> {
    let mut block_graph = DiGraph::new();

    let label_node_map = {
        let mut map = HashMap::new();
        for b in blocks.iter() {
            if let Directive::Label(label) = &b.label {
                map.insert(label, block_graph.add_node(b));
            } else {
                panic!("block label was not a label");
            }
        }
        map
    };

    for idx in block_graph.node_indices() {
        let this_block = block_graph.node_weight(idx).unwrap();
        for i in this_block.instrs.iter() {
            match i {
                Instr::jmp(dest) | Instr::jmpcc(_, dest) => {
                    if let Some(dest_node) = label_node_map.get(dest) {
                        block_graph.add_edge(idx, *dest_node, ());
                    }
                }
                _ => {}
            }
        }
    }

    block_graph
}

macro_rules! id {
    ($name:expr) => {
        crate::syntax_trees::shared::Identifier::Named(std::sync::Arc::from($name))
    };
}
pub(crate) use id;

macro_rules! label {
    ($name:expr) => {
        crate::syntax_trees::x86::Directive::Label(crate::syntax_trees::shared::Identifier::Named(
            std::sync::Arc::from($name),
        ))
    };
}
pub(crate) use label;
