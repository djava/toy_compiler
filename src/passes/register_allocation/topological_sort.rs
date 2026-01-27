use std::collections::HashMap;

use petgraph::{algo::toposort, graph::DiGraph};

use crate::{
    ir::Identifier,
    passes::register_allocation::Block,
    x86_ast::{Directive, Instr},
};

pub(crate) fn block_topological_sort<'a>(mut blocks: Vec<Block>) -> Vec<Block> {
    let mut block_graph: DiGraph<Identifier, ()> = DiGraph::new();

    let block_node_map = {
        let mut map = HashMap::new();
        for b in blocks.iter() {
            if let Directive::Label(l) = b.label.clone() {
                map.insert(l.clone(), block_graph.add_node(l));
            } else {
                panic!("block label isn't a label")
            }
        }
        map
    };

    for b in blocks.iter() {
        let this_block = if let Directive::Label(l) = &b.label {
            l
        } else {
            panic!("block label isn't a label")
        };

        let this_node = block_node_map[this_block];
        for i in b.instrs.iter() {
            match i {
                Instr::jmp(dest) | Instr::jmpcc(_, dest) => {
                    if let Some(dest_node) = block_node_map.get(dest) {
                        block_graph.add_edge(this_node, *dest_node, ());
                    }
                }
                _ => {}
            }
        }
    }

    let sorted = if let Ok(s) = toposort(&block_graph, None) {
        s
    } else {
        panic!("Block graph was cyclical?");
    };

    let sorted_indexes = sorted.into_iter().map(|x| x.index()).collect();
    permute_into_order(&mut blocks, sorted_indexes);

    blocks
}


fn permute_into_order<T>(data: &mut [T], mut order: Vec<usize>) {
    for i in 0..data.len() {
        while order[i] != i {
            let j = order[i];
            data.swap(i, j);
            order.swap(i, j);
        }
    }
}