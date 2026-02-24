use std::collections::VecDeque;

use petgraph::{Graph, graph::NodeIndex, visit::EdgeRef, Direction};

use crate::{
    passes::X86Pass,
    syntax_trees::x86::*,
    utils::{JumpType, x86_block_adj_graph},
};

#[derive(Debug)]
pub struct OptimizeFallthrough;

impl X86Pass for OptimizeFallthrough {
    fn run_pass(self, mut m: X86Program) -> X86Program {
        for f in m.functions.iter_mut() {
            let block_adj_graph = x86_block_adj_graph(&f.blocks);

            let entry_node = block_adj_graph
                .node_indices()
                .find(|idx| {
                    let node_weight = block_adj_graph.node_weight(*idx).unwrap();
                    if let Directive::Label(node_label) = &node_weight.label {
                        *node_label == f.entry_block
                    } else {
                        panic!("node label wasn't label")
                    }
                })
                .expect("Couldn't find entry block in adj graph");

            let node_order = optimize_block_order(block_adj_graph, entry_node);

            permute_into_order(&mut f.blocks, node_order);

            for i in 0..f.blocks.len().saturating_sub(1) {
                let should_pop = if let Some(Instr::jmp(jmp_dest)) = f.blocks[i].instrs.last()
                    && let Directive::Label(next_label) = &f.blocks[i + 1].label
                {
                    jmp_dest == next_label
                } else {
                    false
                };

                if should_pop {
                    f.blocks[i].instrs.pop();
                }
            }
        }

        m
    }
}

fn optimize_block_order(
    adj_graph: Graph<&Block, JumpType>,
    entry_node: NodeIndex,
) -> Vec<NodeIndex> {
    let mut order = vec![];

    let mut queue = VecDeque::from([entry_node]);
    while !queue.is_empty() {
        let mut chain_tail = queue.pop_front().unwrap();
        if order.contains(&chain_tail) {
            continue;
        }

        let mut chain = vec![chain_tail];

        loop {
            // The chain may not include any blocks that don't get
            // unconditionally jumped to. Therefore, add them to the
            // queue to make sure that they get checked over and
            // included in the order.
            queue.extend(
                adj_graph
                    .edges_directed(chain_tail, Direction::Outgoing)
                    .filter(|e| *e.weight() == JumpType::Conditional)
                    .map(|e| e.target())
                    .filter(|n| !order.contains(n)),
            );

            let mut edge_iter = adj_graph
                .edges_directed(chain_tail, Direction::Outgoing)
                .filter(|e| *e.weight() == JumpType::Unconditional);
            if let Some(outgoing_edge) = edge_iter.next() {
                let next_node = outgoing_edge.target();
                // If we're looping back here or hitting something
                // that's already been in a chain further up in the
                // order, then this chain has ended
                if chain.contains(&next_node) || order.contains(&next_node) {
                    order.extend(chain);
                    break;
                } else {
                    // This is a new block, put it in this chain :)
                    chain_tail = next_node;
                    chain.push(next_node);
                }
            } else {
                order.extend(chain);
                break;
            }
        }
    }

    assert!(order.len() == adj_graph.node_count());

    order
}

fn permute_into_order(blocks: &mut Vec<Block>, new_order: Vec<NodeIndex>) {
    let mut old: Vec<Option<Block>> = blocks.drain(..).map(Some).collect();
    *blocks = new_order
        .into_iter()
        .map(|idx| old[idx.index()].take().unwrap())
        .collect();
}
