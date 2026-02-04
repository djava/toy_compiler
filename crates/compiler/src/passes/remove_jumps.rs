use petgraph::visit::EdgeRef;

use crate::{passes::X86Pass, utils::x86_block_adj_graph, syntax_trees::x86::*};

pub struct RemoveJumps;

impl X86Pass for RemoveJumps {
    fn run_pass(self, mut m: X86Program) -> X86Program {
        perform_operation(&mut m.blocks);
        m
    }
}

fn perform_operation(blocks: &mut Vec<Block>) {
    'outer: loop {
        let cloned_blocks = blocks.clone();
        let block_graph = x86_block_adj_graph(&cloned_blocks);

        for from_idx in block_graph.node_indices() {
            let mut edge_iter = block_graph.edges_directed(from_idx, petgraph::Direction::Incoming);
            if edge_iter.clone().count() == 1 {
                let into_idx = edge_iter.next().unwrap().source();

                let from_block = &blocks[from_idx.index()];
                let from_label = if let Directive::Label(label) = &from_block.label {
                    label
                } else {
                    panic!("Block label was not label")
                };

                if let Some(Instr::jmp(into_label)) = &blocks[into_idx.index()].instrs.last()
                    && into_label == from_label
                {
                    // Only now are we sure these blocks can be squashed
                    //  - This block's only source edge is source
                    //  - Source's last instruction jumps to this block
                    // We can remove the last jump and insert this whole
                    // block in its place
                    let mut into_idx_corr = into_idx.index();
                    let from_block = blocks.remove(from_idx.index());

                    if from_idx.index() < into_idx_corr {
                        // If from_idx is before into, then removing it
                        // will mess up the indices. This corrects for that.
                        into_idx_corr -= 1;
                    }
                    let into_block = blocks.get_mut(into_idx_corr).unwrap();

                    into_block.instrs.pop();
                    into_block.instrs.extend(from_block.instrs);

                    // It's inefficient, but for now we're just going to
                    // start over after every time doing this because
                    // otherwise doing this will invalidate the whole
                    // graph, since the CFG contains cycles.
                    continue 'outer;
                }
            }
        }

        break 'outer;
    }
}
