use petgraph::visit::EdgeRef;

use crate::{passes::X86Pass, utils::x86_block_adj_graph, x86_ast::*};

pub struct RemoveJumps;

impl X86Pass for RemoveJumps {
    fn run_pass(self, mut m: X86Program) -> X86Program {
        let block_graph = x86_block_adj_graph(&m.blocks);

        let mut blocks_to_squash = vec![];
        for this_idx in block_graph.node_indices() {
            let mut edge_iter = block_graph.edges_directed(this_idx, petgraph::Direction::Incoming);
            if edge_iter.clone().count() == 1 {
                let source_node = edge_iter.next().unwrap().source();
                let source = block_graph.node_weight(source_node).unwrap();

                let this_block = block_graph.node_weight(this_idx).unwrap();
                let this_label = if let Directive::Label(label) = &this_block.label {
                    label
                } else {
                    panic!("Block label was not label")
                };

                if let Some(Instr::jmp(source_label)) = source.instrs.last()
                    && source_label == this_label
                {
                    // Only now are we sure these blocks can be squashed
                    //  - This block's only source edge is source
                    //  - Source's last instruction jumps to this block
                    // We can remove the last jump and insert this whole
                    // block in its place
                    blocks_to_squash.push((source_node.index(), this_idx.index()));
                }
            }
        }

        // Go in reverse order to prevent the stale indices from being
        // problematic. Blocks should be sorted by usage - we should
        // always be merging a higher index into a lower one, so if this
        // operation is chained then going in forward order could cause
        // a case where a block gets merged out of, then the next block
        // gets merged into it and both get deleted. Reverse order
        // prevents this possibility
        for (into_idx, from_idx) in blocks_to_squash.into_iter().rev() {
            let from_block = m.blocks.remove(from_idx);
            let into_block = m.blocks.get_mut(into_idx).unwrap();

            let from_label = if let Directive::Label(label) = &from_block.label {
                label
            } else {
                panic!("Block label was not label");
            };
            // Sanity check: the merge must still be valid
            if let Some(Instr::jmp(jump_label)) = into_block.instrs.last()
                    && jump_label != from_label {
                println!("RemoveJump: Tried to merge blocks that aren't mergeable.. bug in this function?");
                continue;
            }

            into_block.instrs.pop();
            into_block.instrs.extend(from_block.instrs);
        }

        m
    }
}
