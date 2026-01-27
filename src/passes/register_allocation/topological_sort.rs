use petgraph::algo::toposort;

use crate::{utils::x86_block_adj_graph, x86_ast::Block};

pub(crate) fn block_topological_sort<'a>(mut blocks: Vec<Block>) -> Vec<Block> {
    let block_graph = x86_block_adj_graph(&blocks);

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
