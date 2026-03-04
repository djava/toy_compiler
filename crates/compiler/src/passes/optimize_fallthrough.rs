use std::collections::VecDeque;

use petgraph::{Direction, Graph, graph::NodeIndex, visit::EdgeRef};

use crate::{
    passes::X86Pass,
    syntax_trees::x86::*,
    utils::{JumpType, x86_block_adj_graph},
};

/// `OptimizeFallthrough` Pass
///
/// Reorders basic blocks to maximize fallthrough (sequential execution
/// without jumps) by following unconditional-jump chains from the entry
/// block. After reordering, removes any trailing unconditional jmp
/// whose target is the immediately following block such that
/// fallthrough will occur instead, optimizing the output.
///
/// Optional optimization pass, does not affect functionality
///
/// Pre-conditions:
/// - `RemoveJumps` (Soft pre-condition, or else optimization
///                  opportunities will be lost)
///
/// Post-conditions:
/// - Blocks are reordered for maximum fallthrough
/// - Any unconditional jmp to the physically next block is removed
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
                    node_weight.label == f.entry_block
                })
                .expect("Couldn't find entry block in adj graph");

            let node_order = optimize_block_order(block_adj_graph, entry_node);

            permute_into_order(&mut f.blocks, node_order);

            for i in 0..f.blocks.len().saturating_sub(1) {
                let should_pop = if let Some(Instr::jmp(jmp_dest)) = f.blocks[i].instrs.last() {
                    jmp_dest == &f.blocks[i + 1].label
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

#[cfg(test)]
mod tests {
    use crate::utils::t_global;
    use test_support::compiler::{
        constants::LABEL_MAIN,
        passes::{OptimizeFallthrough, X86Pass},
        syntax_trees::{shared::*, x86::*},
    };

    #[test]
    fn test_straight_chain() {
        let program = OptimizeFallthrough.run_pass(X86Program {
            header: vec![],
            functions: vec![Function {
                header: vec![],
                name: t_global!(LABEL_MAIN),
                blocks: vec![
                    Block {
                        label: Identifier::Ephemeral(4),
                        instrs: vec![Instr::retq],
                    },
                    Block {
                        label: Identifier::Ephemeral(1),
                        instrs: vec![Instr::jmp(Identifier::Ephemeral(2))],
                    },
                    Block {
                        label: Identifier::Ephemeral(3),
                        instrs: vec![Instr::jmp(Identifier::Ephemeral(4))],
                    },
                    Block {
                        label: Identifier::Ephemeral(2),
                        instrs: vec![Instr::jmp(Identifier::Ephemeral(3))],
                    },
                ],
                entry_block: Identifier::Ephemeral(1),
                exit_block: Identifier::Ephemeral(1),
                stack_size: 0,
                gc_stack_size: 0,
                types: TypeEnv::new(),
                callee_saved_used: vec![],
            }],
        });

        assert_eq!(
            program.functions[0].blocks,
            vec![
                Block {
                    label: Identifier::Ephemeral(1),
                    instrs: vec![],
                },
                Block {
                    label: Identifier::Ephemeral(2),
                    instrs: vec![],
                },
                Block {
                    label: Identifier::Ephemeral(3),
                    instrs: vec![],
                },
                Block {
                    label: Identifier::Ephemeral(4),
                    instrs: vec![Instr::retq],
                },
            ],
        )
    }

    #[test]
    fn test_simple_loop() {
        // entry(1) -> header(2) <-> body(3), header(2) -[cond]-> exit(4)
        let program = OptimizeFallthrough.run_pass(X86Program {
            header: vec![],
            functions: vec![Function {
                header: vec![],
                name: t_global!(LABEL_MAIN),
                blocks: vec![
                    Block {
                        label: Identifier::Ephemeral(3),
                        instrs: vec![Instr::jmp(Identifier::Ephemeral(2))],
                    },
                    Block {
                        label: Identifier::Ephemeral(1),
                        instrs: vec![Instr::jmp(Identifier::Ephemeral(2))],
                    },
                    Block {
                        label: Identifier::Ephemeral(4),
                        instrs: vec![Instr::retq],
                    },
                    Block {
                        label: Identifier::Ephemeral(2),
                        instrs: vec![
                            Instr::jmpcc(Comparison::Equals, Identifier::Ephemeral(4)),
                            Instr::jmp(Identifier::Ephemeral(3)),
                        ],
                    },
                ],
                entry_block: Identifier::Ephemeral(1),
                exit_block: Identifier::Ephemeral(1),
                stack_size: 0,
                gc_stack_size: 0,
                types: TypeEnv::new(),
                callee_saved_used: vec![],
            }],
        });

        // Expected order: 1 -> 2 -> 3 -> 4
        // - 1's jmp(2) eliminated (fallthrough)
        // - 2's jmp(3) eliminated (fallthrough); jmpcc(4) kept
        // - 3's jmp(2) kept (back-edge; 4 follows, not 2)
        assert_eq!(
            program.functions[0].blocks,
            vec![
                Block {
                    label: Identifier::Ephemeral(1),
                    instrs: vec![],
                },
                Block {
                    label: Identifier::Ephemeral(2),
                    instrs: vec![Instr::jmpcc(Comparison::Equals, Identifier::Ephemeral(4))],
                },
                Block {
                    label: Identifier::Ephemeral(3),
                    instrs: vec![Instr::jmp(Identifier::Ephemeral(2))],
                },
                Block {
                    label: Identifier::Ephemeral(4),
                    instrs: vec![Instr::retq],
                },
            ],
        )
    }

    #[test]
    fn test_if_in_loop() {
        // loop with if-else inside:
        //   entry(1) -> header(2) -[cond exit]-> exit(6)
        //   header(2) -> if_cond(3) -[cond then]-> then(5) -> header(2)
        //                if_cond(3)              -> else(4) -> header(2)
        let program = OptimizeFallthrough.run_pass(X86Program {
            header: vec![],
            functions: vec![Function {
                header: vec![],
                name: t_global!(LABEL_MAIN),
                blocks: vec![
                    Block {
                        label: Identifier::Ephemeral(5),
                        instrs: vec![Instr::jmp(Identifier::Ephemeral(2))],
                    },
                    Block {
                        label: Identifier::Ephemeral(1),
                        instrs: vec![Instr::jmp(Identifier::Ephemeral(2))],
                    },
                    Block {
                        label: Identifier::Ephemeral(6),
                        instrs: vec![Instr::retq],
                    },
                    Block {
                        label: Identifier::Ephemeral(3),
                        instrs: vec![
                            Instr::jmpcc(Comparison::Equals, Identifier::Ephemeral(5)),
                            Instr::jmp(Identifier::Ephemeral(4)),
                        ],
                    },
                    Block {
                        label: Identifier::Ephemeral(4),
                        instrs: vec![Instr::jmp(Identifier::Ephemeral(2))],
                    },
                    Block {
                        label: Identifier::Ephemeral(2),
                        instrs: vec![
                            Instr::jmpcc(Comparison::Equals, Identifier::Ephemeral(6)),
                            Instr::jmp(Identifier::Ephemeral(3)),
                        ],
                    },
                ],
                entry_block: Identifier::Ephemeral(1),
                exit_block: Identifier::Ephemeral(1),
                stack_size: 0,
                gc_stack_size: 0,
                types: TypeEnv::new(),
                callee_saved_used: vec![],
            }],
        });

        // Expected order: 1 -> 2 -> 3 -> 4 -> 6 -> 5
        // (6 placed before 5 because exit is queued from the outer condition first)
        // - 1's jmp(2) eliminated; 2's jmp(3) eliminated; 3's jmp(4) eliminated
        // - 4's jmp(2) kept (back-edge; 6 follows, not 2)
        // - 5's jmp(2) kept (last block, back-edge)
        assert_eq!(
            program.functions[0].blocks,
            vec![
                Block {
                    label: Identifier::Ephemeral(1),
                    instrs: vec![],
                },
                Block {
                    label: Identifier::Ephemeral(2),
                    instrs: vec![Instr::jmpcc(Comparison::Equals, Identifier::Ephemeral(6))],
                },
                Block {
                    label: Identifier::Ephemeral(3),
                    instrs: vec![Instr::jmpcc(Comparison::Equals, Identifier::Ephemeral(5))],
                },
                Block {
                    label: Identifier::Ephemeral(4),
                    instrs: vec![Instr::jmp(Identifier::Ephemeral(2))],
                },
                Block {
                    label: Identifier::Ephemeral(6),
                    instrs: vec![Instr::retq],
                },
                Block {
                    label: Identifier::Ephemeral(5),
                    instrs: vec![Instr::jmp(Identifier::Ephemeral(2))],
                },
            ],
        )
    }

    #[test]
    fn test_nested_loop() {
        // outer: entry(1) -> outer_header(2) -[cond exit]-> outer_exit(6)
        //   outer_header(2) -> inner_header(3) -[cond inner_exit]-> inner_exit(5) -> outer_header(2)
        //                      inner_header(3) -> inner_body(4) -> inner_header(3)
        let program = OptimizeFallthrough.run_pass(X86Program {
            header: vec![],
            functions: vec![Function {
                header: vec![],
                name: t_global!(LABEL_MAIN),
                blocks: vec![
                    Block {
                        label: Identifier::Ephemeral(5),
                        instrs: vec![Instr::jmp(Identifier::Ephemeral(2))],
                    },
                    Block {
                        label: Identifier::Ephemeral(1),
                        instrs: vec![Instr::jmp(Identifier::Ephemeral(2))],
                    },
                    Block {
                        label: Identifier::Ephemeral(6),
                        instrs: vec![Instr::retq],
                    },
                    Block {
                        label: Identifier::Ephemeral(3),
                        instrs: vec![
                            Instr::jmpcc(Comparison::Equals, Identifier::Ephemeral(5)),
                            Instr::jmp(Identifier::Ephemeral(4)),
                        ],
                    },
                    Block {
                        label: Identifier::Ephemeral(4),
                        instrs: vec![Instr::jmp(Identifier::Ephemeral(3))],
                    },
                    Block {
                        label: Identifier::Ephemeral(2),
                        instrs: vec![
                            Instr::jmpcc(Comparison::Equals, Identifier::Ephemeral(6)),
                            Instr::jmp(Identifier::Ephemeral(3)),
                        ],
                    },
                ],
                entry_block: Identifier::Ephemeral(1),
                exit_block: Identifier::Ephemeral(1),
                stack_size: 0,
                gc_stack_size: 0,
                types: TypeEnv::new(),
                callee_saved_used: vec![],
            }],
        });

        // Expected order: 1 -> 2 -> 3 -> 4 -> 6 -> 5
        // - 1/2/3's trailing jmps eliminated (fallthroughs)
        // - 4's jmp(3) kept (back-edge to inner header; 6 follows, not 3)
        // - 5's jmp(2) kept (back-edge to outer header; last block)
        assert_eq!(
            program.functions[0].blocks,
            vec![
                Block {
                    label: Identifier::Ephemeral(1),
                    instrs: vec![],
                },
                Block {
                    label: Identifier::Ephemeral(2),
                    instrs: vec![Instr::jmpcc(Comparison::Equals, Identifier::Ephemeral(6))],
                },
                Block {
                    label: Identifier::Ephemeral(3),
                    instrs: vec![Instr::jmpcc(Comparison::Equals, Identifier::Ephemeral(5))],
                },
                Block {
                    label: Identifier::Ephemeral(4),
                    instrs: vec![Instr::jmp(Identifier::Ephemeral(3))],
                },
                Block {
                    label: Identifier::Ephemeral(6),
                    instrs: vec![Instr::retq],
                },
                Block {
                    label: Identifier::Ephemeral(5),
                    instrs: vec![Instr::jmp(Identifier::Ephemeral(2))],
                },
            ],
        )
    }

    #[test]
    fn test_chain_with_if() {
        let program = OptimizeFallthrough.run_pass(X86Program {
            header: vec![],
            functions: vec![Function {
                header: vec![],
                name: t_global!(LABEL_MAIN),
                blocks: vec![
                    Block {
                        label: Identifier::Ephemeral(4),
                        instrs: vec![Instr::retq],
                    },
                    Block {
                        label: Identifier::Ephemeral(1),
                        instrs: vec![Instr::jmp(Identifier::Ephemeral(2))],
                    },
                    Block {
                        label: Identifier::Ephemeral(5),
                        instrs: vec![Instr::jmp(Identifier::Ephemeral(4))],
                    },
                    Block {
                        label: Identifier::Ephemeral(3),
                        instrs: vec![Instr::jmp(Identifier::Ephemeral(4))],
                    },
                    Block {
                        label: Identifier::Ephemeral(2),
                        instrs: vec![
                            Instr::jmpcc(Comparison::Equals, Identifier::Ephemeral(5)),
                            Instr::jmp(Identifier::Ephemeral(3)),
                        ],
                    },
                ],
                entry_block: Identifier::Ephemeral(1),
                exit_block: Identifier::Ephemeral(1),
                stack_size: 0,
                gc_stack_size: 0,
                types: TypeEnv::new(),
                callee_saved_used: vec![],
            }],
        });

        assert_eq!(
            program.functions[0].blocks,
            vec![
                Block {
                    label: Identifier::Ephemeral(1),
                    instrs: vec![],
                },
                Block {
                    label: Identifier::Ephemeral(2),
                    instrs: vec![Instr::jmpcc(Comparison::Equals, Identifier::Ephemeral(5))],
                },
                Block {
                    label: Identifier::Ephemeral(3),
                    instrs: vec![],
                },
                Block {
                    label: Identifier::Ephemeral(4),
                    instrs: vec![Instr::retq],
                },
                Block {
                    label: Identifier::Ephemeral(5),
                    instrs: vec![Instr::jmp(Identifier::Ephemeral(4))],
                },
            ],
        )
    }
}
