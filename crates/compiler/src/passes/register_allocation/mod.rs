mod dataflow_analysis;
mod graph_coloring;

use std::collections::HashMap;
use std::mem::size_of;

use crate::{
    constants::*,
    passes::{X86Pass, register_allocation::graph_coloring::COLOR_TO_REG_STORAGE},
    syntax_trees::{shared::*, x86},
};
use dataflow_analysis::LivenessMap;
use graph_coloring::color_location_graph;
use x86::*;

pub struct RegisterAllocation;

impl X86Pass for RegisterAllocation {
    fn run_pass(self, mut m: X86Program) -> X86Program {
        let liveness = LivenessMap::from_program(&m);

        let AllocateStorageResult {
            id_to_storage,
            stack_size,
            gc_stack_size,
        } = allocate_storage(&liveness, &m.types);

        let callee_saved_used: Vec<_> = id_to_storage
            .values()
            .filter(|stg| matches!(stg, Storage::Reg(reg) if CALLEE_SAVED_REGISTERS.contains(reg)))
            .collect();
        let callee_offset = -((callee_saved_used.len() * size_of::<i64>()) as i32);

        for b in m.blocks.iter_mut() {
            run_for_block(&mut b.instrs, &id_to_storage, callee_offset);
        }

        if let Some(user_entry) = m
            .blocks
            .iter_mut()
            .find(|b| b.label == Directive::Label(Identifier::from(LABEL_USER_ENTRY)))
        {
            let callee_pushqs = callee_saved_used.iter().filter_map(|loc| {
                if let Storage::Reg(reg) = loc {
                    Some(Instr::pushq(Arg::Reg(*reg)))
                } else {
                    None
                }
            });

            user_entry.instrs.splice(0..0, callee_pushqs);
        }

        if let Some(user_exit) = m
            .blocks
            .iter_mut()
            .find(|b| b.label == Directive::Label(Identifier::from(LABEL_USER_EXIT)))
        {
            let callee_popqs = callee_saved_used.iter().rev().filter_map(|loc| {
                if let Storage::Reg(reg) = loc {
                    Some(Instr::popq(Arg::Reg(*reg)))
                } else {
                    None
                }
            });

            let len = user_exit.instrs.len();
            user_exit.instrs.splice((len - 2)..=(len - 2), callee_popqs);
        }

        let used_stack = -callee_offset + stack_size;
        let aligned_stack_size = if used_stack % STACK_ALIGNMENT == 0 {
            used_stack
        } else {
            used_stack + (STACK_ALIGNMENT - (used_stack % STACK_ALIGNMENT))
        };

        m.stack_size = aligned_stack_size as usize;
        m.gc_stack_size = gc_stack_size as usize;

        m
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
enum Location {
    Id(Identifier),
    Reg(Register),
}

impl Location {
    fn try_from_arg(arg: &Arg) -> Option<Self> {
        match arg {
            Arg::Deref(reg, _) => Some(Location::Reg(*reg)),
            Arg::Variable(id) => Some(Location::Id(id.clone())),
            Arg::Reg(reg) => Some(Location::Reg(*reg)),
            Arg::ByteReg(bytereg) => Some(Location::Reg(bytereg.to_underlying())),
            Arg::Immediate(_) => None,
            Arg::Global(_) => None,
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
enum Storage {
    Stack(i32),
    GCStack(i32),
    Reg(Register),
}

impl Storage {
    pub fn to_arg(self) -> Arg {
        match self {
            Storage::Stack(offset) => Arg::Deref(Register::rbp, offset),
            Storage::GCStack(offset) => Arg::Deref(Register::r15, offset),
            Storage::Reg(reg) => Arg::Reg(reg),
        }
    }

    pub fn with_stack_offset(self, off: i32) -> Self {
        if let Storage::Stack(offset) = self {
            Storage::Stack(offset + off)
        } else {
            self
        }
    }
}

fn run_for_block(
    instrs: &mut Vec<Instr>,
    id_to_storage: &HashMap<Identifier, Storage>,
    stack_offset: i32,
) {
    for i in instrs.iter_mut() {
        match i {
            Instr::addq(s, d)
            | Instr::subq(s, d)
            | Instr::imulq(s, d)
            | Instr::movq(s, d)
            | Instr::xorq(s, d)
            | Instr::cmpq(s, d)
            | Instr::andq(s, d)
            | Instr::sarq(s, d)
            | Instr::salq(s, d) => {
                replace_arg_with_allocated(s, id_to_storage, stack_offset);
                replace_arg_with_allocated(d, id_to_storage, stack_offset);
            }
            Instr::negq(a) | Instr::pushq(a) | Instr::popq(a) | Instr::movzbq(_, a) => {
                replace_arg_with_allocated(a, id_to_storage, stack_offset);
            }
            Instr::callq(_, _)
            | Instr::retq
            | Instr::jmpcc(_, _)
            | Instr::jmp(_)
            | Instr::set(_, _) => {
                // No real args to replace
            }
        }
    }
}

struct AllocateStorageResult {
    id_to_storage: HashMap<Identifier, Storage>,
    stack_size: i32,
    gc_stack_size: i32,
}

fn allocate_storage<'a>(liveness: &'a LivenessMap, types: &TypeEnv) -> AllocateStorageResult {
    let mut curr_stack_offset = 0i32;
    let mut curr_gc_stack_offset = 0i32;

    let graph_colors = color_location_graph(&liveness.interference_graph);
    let mut id_to_storage = HashMap::new();
    let mut color_to_storage = HashMap::from(COLOR_TO_REG_STORAGE);

    for (location, color) in &graph_colors {
        if let Location::Id(id) = location {
            let storage = if let Some(storage) = color_to_storage.get(color) {
                // If this color was already allocated a storage, use that
                *storage
            } else {
                // Color hasn't been seen before - has to be past the end of
                // the register colors because we prefill the map with
                // register colors. Allocate it a new stack storage (or
                // GC stack storage if its a tuple)
                let s =
                    if let Some(ValueType::TupleType(_)) = types.get(&AssignDest::Id(id.clone())) {
                        let stg = Storage::GCStack(curr_gc_stack_offset);
                        curr_gc_stack_offset += WORD_SIZE as i32;
                        stg
                    } else {
                        curr_stack_offset -= WORD_SIZE as i32;
                        Storage::Stack(curr_stack_offset)
                    };

                color_to_storage.insert(*color, s);
                s
            };

            id_to_storage.insert(id.clone(), storage);
        } else if let Location::Reg(r) = location
            && CALLEE_SAVED_REGISTERS.contains(r)
        {
            // Silly hack to make the callee-saved registers work
            // properly if they're used but not assigned to a
            // variable... TODO: Fix this..
            id_to_storage.insert(Identifier::new_ephemeral(), Storage::Reg(*r));
        }
    }

    // Offset is negative because stack grows down. We negate it to get
    // the stack size.
    let stack_size = -curr_stack_offset;
    let gc_stack_size = curr_gc_stack_offset;
    AllocateStorageResult {
        id_to_storage,
        stack_size,
        gc_stack_size,
    }
}

fn replace_arg_with_allocated(
    arg: &mut Arg,
    id_to_stg: &HashMap<Identifier, Storage>,
    stack_offset: i32,
) {
    match arg {
        Arg::Variable(id) => {
            if let Some(storage) = id_to_stg.get(id) {
                *arg = storage.with_stack_offset(stack_offset).to_arg()
            } else {
                panic!("No storage found for variable: {id:?}");
            }
        }
        Arg::Immediate(_) | Arg::Reg(_) | Arg::ByteReg(_) | Arg::Global(_) | Arg::Deref(_, _) => {
            // All of these argument forms should've been *unchanged* by
            // register allocation, as they all refer to absolute
            // things. They do *participate* in regalloc for graph
            // coloring purposes, but they should NOT have been
            // reassigned to a different location
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;

    use test_support::{
        compiler::{
            passes::*,
            pipeline::Pipeline,
            syntax_trees::{ast::*, shared::*, x86},
        },
        x86_interpreter::interpret_x86,
    };

    struct TestCase {
        ast: Module,
        inputs: VecDeque<i64>,
        expected_outputs: VecDeque<i64>,
    }

    fn execute_test_case(mut tc: TestCase) {
        let pipeline = Pipeline {
            ast_passes: vec![
                ASTtoAST::from(TypeCheck),
                ASTtoAST::from(RemoveComplexOperands),
                ASTtoAST::from(TypeCheck),
                ASTtoAST::from(InjectAllocations),
            ],
            ast_to_ir_pass: ASTtoIR::from(TranslateASTtoIR),
            ir_passes: vec![],
            ir_to_x86_pass: IRtoX86::from(TranslateIRtoX86),
            x86_passes: vec![],
        };

        let before_ast = pipeline.run(tc.ast);
        println!("-- AST before RegAlloc:\n{before_ast}");
        let after_ast = RegisterAllocation.run_pass(before_ast);
        println!("-- AST after RegAlloc:\n{after_ast}");

        // Ensure that all the variable arguments have been removed
        for i in after_ast.blocks.iter().map(|x| &x.instrs).flatten() {
            use x86::{Arg, Instr};
            match i {
                Instr::addq(s, d) | Instr::subq(s, d) | Instr::movq(s, d) => {
                    for arg in [s, d] {
                        assert!(!matches!(arg, Arg::Variable(_)));
                    }
                }
                Instr::negq(arg) | Instr::pushq(arg) | Instr::popq(arg) => {
                    assert!(!matches!(arg, Arg::Variable(_)));
                }
                _ => {}
            }
        }

        let with_prelude = PreludeConclusion.run_pass(after_ast);
        let mut outputs = VecDeque::<i64>::new();
        interpret_x86(&with_prelude, &mut tc.inputs, &mut outputs);

        assert_eq!(outputs, tc.expected_outputs);
    }

    #[test]
    fn test_add() {
        execute_test_case(TestCase {
            ast: Module {
                body: vec![Statement::Expr(Expr::Call(
                    Identifier::from("print_int"),
                    vec![Expr::BinaryOp(
                        Box::new(Expr::Constant(Value::I64(40))),
                        BinaryOperator::Add,
                        Box::new(Expr::Constant(Value::I64(2))),
                    )],
                ))],
                types: TypeEnv::new(),
            },
            inputs: VecDeque::new(),
            expected_outputs: VecDeque::from(vec![42]),
        })
    }

    #[test]
    fn test_input() {
        execute_test_case(TestCase {
            ast: Module {
                body: vec![Statement::Expr(Expr::Call(
                    Identifier::from("print_int"),
                    vec![Expr::Call(Identifier::from("read_int"), vec![])],
                ))],
                types: TypeEnv::new(),
            },
            inputs: VecDeque::from(vec![42]),
            expected_outputs: VecDeque::from(vec![42]),
        })
    }

    #[test]
    fn test_subinput() {
        execute_test_case(TestCase {
            ast: Module {
                body: vec![Statement::Expr(Expr::Call(
                    Identifier::from("print_int"),
                    vec![Expr::BinaryOp(
                        Box::new(Expr::Call(Identifier::from("read_int"), vec![])),
                        BinaryOperator::Subtract,
                        Box::new(Expr::Call(Identifier::from("read_int"), vec![])),
                    )],
                ))],
                types: TypeEnv::new(),
            },
            inputs: VecDeque::from(vec![5, 3]),
            expected_outputs: VecDeque::from(vec![2]),
        });
    }

    #[test]
    fn test_zero() {
        execute_test_case(TestCase {
            ast: Module {
                body: vec![Statement::Expr(Expr::Call(
                    Identifier::from("print_int"),
                    vec![Expr::Constant(Value::I64(0))],
                ))],
                types: TypeEnv::new(),
            },
            inputs: VecDeque::from(vec![]),
            expected_outputs: VecDeque::from(vec![0]),
        });
    }

    #[test]
    fn test_nested() {
        execute_test_case(TestCase {
            ast: Module {
                body: vec![Statement::Expr(Expr::Call(
                    Identifier::from("print_int"),
                    vec![Expr::BinaryOp(
                        Box::new(Expr::BinaryOp(
                            Box::new(Expr::Constant(Value::I64(40))),
                            BinaryOperator::Add,
                            Box::new(Expr::Constant(Value::I64(2))),
                        )),
                        BinaryOperator::Add,
                        Box::new(Expr::BinaryOp(
                            Box::new(Expr::Constant(Value::I64(40))),
                            BinaryOperator::Add,
                            Box::new(Expr::Constant(Value::I64(2))),
                        )),
                    )],
                ))],
                types: TypeEnv::new(),
            },
            inputs: VecDeque::from(vec![]),
            expected_outputs: VecDeque::from(vec![84]),
        });
    }

    #[test]
    fn test_mixed() {
        execute_test_case(TestCase {
            ast: Module {
                body: vec![Statement::Expr(Expr::Call(
                    Identifier::from("print_int"),
                    vec![Expr::BinaryOp(
                        Box::new(Expr::BinaryOp(
                            Box::new(Expr::Call(Identifier::from("read_int"), vec![])),
                            BinaryOperator::Add,
                            Box::new(Expr::Constant(Value::I64(2))),
                        )),
                        BinaryOperator::Add,
                        Box::new(Expr::BinaryOp(
                            Box::new(Expr::Constant(Value::I64(40))),
                            BinaryOperator::Add,
                            Box::new(Expr::Constant(Value::I64(2))),
                        )),
                    )],
                ))],
                types: TypeEnv::new(),
            },
            inputs: VecDeque::from(vec![-100]),
            expected_outputs: VecDeque::from(vec![44 - 100]),
        });
    }

    #[test]
    fn test_simple_assignment() {
        execute_test_case(TestCase {
            ast: Module {
                body: vec![
                    Statement::Assign(
                        AssignDest::Id(Identifier::from("x")),
                        Expr::Constant(Value::I64(1000)),
                    ),
                    Statement::Expr(Expr::Call(
                        Identifier::from("print_int"),
                        vec![Expr::Id(Identifier::from("x"))],
                    )),
                ],
                types: TypeEnv::new(),
            },
            inputs: VecDeque::from(vec![]),
            expected_outputs: VecDeque::from(vec![1000]),
        });
    }

    #[test]
    fn test_complex_assignment() {
        // -- Original
        // print((input_int() + 2) + (40 - 2))

        // -- Expected
        // e0 = input_int()
        // e1 = e0 + 2
        // e2 = 40 - 2
        // e3 = e1 + e2
        // print(e3)
        execute_test_case(TestCase {
            ast: Module {
                body: vec![
                    Statement::Assign(
                        AssignDest::Id(Identifier::from("foofoo")),
                        Expr::BinaryOp(
                            Box::new(Expr::BinaryOp(
                                Box::new(Expr::Call(Identifier::from("read_int"), vec![])),
                                BinaryOperator::Add,
                                Box::new(Expr::Constant(Value::I64(2))),
                            )),
                            BinaryOperator::Add,
                            Box::new(Expr::BinaryOp(
                                Box::new(Expr::Constant(Value::I64(40))),
                                BinaryOperator::Subtract,
                                Box::new(Expr::Constant(Value::I64(2))),
                            )),
                        ),
                    ),
                    Statement::Expr(Expr::Call(
                        Identifier::from("print_int"),
                        vec![Expr::Id(Identifier::from("foofoo"))],
                    )),
                ],
                types: TypeEnv::new(),
            },
            inputs: VecDeque::from(vec![10]),
            expected_outputs: VecDeque::from(vec![10 + 2 + 40 - 2]),
        });
    }

    #[test]
    fn test_complex_args() {
        execute_test_case(TestCase {
            ast: Module {
                body: vec![Statement::Expr(Expr::Call(
                    Identifier::from("print_int"),
                    vec![Expr::BinaryOp(
                        Box::new(Expr::BinaryOp(
                            Box::new(Expr::Call(Identifier::from("read_int"), vec![])),
                            BinaryOperator::Add,
                            Box::new(Expr::Constant(Value::I64(2))),
                        )),
                        BinaryOperator::Add,
                        Box::new(Expr::BinaryOp(
                            Box::new(Expr::Constant(Value::I64(40))),
                            BinaryOperator::Subtract,
                            Box::new(Expr::Constant(Value::I64(2))),
                        )),
                    )],
                ))],
                types: TypeEnv::new(),
            },
            inputs: VecDeque::from(vec![10]),
            expected_outputs: VecDeque::from(vec![10 + 2 + 40 - 2]),
        });
    }

    #[test]
    fn test_cascading_assigns() {
        // -- Original
        // foo = input_int(10)        # 10
        // bar = input_int(20) + foo  # 20 + 10 = 30
        // baz = input_int(30) + bar  # 30 + 30 = 60
        // bop = foo + bar + baz      # 10 + 30 + 60 = 100
        // print(input_int(40) + bop) # 40 + 100 = 140

        // -- Expected
        // foo = input_int(10)
        // e0 = input_int(20)
        // bar = e0 + foo
        // e1 = input_int(30)
        // baz = e1 + bar
        // e2 = foo + bar
        // bop = e2 + baz
        // e3 = input_int(40)
        // e4 = e3 + bop
        // print(e4)

        execute_test_case(TestCase {
            ast: Module {
                body: vec![
                    Statement::Assign(
                        AssignDest::Id(Identifier::from("finger")),
                        Expr::Call(Identifier::from("read_int"), vec![]),
                    ),
                    Statement::Assign(
                        AssignDest::Id(Identifier::from("bar")),
                        Expr::BinaryOp(
                            Box::new(Expr::Call(Identifier::from("read_int"), vec![])),
                            BinaryOperator::Add,
                            Box::new(Expr::Id(Identifier::from("finger"))),
                        ),
                    ),
                    Statement::Assign(
                        AssignDest::Id(Identifier::from("baz")),
                        Expr::BinaryOp(
                            Box::new(Expr::Call(Identifier::from("read_int"), vec![])),
                            BinaryOperator::Add,
                            Box::new(Expr::Id(Identifier::from("bar"))),
                        ),
                    ),
                    Statement::Assign(
                        AssignDest::Id(Identifier::from("bop")),
                        Expr::BinaryOp(
                            Box::new(Expr::BinaryOp(
                                Box::new(Expr::Id(Identifier::from("finger"))),
                                BinaryOperator::Add,
                                Box::new(Expr::Id(Identifier::from("bar"))),
                            )),
                            BinaryOperator::Add,
                            Box::new(Expr::Id(Identifier::from("baz"))),
                        ),
                    ),
                    Statement::Expr(Expr::Call(
                        Identifier::from("print_int"),
                        vec![Expr::BinaryOp(
                            Box::new(Expr::Call(Identifier::from("read_int"), vec![])),
                            BinaryOperator::Add,
                            Box::new(Expr::Id(Identifier::from("bop"))),
                        )],
                    )),
                ],
                types: TypeEnv::new(),
            },
            inputs: VecDeque::from(vec![10, 20, 30, 40]),
            expected_outputs: VecDeque::from(vec![10 + (10 + 20) + (10 + 20 + 30) + 40]),
        });
    }

    #[test]
    fn test_force_spills() {
        execute_test_case(TestCase {
        ast: Module { body: vec![
            Statement::Expr(Expr::Call(Identifier::from("print_int"), vec![
                Expr::BinaryOp(Box::new(Expr::Call(Identifier::from("read_int"), vec![])), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Call(Identifier::from("read_int"), vec![])), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Call(Identifier::from("read_int"), vec![])), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Call(Identifier::from("read_int"), vec![])), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Call(Identifier::from("read_int"), vec![])), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Call(Identifier::from("read_int"), vec![])), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Call(Identifier::from("read_int"), vec![])), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Call(Identifier::from("read_int"), vec![])), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Call(Identifier::from("read_int"), vec![])), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Call(Identifier::from("read_int"), vec![])), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Call(Identifier::from("read_int"), vec![])), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Call(Identifier::from("read_int"), vec![])), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Call(Identifier::from("read_int"), vec![])), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Call(Identifier::from("read_int"), vec![])), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Call(Identifier::from("read_int"), vec![])), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Call(Identifier::from("read_int"), vec![])), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Call(Identifier::from("read_int"), vec![])), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Call(Identifier::from("read_int"), vec![])), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Call(Identifier::from("read_int"), vec![])), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Call(Identifier::from("read_int"), vec![])), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Call(Identifier::from("read_int"), vec![])), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Call(Identifier::from("read_int"), vec![])), BinaryOperator::Add,
                Box::new(Expr::Call(Identifier::from("read_int"), vec![])))))))))))))))))))))))))))))))))))))))))))))
            ]))
        ], types: TypeEnv::new() },
        inputs: (0..23).collect(),
        expected_outputs: VecDeque::from([(0..23).sum::<i64>()]),
    });
    }

    #[test]
    fn test_force_spills_constants() {
        execute_test_case(TestCase {
        ast: Module { body: vec![
            Statement::Assign(AssignDest::Id(Identifier::from("x1")), Expr::Constant(Value::I64(1))),
            Statement::Assign(AssignDest::Id(Identifier::from("x2")), Expr::Constant(Value::I64(1))),
            Statement::Assign(AssignDest::Id(Identifier::from("x3")), Expr::Constant(Value::I64(1))),
            Statement::Assign(AssignDest::Id(Identifier::from("x4")), Expr::Constant(Value::I64(1))),
            Statement::Assign(AssignDest::Id(Identifier::from("x5")), Expr::Constant(Value::I64(1))),
            Statement::Assign(AssignDest::Id(Identifier::from("x6")), Expr::Constant(Value::I64(1))),
            Statement::Assign(AssignDest::Id(Identifier::from("x7")), Expr::Constant(Value::I64(1))),
            Statement::Assign(AssignDest::Id(Identifier::from("x8")), Expr::Constant(Value::I64(1))),
            Statement::Assign(AssignDest::Id(Identifier::from("x9")), Expr::Constant(Value::I64(1))),
            Statement::Assign(AssignDest::Id(Identifier::from("x10")), Expr::Constant(Value::I64(1))),
            Statement::Assign(AssignDest::Id(Identifier::from("x11")), Expr::Constant(Value::I64(1))),
            Statement::Assign(AssignDest::Id(Identifier::from("x12")), Expr::Constant(Value::I64(1))),
            Statement::Assign(AssignDest::Id(Identifier::from("x13")), Expr::Constant(Value::I64(1))),
            Statement::Assign(AssignDest::Id(Identifier::from("x14")), Expr::Constant(Value::I64(1))),
            Statement::Assign(AssignDest::Id(Identifier::from("x15")), Expr::Constant(Value::I64(1))),
            Statement::Assign(AssignDest::Id(Identifier::from("x16")), Expr::Constant(Value::I64(1))),
            Statement::Assign(AssignDest::Id(Identifier::from("x17")), Expr::Constant(Value::I64(1))),
            Statement::Assign(AssignDest::Id(Identifier::from("x18")), Expr::Constant(Value::I64(1))),
            Statement::Assign(AssignDest::Id(Identifier::from("x19")), Expr::Constant(Value::I64(1))),
            Statement::Assign(AssignDest::Id(Identifier::from("x20")), Expr::Constant(Value::I64(1))),
            Statement::Assign(AssignDest::Id(Identifier::from("x21")), Expr::Constant(Value::I64(1))),
            Statement::Assign(AssignDest::Id(Identifier::from("x22")), Expr::Constant(Value::I64(1))),
            Statement::Assign(AssignDest::Id(Identifier::from("x23")), Expr::Constant(Value::I64(1))),
            Statement::Expr(Expr::Call(Identifier::from("print_int"), vec![
                         Expr::BinaryOp(Box::new(Expr::Id(Identifier::from("x1"))), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Id(Identifier::from("x2"))), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Id(Identifier::from("x3"))), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Id(Identifier::from("x4"))), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Id(Identifier::from("x5"))), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Id(Identifier::from("x6"))), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Id(Identifier::from("x7"))), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Id(Identifier::from("x8"))), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Id(Identifier::from("x9"))), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Id(Identifier::from("x10"))), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Id(Identifier::from("x11"))), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Id(Identifier::from("x12"))), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Id(Identifier::from("x13"))), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Id(Identifier::from("x14"))), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Id(Identifier::from("x15"))), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Id(Identifier::from("x16"))), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Id(Identifier::from("x17"))), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Id(Identifier::from("x18"))), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Id(Identifier::from("x19"))), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Id(Identifier::from("x20"))), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Id(Identifier::from("x21"))), BinaryOperator::Add,
                Box::new(Expr::BinaryOp(Box::new(Expr::Id(Identifier::from("x22"))), BinaryOperator::Add,
                                        Box::new(Expr::Id(Identifier::from("x23")
            )))))))))))))))))))))))))))))))))))))))))))))
            ]))
        ], types: TypeEnv::new() },
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from([23]),
    });
    }

    #[test]
    fn test_tuple_simple_read() {
        // tup = (10, 20, 30)
        // print_int(tup[1])
        execute_test_case(TestCase {
            ast: Module {
                body: vec![
                    Statement::Assign(
                        AssignDest::Id(Identifier::from("tup")),
                        Expr::Tuple(vec![
                            Expr::Constant(Value::I64(10)),
                            Expr::Constant(Value::I64(20)),
                            Expr::Constant(Value::I64(30)),
                        ]),
                    ),
                    Statement::Expr(Expr::Call(
                        Identifier::from("print_int"),
                        vec![Expr::Subscript(
                            Box::new(Expr::Id(Identifier::from("tup"))),
                            1,
                        )],
                    )),
                ],
                types: TypeEnv::new(),
            },
            inputs: VecDeque::new(),
            expected_outputs: VecDeque::from(vec![20]),
        });
    }

    #[test]
    fn test_tuple_read_all_elements() {
        // tup = (10, 20, 30)
        // print_int(tup[0])
        // print_int(tup[1])
        // print_int(tup[2])
        execute_test_case(TestCase {
            ast: Module {
                body: vec![
                    Statement::Assign(
                        AssignDest::Id(Identifier::from("tup")),
                        Expr::Tuple(vec![
                            Expr::Constant(Value::I64(10)),
                            Expr::Constant(Value::I64(20)),
                            Expr::Constant(Value::I64(30)),
                        ]),
                    ),
                    Statement::Expr(Expr::Call(
                        Identifier::from("print_int"),
                        vec![Expr::Subscript(
                            Box::new(Expr::Id(Identifier::from("tup"))),
                            0,
                        )],
                    )),
                    Statement::Expr(Expr::Call(
                        Identifier::from("print_int"),
                        vec![Expr::Subscript(
                            Box::new(Expr::Id(Identifier::from("tup"))),
                            1,
                        )],
                    )),
                    Statement::Expr(Expr::Call(
                        Identifier::from("print_int"),
                        vec![Expr::Subscript(
                            Box::new(Expr::Id(Identifier::from("tup"))),
                            2,
                        )],
                    )),
                ],
                types: TypeEnv::new(),
            },
            inputs: VecDeque::new(),
            expected_outputs: VecDeque::from(vec![10, 20, 30]),
        });
    }

    #[test]
    fn test_tuple_single_element() {
        // tup = (42,)
        // print_int(tup[0])
        execute_test_case(TestCase {
            ast: Module {
                body: vec![
                    Statement::Assign(
                        AssignDest::Id(Identifier::from("tup")),
                        Expr::Tuple(vec![Expr::Constant(Value::I64(42))]),
                    ),
                    Statement::Expr(Expr::Call(
                        Identifier::from("print_int"),
                        vec![Expr::Subscript(
                            Box::new(Expr::Id(Identifier::from("tup"))),
                            0,
                        )],
                    )),
                ],
                types: TypeEnv::new(),
            },
            inputs: VecDeque::new(),
            expected_outputs: VecDeque::from(vec![42]),
        });
    }

    #[test]
    fn test_tuple_two_alive() {
        // t1 = (1, 2)
        // t2 = (3, 4)
        // print_int(t1[0] + t2[1])
        execute_test_case(TestCase {
            ast: Module {
                body: vec![
                    Statement::Assign(
                        AssignDest::Id(Identifier::from("t1")),
                        Expr::Tuple(vec![
                            Expr::Constant(Value::I64(1)),
                            Expr::Constant(Value::I64(2)),
                        ]),
                    ),
                    Statement::Assign(
                        AssignDest::Id(Identifier::from("t2")),
                        Expr::Tuple(vec![
                            Expr::Constant(Value::I64(3)),
                            Expr::Constant(Value::I64(4)),
                        ]),
                    ),
                    Statement::Expr(Expr::Call(
                        Identifier::from("print_int"),
                        vec![Expr::BinaryOp(
                            Box::new(Expr::Subscript(
                                Box::new(Expr::Id(Identifier::from("t1"))),
                                0,
                            )),
                            BinaryOperator::Add,
                            Box::new(Expr::Subscript(
                                Box::new(Expr::Id(Identifier::from("t2"))),
                                1,
                            )),
                        )],
                    )),
                ],
                types: TypeEnv::new(),
            },
            inputs: VecDeque::new(),
            expected_outputs: VecDeque::from(vec![5]),
        });
    }

    #[test]
    fn test_tuple_subscript_write() {
        // tup = (1, 2, 3)
        // tup[0] = 99
        // print_int(tup[0])
        // print_int(tup[1])
        execute_test_case(TestCase {
            ast: Module {
                body: vec![
                    Statement::Assign(
                        AssignDest::Id(Identifier::from("tup")),
                        Expr::Tuple(vec![
                            Expr::Constant(Value::I64(1)),
                            Expr::Constant(Value::I64(2)),
                            Expr::Constant(Value::I64(3)),
                        ]),
                    ),
                    Statement::Assign(
                        AssignDest::Subscript(Identifier::from("tup"), 0),
                        Expr::Constant(Value::I64(99)),
                    ),
                    Statement::Expr(Expr::Call(
                        Identifier::from("print_int"),
                        vec![Expr::Subscript(
                            Box::new(Expr::Id(Identifier::from("tup"))),
                            0,
                        )],
                    )),
                    Statement::Expr(Expr::Call(
                        Identifier::from("print_int"),
                        vec![Expr::Subscript(
                            Box::new(Expr::Id(Identifier::from("tup"))),
                            1,
                        )],
                    )),
                ],
                types: TypeEnv::new(),
            },
            inputs: VecDeque::new(),
            expected_outputs: VecDeque::from(vec![99, 2]),
        });
    }

    #[test]
    fn test_tuple_element_arithmetic() {
        // tup = (10, 20)
        // x = tup[0] + tup[1]
        // print_int(x)
        execute_test_case(TestCase {
            ast: Module {
                body: vec![
                    Statement::Assign(
                        AssignDest::Id(Identifier::from("tup")),
                        Expr::Tuple(vec![
                            Expr::Constant(Value::I64(10)),
                            Expr::Constant(Value::I64(20)),
                        ]),
                    ),
                    Statement::Assign(
                        AssignDest::Id(Identifier::from("x")),
                        Expr::BinaryOp(
                            Box::new(Expr::Subscript(
                                Box::new(Expr::Id(Identifier::from("tup"))),
                                0,
                            )),
                            BinaryOperator::Add,
                            Box::new(Expr::Subscript(
                                Box::new(Expr::Id(Identifier::from("tup"))),
                                1,
                            )),
                        ),
                    ),
                    Statement::Expr(Expr::Call(
                        Identifier::from("print_int"),
                        vec![Expr::Id(Identifier::from("x"))],
                    )),
                ],
                types: TypeEnv::new(),
            },
            inputs: VecDeque::new(),
            expected_outputs: VecDeque::from(vec![30]),
        });
    }

    #[test]
    fn test_tuple_with_input() {
        // tup = (read_int(), read_int(), read_int())
        // print_int(tup[0] + tup[1] + tup[2])
        execute_test_case(TestCase {
            ast: Module {
                body: vec![
                    Statement::Assign(
                        AssignDest::Id(Identifier::from("tup")),
                        Expr::Tuple(vec![
                            Expr::Call(Identifier::from("read_int"), vec![]),
                            Expr::Call(Identifier::from("read_int"), vec![]),
                            Expr::Call(Identifier::from("read_int"), vec![]),
                        ]),
                    ),
                    Statement::Expr(Expr::Call(
                        Identifier::from("print_int"),
                        vec![Expr::BinaryOp(
                            Box::new(Expr::Subscript(
                                Box::new(Expr::Id(Identifier::from("tup"))),
                                0,
                            )),
                            BinaryOperator::Add,
                            Box::new(Expr::BinaryOp(
                                Box::new(Expr::Subscript(
                                    Box::new(Expr::Id(Identifier::from("tup"))),
                                    1,
                                )),
                                BinaryOperator::Add,
                                Box::new(Expr::Subscript(
                                    Box::new(Expr::Id(Identifier::from("tup"))),
                                    2,
                                )),
                            )),
                        )],
                    )),
                ],
                types: TypeEnv::new(),
            },
            inputs: VecDeque::from(vec![10, 20, 30]),
            expected_outputs: VecDeque::from(vec![60]),
        });
    }

    #[test]
    fn test_tuple_with_scalars_register_pressure() {
        // x1..x10 = 1..10
        // tup = (100, 200)
        // print_int(x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + tup[0])
        let mut body: Vec<Statement> = (1..=10)
            .map(|i| {
                Statement::Assign(
                    AssignDest::Id(Identifier::from(format!("x{i}").as_str())),
                    Expr::Constant(Value::I64(i)),
                )
            })
            .collect();

        body.push(Statement::Assign(
            AssignDest::Id(Identifier::from("tup")),
            Expr::Tuple(vec![
                Expr::Constant(Value::I64(100)),
                Expr::Constant(Value::I64(200)),
            ]),
        ));

        // Build x1 + x2 + ... + x10 + tup[0]
        let mut sum_expr: Expr = Expr::Id(Identifier::from("x1"));
        for i in 2..=10 {
            sum_expr = Expr::BinaryOp(
                Box::new(sum_expr),
                BinaryOperator::Add,
                Box::new(Expr::Id(Identifier::from(format!("x{i}").as_str()))),
            );
        }
        sum_expr = Expr::BinaryOp(
            Box::new(sum_expr),
            BinaryOperator::Add,
            Box::new(Expr::Subscript(
                Box::new(Expr::Id(Identifier::from("tup"))),
                0,
            )),
        );

        body.push(Statement::Expr(Expr::Call(
            Identifier::from("print_int"),
            vec![sum_expr],
        )));

        execute_test_case(TestCase {
            ast: Module {
                body,
                types: TypeEnv::new(),
            },
            inputs: VecDeque::new(),
            // 1+2+...+10 = 55, + 100 = 155
            expected_outputs: VecDeque::from(vec![155]),
        });
    }

    #[test]
    fn test_tuple_in_loop() {
        // tup = (0, 1)
        // i = 3
        // while i > 0 {
        //     print_int(tup[0] + tup[1])
        //     i = i - 1
        // }
        execute_test_case(TestCase {
            ast: Module {
                body: vec![
                    Statement::Assign(
                        AssignDest::Id(Identifier::from("tup")),
                        Expr::Tuple(vec![
                            Expr::Constant(Value::I64(7)),
                            Expr::Constant(Value::I64(3)),
                        ]),
                    ),
                    Statement::Assign(
                        AssignDest::Id(Identifier::from("i")),
                        Expr::Constant(Value::I64(3)),
                    ),
                    Statement::WhileLoop(
                        Expr::BinaryOp(
                            Box::new(Expr::Id(Identifier::from("i"))),
                            BinaryOperator::Greater,
                            Box::new(Expr::Constant(Value::I64(0))),
                        ),
                        vec![
                            Statement::Expr(Expr::Call(
                                Identifier::from("print_int"),
                                vec![Expr::BinaryOp(
                                    Box::new(Expr::Subscript(
                                        Box::new(Expr::Id(Identifier::from("tup"))),
                                        0,
                                    )),
                                    BinaryOperator::Add,
                                    Box::new(Expr::Subscript(
                                        Box::new(Expr::Id(Identifier::from("tup"))),
                                        1,
                                    )),
                                )],
                            )),
                            Statement::Assign(
                                AssignDest::Id(Identifier::from("i")),
                                Expr::BinaryOp(
                                    Box::new(Expr::Id(Identifier::from("i"))),
                                    BinaryOperator::Subtract,
                                    Box::new(Expr::Constant(Value::I64(1))),
                                ),
                            ),
                        ],
                    ),
                ],
                types: TypeEnv::new(),
            },
            inputs: VecDeque::new(),
            expected_outputs: VecDeque::from(vec![10, 10, 10]),
        });
    }

    #[test]
    fn test_tuple_multiple_simultaneous() {
        // t1 = (1, 2, 3)
        // t2 = (10, 20, 30)
        // t3 = (100, 200, 300)
        // print_int(t1[0] + t2[1] + t3[2])
        execute_test_case(TestCase {
            ast: Module {
                body: vec![
                    Statement::Assign(
                        AssignDest::Id(Identifier::from("t1")),
                        Expr::Tuple(vec![
                            Expr::Constant(Value::I64(1)),
                            Expr::Constant(Value::I64(2)),
                            Expr::Constant(Value::I64(3)),
                        ]),
                    ),
                    Statement::Assign(
                        AssignDest::Id(Identifier::from("t2")),
                        Expr::Tuple(vec![
                            Expr::Constant(Value::I64(10)),
                            Expr::Constant(Value::I64(20)),
                            Expr::Constant(Value::I64(30)),
                        ]),
                    ),
                    Statement::Assign(
                        AssignDest::Id(Identifier::from("t3")),
                        Expr::Tuple(vec![
                            Expr::Constant(Value::I64(100)),
                            Expr::Constant(Value::I64(200)),
                            Expr::Constant(Value::I64(300)),
                        ]),
                    ),
                    Statement::Expr(Expr::Call(
                        Identifier::from("print_int"),
                        vec![Expr::BinaryOp(
                            Box::new(Expr::Subscript(
                                Box::new(Expr::Id(Identifier::from("t1"))),
                                0,
                            )),
                            BinaryOperator::Add,
                            Box::new(Expr::BinaryOp(
                                Box::new(Expr::Subscript(
                                    Box::new(Expr::Id(Identifier::from("t2"))),
                                    1,
                                )),
                                BinaryOperator::Add,
                                Box::new(Expr::Subscript(
                                    Box::new(Expr::Id(Identifier::from("t3"))),
                                    2,
                                )),
                            )),
                        )],
                    )),
                ],
                types: TypeEnv::new(),
            },
            inputs: VecDeque::new(),
            // 1 + 20 + 300 = 321
            expected_outputs: VecDeque::from(vec![321]),
        });
    }

    #[test]
    fn test_tuple_mutate_and_sum() {
        // tup = (1, 2, 3)
        // tup[0] = tup[1] + tup[2]   // tup = (5, 2, 3)
        // tup[1] = tup[0] + tup[2]   // tup = (5, 8, 3)
        // print_int(tup[0])
        // print_int(tup[1])
        // print_int(tup[2])
        execute_test_case(TestCase {
            ast: Module {
                body: vec![
                    Statement::Assign(
                        AssignDest::Id(Identifier::from("tup")),
                        Expr::Tuple(vec![
                            Expr::Constant(Value::I64(1)),
                            Expr::Constant(Value::I64(2)),
                            Expr::Constant(Value::I64(3)),
                        ]),
                    ),
                    Statement::Assign(
                        AssignDest::Subscript(Identifier::from("tup"), 0),
                        Expr::BinaryOp(
                            Box::new(Expr::Subscript(
                                Box::new(Expr::Id(Identifier::from("tup"))),
                                1,
                            )),
                            BinaryOperator::Add,
                            Box::new(Expr::Subscript(
                                Box::new(Expr::Id(Identifier::from("tup"))),
                                2,
                            )),
                        ),
                    ),
                    Statement::Assign(
                        AssignDest::Subscript(Identifier::from("tup"), 1),
                        Expr::BinaryOp(
                            Box::new(Expr::Subscript(
                                Box::new(Expr::Id(Identifier::from("tup"))),
                                0,
                            )),
                            BinaryOperator::Add,
                            Box::new(Expr::Subscript(
                                Box::new(Expr::Id(Identifier::from("tup"))),
                                2,
                            )),
                        ),
                    ),
                    Statement::Expr(Expr::Call(
                        Identifier::from("print_int"),
                        vec![Expr::Subscript(
                            Box::new(Expr::Id(Identifier::from("tup"))),
                            0,
                        )],
                    )),
                    Statement::Expr(Expr::Call(
                        Identifier::from("print_int"),
                        vec![Expr::Subscript(
                            Box::new(Expr::Id(Identifier::from("tup"))),
                            1,
                        )],
                    )),
                    Statement::Expr(Expr::Call(
                        Identifier::from("print_int"),
                        vec![Expr::Subscript(
                            Box::new(Expr::Id(Identifier::from("tup"))),
                            2,
                        )],
                    )),
                ],
                types: TypeEnv::new(),
            },
            inputs: VecDeque::new(),
            expected_outputs: VecDeque::from(vec![5, 8, 3]),
        });
    }
}
