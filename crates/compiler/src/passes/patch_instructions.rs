use crate::{passes::X86Pass, syntax_trees::x86::*};

pub struct PatchInstructions;

impl X86Pass for PatchInstructions {
    fn run_pass(self, mut m: X86Program) -> X86Program {
        for f in m.functions.iter_mut() {
            for b in f.blocks.iter_mut() {
                patch_block(b);
            }
        }

        m
    }
}

fn patch_block(b: &mut Block) {
    let instrs = &mut b.instrs;

    let mut new_instrs = vec![];
    // Reserve for worst case because why not
    new_instrs.reserve(instrs.len() * 2);

    for i in instrs.iter_mut() {
        match &i {
            // If both args to an instr are derefs, we need to add a
            // patch instruction
            Instr::addq(s, d)
            | Instr::subq(s, d)
            | Instr::movq(s, d)
            | Instr::xorq(s, d)
            | Instr::cmpq(s, d)
            | Instr::sarq(s, d)
            | Instr::salq(s, d)
            | Instr::andq(s, d)
                if matches!(s, Arg::Deref(_, _)) && matches!(d, Arg::Deref(_, _)) =>
            {
                new_instrs.push(Instr::movq(s.clone(), Arg::Reg(Register::rax)));
                new_instrs.push(match &i {
                    Instr::addq(_, dest) => Instr::addq(Arg::Reg(Register::rax), dest.clone()),
                    Instr::subq(_, dest) => Instr::subq(Arg::Reg(Register::rax), dest.clone()),
                    Instr::movq(_, dest) => Instr::movq(Arg::Reg(Register::rax), dest.clone()),
                    _ => unreachable!(),
                });
            }

            // If the instruction has an immediate > 32 bits and
            // also accesses memory, need a patch instr
            Instr::addq(s, d)
            | Instr::subq(s, d)
            | Instr::movq(s, d)
            | Instr::xorq(s, d)
            | Instr::cmpq(s, d)
            | Instr::sarq(s, d)
            | Instr::salq(s, d)
            | Instr::andq(s, d)
                if matches!(s, Arg::Immediate(v) if i32::try_from(*v).is_err())
                    && matches!(d, Arg::Deref(_, _)) =>
            {
                new_instrs.push(Instr::movq(s.clone(), Arg::Reg(Register::rax)));
                new_instrs.push(match &i {
                    Instr::addq(_, dest) => Instr::addq(Arg::Reg(Register::rax), dest.clone()),
                    Instr::subq(_, dest) => Instr::subq(Arg::Reg(Register::rax), dest.clone()),
                    Instr::movq(_, dest) => Instr::movq(Arg::Reg(Register::rax), dest.clone()),
                    _ => unreachable!(),
                });
            }

            Instr::movq(s, d) if s == d => {
                // Trival mov to itself, don't keep this instruction
            }

            Instr::cmpq(s, imm @ Arg::Immediate(_)) => {
                // Second arg of cmpq can't be an immediate
                new_instrs.push(Instr::movq(imm.clone(), Arg::Reg(Register::rax)));
                new_instrs.push(Instr::cmpq(s.clone(), Arg::Reg(Register::rax)));
            }

            Instr::sarq(s, _) | Instr::salq(s, _) if matches!(s, Arg::Reg(_)) => {
                // TODO: Need to move `s` into `%cl` to do variable
                // shifts (or set to 0 or something if s > 64)
                todo!("Non-constant shifts aren't implemented");
            }

            Instr::imulq(s, d) if !matches!(d, Arg::Reg(_)) => {
                // Dest of imulq must be a register, add a patch
                // through rax to make it so
                new_instrs.extend([
                    Instr::movq(d.clone(), Arg::Reg(Register::rax)),
                    Instr::imulq(s.clone(), Arg::Reg(Register::rax)),
                    Instr::movq(Arg::Reg(Register::rax), d.clone()),
                ]);
            }

            _ => new_instrs.push(i.clone()),
        }
    }

    *instrs = new_instrs;
}

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;

    use indexmap::IndexMap;
    use crate::utils::t_id;
    use test_support::{
        compiler::{
            constants::LABEL_MAIN,
            passes::*,
            pipeline::Pipeline,
            syntax_trees::{ast::*, shared::*, x86},
        },
        x86_interpreter::interpret_x86,
    };

    struct TestCase {
        ast: Program,
        inputs: VecDeque<i64>,
        expected_outputs: VecDeque<i64>,
    }

    fn check_invariants(p: &x86::X86Program) {
        for i in p.functions.iter().map(|f| &f.blocks).flatten().map(|x| &x.instrs).flatten() {
            use x86::{Arg, Instr};
            match i {
                Instr::addq(s, d)
                | Instr::subq(s, d)
                | Instr::movq(s, d)
                | Instr::xorq(s, d)
                | Instr::cmpq(s, d)
                | Instr::sarq(s, d)
                | Instr::salq(s, d)
                | Instr::andq(s, d)
                    if matches!(s, Arg::Deref(_, _)) && matches!(d, Arg::Deref(_, _)) =>
                {
                    panic!("PatchInstructions should remove all double-memory-access instructions");
                }
                Instr::addq(s, d)
                | Instr::subq(s, d)
                | Instr::movq(s, d)
                | Instr::xorq(s, d)
                | Instr::cmpq(s, d)
                | Instr::sarq(s, d)
                | Instr::salq(s, d)
                | Instr::andq(s, d)
                    if matches!(s, Arg::Immediate(v) if i32::try_from(*v).is_err())
                        && matches!(d, Arg::Deref(_, _)) =>
                {
                    panic!(
                        "PatchInstructions should remove all memory-access + 64-bit-imm instructions"
                    );
                }
                Instr::movq(s, d) if s == d => {
                    panic!("PatchInstructions should remove all trivial memory accesses");
                }
                Instr::cmpq(_, d) if matches!(d, Arg::Immediate(_)) => {
                    panic!("PatchInstructions should remove all cmpq's with Imm d");
                }
                Instr::imulq(_, d) if !matches!(d, Arg::Reg(_)) => {
                    panic!("PatchInstructions should ensure imulq dest is always a register");
                }
                _ => {}
            }
        }
    }

    fn execute_test_case(mut tc: TestCase) {
        let pipeline = Pipeline {
            ast_passes: vec![
                ASTtoAST::from(TypeCheck),
                ASTtoAST::from(RemoveComplexOperands),
            ],
            ast_to_ir_pass: ASTtoIR::from(TranslateASTtoIR),
            ir_passes: vec![],
            ir_to_x86_pass: IRtoX86::from(TranslateIRtoX86),
            x86_passes: vec![X86toX86::from(PatchInstructions)],
        };

        let before_ast = pipeline.run(tc.ast);
        println!("-- AST before PatchInstr:\n{before_ast}");
        let after_ast = PatchInstructions.run_pass(before_ast);
        println!("-- AST after PatchInstr:\n{after_ast}");

        check_invariants(&after_ast);

        let with_prelude = PreludeConclusion.run_pass(after_ast);
        let mut outputs = VecDeque::<i64>::new();
        interpret_x86(&with_prelude, &mut tc.inputs, &mut outputs);

        assert_eq!(outputs, tc.expected_outputs);
    }

    #[test]
    fn test_add() {
        execute_test_case(TestCase {
            ast: Program {
                functions: vec![Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![Statement::Expr(Expr::Call(
                        t_id!("print_int"),
                        vec![Expr::BinaryOp(
                            Box::new(Expr::Constant(Value::I64(40))),
                            BinaryOperator::Add,
                            Box::new(Expr::Constant(Value::I64(2))),
                        )],
                    ))],
                    types: TypeEnv::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: TypeEnv::new(),
            },
            inputs: VecDeque::new(),
            expected_outputs: VecDeque::from(vec![42]),
        })
    }

    #[test]
    fn test_input() {
        execute_test_case(TestCase {
            ast: Program {
                functions: vec![Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![Statement::Expr(Expr::Call(
                        t_id!("print_int"),
                        vec![Expr::Call(t_id!("read_int"), vec![])],
                    ))],
                    types: TypeEnv::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: TypeEnv::new(),
            },
            inputs: VecDeque::from(vec![42]),
            expected_outputs: VecDeque::from(vec![42]),
        })
    }

    #[test]
    fn test_subinput() {
        execute_test_case(TestCase {
            ast: Program {
                functions: vec![Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![Statement::Expr(Expr::Call(
                        t_id!("print_int"),
                        vec![Expr::BinaryOp(
                            Box::new(Expr::Call(t_id!("read_int"), vec![])),
                            BinaryOperator::Subtract,
                            Box::new(Expr::Call(t_id!("read_int"), vec![])),
                        )],
                    ))],
                    types: TypeEnv::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: TypeEnv::new(),
            },
            inputs: VecDeque::from(vec![5, 3]),
            expected_outputs: VecDeque::from(vec![2]),
        });
    }

    #[test]
    fn test_zero() {
        execute_test_case(TestCase {
            ast: Program {
                functions: vec![Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![Statement::Expr(Expr::Call(
                        t_id!("print_int"),
                        vec![Expr::Constant(Value::I64(0))],
                    ))],
                    types: TypeEnv::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: TypeEnv::new(),
            },
            inputs: VecDeque::from(vec![]),
            expected_outputs: VecDeque::from(vec![0]),
        });
    }

    #[test]
    fn test_nested() {
        execute_test_case(TestCase {
            ast: Program {
                functions: vec![Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![Statement::Expr(Expr::Call(
                        t_id!("print_int"),
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
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: TypeEnv::new(),
            },
            inputs: VecDeque::from(vec![]),
            expected_outputs: VecDeque::from(vec![84]),
        });
    }

    #[test]
    fn test_mixed() {
        execute_test_case(TestCase {
            ast: Program {
                functions: vec![Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![Statement::Expr(Expr::Call(
                        t_id!("print_int"),
                        vec![Expr::BinaryOp(
                            Box::new(Expr::BinaryOp(
                                Box::new(Expr::Call(t_id!("read_int"), vec![])),
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
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: TypeEnv::new(),
            },
            inputs: VecDeque::from(vec![-100]),
            expected_outputs: VecDeque::from(vec![44 - 100]),
        });
    }

    #[test]
    fn test_simple_assignment() {
        execute_test_case(TestCase {
            ast: Program {
                functions: vec![Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![
                        Statement::Assign(
                            AssignDest::Id(t_id!("x")),
                            Expr::Constant(Value::I64(1000)),
                        ),
                        Statement::Expr(Expr::Call(t_id!("print_int"), vec![Expr::Id(t_id!("x"))])),
                    ],
                    types: TypeEnv::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: TypeEnv::new(),
            },
            inputs: VecDeque::from(vec![]),
            expected_outputs: VecDeque::from(vec![1000]),
        });
    }

    #[test]
    fn test_simple_assignment_imm64() {
        execute_test_case(TestCase {
            ast: Program {
                functions: vec![Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![
                        Statement::Assign(
                            AssignDest::Id(t_id!("x")),
                            Expr::Constant(Value::I64(i64::MAX)),
                        ),
                        Statement::Expr(Expr::Call(t_id!("print_int"), vec![Expr::Id(t_id!("x"))])),
                    ],
                    types: TypeEnv::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: TypeEnv::new(),
            },
            inputs: VecDeque::from(vec![]),
            expected_outputs: VecDeque::from(vec![i64::MAX]),
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
            ast: Program {
                functions: vec![Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![
                        Statement::Assign(
                            AssignDest::Id(t_id!("foofoo")),
                            Expr::BinaryOp(
                                Box::new(Expr::BinaryOp(
                                    Box::new(Expr::Call(t_id!("read_int"), vec![])),
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
                            t_id!("print_int"),
                            vec![Expr::Id(t_id!("foofoo"))],
                        )),
                    ],
                    types: TypeEnv::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: TypeEnv::new(),
            },
            inputs: VecDeque::from(vec![10]),
            expected_outputs: VecDeque::from(vec![10 + 2 + 40 - 2]),
        });
    }

    #[test]
    fn test_complex_args() {
        execute_test_case(TestCase {
            ast: Program {
                functions: vec![Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![Statement::Expr(Expr::Call(
                        t_id!("print_int"),
                        vec![Expr::BinaryOp(
                            Box::new(Expr::BinaryOp(
                                Box::new(Expr::Call(t_id!("read_int"), vec![])),
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
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: TypeEnv::new(),
            },
            inputs: VecDeque::from(vec![10]),
            expected_outputs: VecDeque::from(vec![10 + 2 + 40 - 2]),
        });
    }

    #[test]
    fn test_multiply_simple() {
        execute_test_case(TestCase {
            ast: Program {
                functions: vec![Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![Statement::Expr(Expr::Call(
                        t_id!("print_int"),
                        vec![Expr::BinaryOp(
                            Box::new(Expr::Call(t_id!("read_int"), vec![])),
                            BinaryOperator::Multiply,
                            Box::new(Expr::Call(t_id!("read_int"), vec![])),
                        )],
                    ))],
                    types: TypeEnv::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: TypeEnv::new(),
            },
            inputs: VecDeque::from(vec![6, 7]),
            expected_outputs: VecDeque::from(vec![42]),
        });
    }

    #[test]
    fn test_multiply_constants() {
        execute_test_case(TestCase {
            ast: Program {
                functions: vec![Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![Statement::Expr(Expr::Call(
                        t_id!("print_int"),
                        vec![Expr::BinaryOp(
                            Box::new(Expr::Constant(Value::I64(5))),
                            BinaryOperator::Multiply,
                            Box::new(Expr::Constant(Value::I64(9))),
                        )],
                    ))],
                    types: TypeEnv::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: TypeEnv::new(),
            },
            inputs: VecDeque::new(),
            expected_outputs: VecDeque::from(vec![45]),
        });
    }

    #[test]
    fn test_multiply_with_spilled_dest() {
        // Many variables to force spills, then multiply to exercise
        // the imulq patching path where dest is a stack location
        let mut body: Vec<Statement> = (1..=15)
            .map(|i| {
                Statement::Assign(
                    AssignDest::Id(t_id!(format!("v{i}").as_str())),
                    Expr::Call(t_id!("read_int"), vec![]),
                )
            })
            .collect();

        // Multiply two of them - if dest gets spilled, patch_instructions
        // must route through rax
        body.push(Statement::Assign(
            AssignDest::Id(t_id!("result")),
            Expr::BinaryOp(
                Box::new(Expr::Id(t_id!("v1"))),
                BinaryOperator::Multiply,
                Box::new(Expr::Id(t_id!("v2"))),
            ),
        ));

        // Use all variables to keep them alive across the multiply
        let mut sum_expr: Expr = Expr::Id(t_id!("v1"));
        for i in 2..=15 {
            sum_expr = Expr::BinaryOp(
                Box::new(sum_expr),
                BinaryOperator::Add,
                Box::new(Expr::Id(t_id!(format!("v{i}").as_str()))),
            );
        }
        sum_expr = Expr::BinaryOp(
            Box::new(sum_expr),
            BinaryOperator::Add,
            Box::new(Expr::Id(t_id!("result"))),
        );

        body.push(Statement::Expr(Expr::Call(
            t_id!("print_int"),
            vec![sum_expr],
        )));

        execute_test_case(TestCase {
            ast: Program {
                functions: vec![Function {
                    name: t_id!(LABEL_MAIN),
                    body,
                    types: TypeEnv::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: TypeEnv::new(),
            },
            inputs: (1..=15).collect(),
            // sum(1..=15) = 120, result = 1*2 = 2, total = 122
            expected_outputs: VecDeque::from(vec![120 + 2]),
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
            ast: Program {
                functions: vec![Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![
                        Statement::Assign(
                            AssignDest::Id(t_id!("foo")),
                            Expr::Call(t_id!("read_int"), vec![]),
                        ),
                        Statement::Assign(
                            AssignDest::Id(t_id!("bar")),
                            Expr::BinaryOp(
                                Box::new(Expr::Call(t_id!("read_int"), vec![])),
                                BinaryOperator::Add,
                                Box::new(Expr::Id(t_id!("foo"))),
                            ),
                        ),
                        Statement::Assign(
                            AssignDest::Id(t_id!("baz")),
                            Expr::BinaryOp(
                                Box::new(Expr::Call(t_id!("read_int"), vec![])),
                                BinaryOperator::Add,
                                Box::new(Expr::Id(t_id!("bar"))),
                            ),
                        ),
                        Statement::Assign(
                            AssignDest::Id(t_id!("bop")),
                            Expr::BinaryOp(
                                Box::new(Expr::BinaryOp(
                                    Box::new(Expr::Id(t_id!("foo"))),
                                    BinaryOperator::Add,
                                    Box::new(Expr::Id(t_id!("bar"))),
                                )),
                                BinaryOperator::Add,
                                Box::new(Expr::Id(t_id!("baz"))),
                            ),
                        ),
                        Statement::Expr(Expr::Call(
                            t_id!("print_int"),
                            vec![Expr::BinaryOp(
                                Box::new(Expr::Call(t_id!("read_int"), vec![])),
                                BinaryOperator::Add,
                                Box::new(Expr::Id(t_id!("bop"))),
                            )],
                        )),
                    ],
                    types: TypeEnv::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: TypeEnv::new(),
            },
            inputs: VecDeque::from(vec![10, 20, 30, 40]),
            expected_outputs: VecDeque::from(vec![10 + (10 + 20) + (10 + 20 + 30) + 40]),
        });
    }
}
