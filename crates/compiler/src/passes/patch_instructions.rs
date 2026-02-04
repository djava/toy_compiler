use crate::{passes::X86Pass, x86_ast::*};

pub struct PatchInstructions;

impl X86Pass for PatchInstructions {
    fn run_pass(self, mut m: X86Program) -> X86Program {
        let main_instrs = &mut m
            .blocks
            .iter_mut()
            .find(|block| block.label == Directive::Label(Identifier::from("user_entry")))
            .expect("Didn't find an entry function")
            .instrs;

        let mut new_instrs = vec![];
        // Reserve for worst case because why not
        new_instrs.reserve(main_instrs.len() * 2);

        for i in main_instrs.iter_mut() {
            match &i {
                // If both args to an instr are derefs, we need to add a
                // patch instruction
                Instr::addq(s, d) | Instr::subq(s, d) | Instr::movq(s, d)
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
                Instr::addq(s, d) | Instr::subq(s, d) | Instr::movq(s, d)
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

                _ => new_instrs.push(i.clone()),
            }
        }

        *main_instrs = new_instrs;
        m
    }
}

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;

    use test_support::{
        compiler::{ast::*, passes::*, pipeline::Pipeline, x86_ast},
        x86_interpreter::interpret_x86,
    };

    struct TestCase {
        ast: Module,
        inputs: VecDeque<i64>,
        expected_outputs: VecDeque<i64>,
    }

    fn check_invariants(p: &x86_ast::X86Program) {
        for i in p.blocks.iter().map(|x| &x.instrs).flatten() {
            use x86_ast::{Arg, Instr};
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
    fn test_simple_assignment_imm64() {
        execute_test_case(TestCase {
            ast: Module {
                body: vec![
                    Statement::Assign(
                        AssignDest::Id(Identifier::from("x")),
                        Expr::Constant(Value::I64(i64::MAX)),
                    ),
                    Statement::Expr(Expr::Call(
                        Identifier::from("print_int"),
                        vec![Expr::Id(Identifier::from("x"))],
                    )),
                ],
                types: TypeEnv::new(),
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
                        AssignDest::Id(Identifier::from("foo")),
                        Expr::Call(Identifier::from("read_int"), vec![]),
                    ),
                    Statement::Assign(
                        AssignDest::Id(Identifier::from("bar")),
                        Expr::BinaryOp(
                            Box::new(Expr::Call(Identifier::from("read_int"), vec![])),
                            BinaryOperator::Add,
                            Box::new(Expr::Id(Identifier::from("foo"))),
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
                                Box::new(Expr::Id(Identifier::from("foo"))),
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
}
