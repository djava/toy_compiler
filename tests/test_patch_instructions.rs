mod infra;
use std::sync::Arc;
use std::collections::VecDeque;

use cs4999_compiler::{ast::*, passes::*, pipeline::Pipeline, x86_ast};

use crate::infra::{ast_type_check::type_check, x86_interpreter::interpret_x86};

struct TestCase {
    ast: Module,
    inputs: VecDeque<i64>,
    expected_outputs: VecDeque<i64>,
}

fn execute_test_case(mut tc: TestCase) {
    println!("\n==================");

    type_check(&tc.ast);
    println!("Type-check passed on source");

    let pipeline = Pipeline {
        ast_passes: vec![ASTtoAST::from(RemoveComplexOperands)],
        ast_to_ir_pass: todo!(),
        ir_passes: todo!(),
        ir_to_x86_pass: IRtoX86::from(TranslateIRtoX86),
        x86_passes: vec![X86toX86::from(PatchInstructions)],
    };

    let before_ast = pipeline.run(tc.ast);
    println!("-- AST before PatchInstr:\n{before_ast}");
    let after_ast = PatchInstructions.run_pass(before_ast);
    println!("-- AST after PatchInstr:\n{after_ast}");

    // Ensure that all the variable arguments have been removed
    for i in after_ast.blocks.iter().map(|x| &x.1).flatten() {
        use x86_ast::{Arg, Instr};
        match i {
            Instr::addq(s, d) | Instr::subq(s, d) | Instr::movq(s, d)
                if matches!(s, Arg::Deref(_, _)) && matches!(d, Arg::Deref(_, _)) =>
            {
                panic!("PatchInstructions should remove all double-memory-access instructions");
            }
            Instr::addq(s, d) | Instr::subq(s, d) | Instr::movq(s, d)
                if matches!(s, Arg::Immediate(v) if i32::try_from(*v).is_err())
                    && matches!(d, Arg::Deref(_, _)) =>
            {
                panic!(
                    "PatchInstructions should remove all memory-access + 64-bit-imm instructions"
                );
            }
            _ => {}
        }
    }

    let mut outputs = VecDeque::<i64>::new();
    interpret_x86(&after_ast, &mut tc.inputs, &mut outputs);

    assert_eq!(outputs, tc.expected_outputs);
}

#[test]
fn test_patch_instructions_add() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![Statement::Expr(Expr::Call(
            Identifier::Named(Arc::from("print_int")),
            vec![Expr::BinaryOp(
                Box::new(Expr::Constant(Value::I64(40))),
                BinaryOperator::Add,
                Box::new(Expr::Constant(Value::I64(2))),
            )],
        ))]),
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![42]),
    })
}

#[test]
fn test_patch_instructions_input() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![Statement::Expr(Expr::Call(
            Identifier::Named(Arc::from("print_int")),
            vec![Expr::Call(
                Identifier::Named(Arc::from("read_int")),
                vec![],
            )],
        ))]),
        inputs: VecDeque::from(vec![42]),
        expected_outputs: VecDeque::from(vec![42]),
    })
}

#[test]
fn test_patch_instructions_subinput() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![Statement::Expr(Expr::Call(
            Identifier::Named(Arc::from("print_int")),
            vec![Expr::BinaryOp(
                Box::new(Expr::Call(
                    Identifier::Named(Arc::from("read_int")),
                    vec![],
                )),
                BinaryOperator::Subtract,
                Box::new(Expr::Call(
                    Identifier::Named(Arc::from("read_int")),
                    vec![],
                )),
            )],
        ))]),
        inputs: VecDeque::from(vec![5, 3]),
        expected_outputs: VecDeque::from(vec![2]),
    });
}

#[test]
fn test_patch_instructions_zero() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![Statement::Expr(Expr::Call(
            Identifier::Named(Arc::from("print_int")),
            vec![Expr::Constant(Value::I64(0))],
        ))]),
        inputs: VecDeque::from(vec![]),
        expected_outputs: VecDeque::from(vec![0]),
    });
}

#[test]
fn test_patch_instructions_nested() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![Statement::Expr(Expr::Call(
            Identifier::Named(Arc::from("print_int")),
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
        ))]),
        inputs: VecDeque::from(vec![]),
        expected_outputs: VecDeque::from(vec![84]),
    });
}

#[test]
fn test_patch_instructions_mixed() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![Statement::Expr(Expr::Call(
            Identifier::Named(Arc::from("print_int")),
            vec![Expr::BinaryOp(
                Box::new(Expr::BinaryOp(
                    Box::new(Expr::Call(
                        Identifier::Named(Arc::from("read_int")),
                        vec![],
                    )),
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
        ))]),
        inputs: VecDeque::from(vec![-100]),
        expected_outputs: VecDeque::from(vec![44 - 100]),
    });
}

#[test]
fn test_patch_instructions_simple_assignment() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![
            Statement::Assign(
                Identifier::Named(Arc::from("x")),
                Expr::Constant(Value::I64(1000)),
            ),
            Statement::Expr(Expr::Call(
                Identifier::Named(Arc::from("print_int")),
                vec![Expr::Id(Identifier::Named(Arc::from("x")))],
            )),
        ]),
        inputs: VecDeque::from(vec![]),
        expected_outputs: VecDeque::from(vec![1000]),
    });
}

#[test]
fn test_patch_instructions_simple_assignment_imm64() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![
            Statement::Assign(
                Identifier::Named(Arc::from("x")),
                Expr::Constant(Value::I64(i64::MAX)),
            ),
            Statement::Expr(Expr::Call(
                Identifier::Named(Arc::from("print_int")),
                vec![Expr::Id(Identifier::Named(Arc::from("x")))],
            )),
        ]),
        inputs: VecDeque::from(vec![]),
        expected_outputs: VecDeque::from(vec![i64::MAX]),
    });
}

#[test]
fn test_patch_instructions_complex_assignment() {
    // -- Original
    // print((input_int() + 2) + (40 - 2))

    // -- Expected
    // e0 = input_int()
    // e1 = e0 + 2
    // e2 = 40 - 2
    // e3 = e1 + e2
    // print(e3)
    execute_test_case(TestCase {
        ast: Module::Body(vec![
            Statement::Assign(
                Identifier::Named(Arc::from("foofoo")),
                Expr::BinaryOp(
                    Box::new(Expr::BinaryOp(
                        Box::new(Expr::Call(
                            Identifier::Named(Arc::from("read_int")),
                            vec![],
                        )),
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
                Identifier::Named(Arc::from("print_int")),
                vec![Expr::Id(Identifier::Named(Arc::from("foofoo")))],
            )),
        ]),
        inputs: VecDeque::from(vec![10]),
        expected_outputs: VecDeque::from(vec![10 + 2 + 40 - 2]),
    });
}

#[test]
fn test_patch_instructions_complex_args() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![Statement::Expr(Expr::Call(
            Identifier::Named(Arc::from("print_int")),
            vec![Expr::BinaryOp(
                Box::new(Expr::BinaryOp(
                    Box::new(Expr::Call(
                        Identifier::Named(Arc::from("read_int")),
                        vec![],
                    )),
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
        ))]),
        inputs: VecDeque::from(vec![10]),
        expected_outputs: VecDeque::from(vec![10 + 2 + 40 - 2]),
    });
}

#[test]
fn test_patch_instructions_cascading_assigns() {
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
        ast: Module::Body(vec![
            Statement::Assign(
                Identifier::Named(Arc::from("foo")),
                Expr::Call(Identifier::Named(Arc::from("read_int")), vec![]),
            ),
            Statement::Assign(
                Identifier::Named(Arc::from("bar")),
                Expr::BinaryOp(
                    Box::new(Expr::Call(
                        Identifier::Named(Arc::from("read_int")),
                        vec![],
                    )),
                    BinaryOperator::Add,
                    Box::new(Expr::Id(Identifier::Named(Arc::from("foo")))),
                ),
            ),
            Statement::Assign(
                Identifier::Named(Arc::from("baz")),
                Expr::BinaryOp(
                    Box::new(Expr::Call(
                        Identifier::Named(Arc::from("read_int")),
                        vec![],
                    )),
                    BinaryOperator::Add,
                    Box::new(Expr::Id(Identifier::Named(Arc::from("bar")))),
                ),
            ),
            Statement::Assign(
                Identifier::Named(Arc::from("bop")),
                Expr::BinaryOp(
                    Box::new(Expr::BinaryOp(
                        Box::new(Expr::Id(Identifier::Named(Arc::from("foo")))),
                        BinaryOperator::Add,
                        Box::new(Expr::Id(Identifier::Named(Arc::from("bar")))),
                    )),
                    BinaryOperator::Add,
                    Box::new(Expr::Id(Identifier::Named(Arc::from("baz")))),
                ),
            ),
            Statement::Expr(Expr::Call(
                Identifier::Named(Arc::from("print_int")),
                vec![Expr::BinaryOp(
                    Box::new(Expr::Call(
                        Identifier::Named(Arc::from("read_int")),
                        vec![],
                    )),
                    BinaryOperator::Add,
                    Box::new(Expr::Id(Identifier::Named(Arc::from("bop")))),
                )],
            )),
        ]),
        inputs: VecDeque::from(vec![10, 20, 30, 40]),
        expected_outputs: VecDeque::from(vec![10 + (10 + 20) + (10 + 20 + 30) + 40]),
    });
}
