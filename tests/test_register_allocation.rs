mod infra;
use std::collections::VecDeque;

use cs4999_compiler::{
    ast::*,
    passes::{
        IRPass, IRToX86Pass, X86Pass, register_allocation::RegisterAllocation,
        remove_complex_operands::RemoveComplexOperands, select_instructions::SelectInstructions,
    },
    x86_ast,
};

use crate::infra::{type_check::type_check, x86_interpreter::interpret_x86};

struct TestCase {
    ast: Module,
    inputs: VecDeque<i64>,
    expected_outputs: VecDeque<i64>,
}

fn execute_test_case(mut tc: TestCase) {
    println!("\n==================");

    type_check(&tc.ast);
    println!("Type-check passed on source");

    let post_rco_ast = RemoveComplexOperands::run_pass(tc.ast);

    let post_instr_sel_x86ast = SelectInstructions::run_pass(post_rco_ast);
    println!("-- AST before RegAlloc:\n{post_instr_sel_x86ast}");
    let post_reg_alloc_x86ast = RegisterAllocation::run_pass(post_instr_sel_x86ast);
    println!("-- AST after RegAlloc:\n{post_reg_alloc_x86ast}");

    // Ensure that all the variable arguments have been removed
    for i in post_reg_alloc_x86ast.functions.iter().map(|x| &x.1).flatten() {
        use x86_ast::{Instr, Arg};
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

    let mut outputs = VecDeque::<i64>::new();
    interpret_x86(&post_reg_alloc_x86ast, &mut tc.inputs, &mut outputs);

    assert_eq!(outputs, tc.expected_outputs);
}

#[test]
fn test_register_allocation_add() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![Statement::Expr(Expr::Call(
            Identifier::Named(String::from("print")),
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
fn test_register_allocation_input() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![Statement::Expr(Expr::Call(
            Identifier::Named(String::from("print")),
            vec![Expr::Call(
                Identifier::Named(String::from("input_int")),
                vec![],
            )],
        ))]),
        inputs: VecDeque::from(vec![42]),
        expected_outputs: VecDeque::from(vec![42]),
    })
}

#[test]
fn test_register_allocation_subinput() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![Statement::Expr(Expr::Call(
            Identifier::Named(String::from("print")),
            vec![Expr::BinaryOp(
                Box::new(Expr::Call(
                    Identifier::Named(String::from("input_int")),
                    vec![],
                )),
                BinaryOperator::Subtract,
                Box::new(Expr::Call(
                    Identifier::Named(String::from("input_int")),
                    vec![],
                )),
            )],
        ))]),
        inputs: VecDeque::from(vec![5, 3]),
        expected_outputs: VecDeque::from(vec![2]),
    });
}

#[test]
fn test_register_allocation_zero() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![Statement::Expr(Expr::Call(
            Identifier::Named(String::from("print")),
            vec![Expr::Constant(Value::I64(0))],
        ))]),
        inputs: VecDeque::from(vec![]),
        expected_outputs: VecDeque::from(vec![0]),
    });
}

#[test]
fn test_register_allocation_nested() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![Statement::Expr(Expr::Call(
            Identifier::Named(String::from("print")),
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
fn test_register_allocation_mixed() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![Statement::Expr(Expr::Call(
            Identifier::Named(String::from("print")),
            vec![Expr::BinaryOp(
                Box::new(Expr::BinaryOp(
                    Box::new(Expr::Call(
                        Identifier::Named(String::from("input_int")),
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
fn test_register_allocation_simple_assignment() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![
            Statement::Assign(
                Identifier::Named(String::from("x")),
                Expr::Constant(Value::I64(1000)),
            ),
            Statement::Expr(Expr::Call(
                Identifier::Named(String::from("print")),
                vec![Expr::Id(Identifier::Named(String::from("x")))],
            )),
        ]),
        inputs: VecDeque::from(vec![]),
        expected_outputs: VecDeque::from(vec![1000]),
    });
}

#[test]
fn test_register_allocation_complex_assignment() {
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
                Identifier::Named(String::from("foofoo")),
                Expr::BinaryOp(
                    Box::new(Expr::BinaryOp(
                        Box::new(Expr::Call(
                            Identifier::Named(String::from("input_int")),
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
                Identifier::Named(String::from("print")),
                vec![Expr::Id(Identifier::Named(String::from("foofoo")))],
            )),
        ]),
        inputs: VecDeque::from(vec![10]),
        expected_outputs: VecDeque::from(vec![10 + 2 + 40 - 2]),
    });
}

#[test]
fn test_register_allocation_complex_args() {
    execute_test_case(TestCase {
        ast: Module::Body(vec![Statement::Expr(Expr::Call(
            Identifier::Named(String::from("print")),
            vec![Expr::BinaryOp(
                Box::new(Expr::BinaryOp(
                    Box::new(Expr::Call(
                        Identifier::Named(String::from("input_int")),
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
fn test_register_allocation_cascading_assigns() {
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
                Identifier::Named(String::from("foo")),
                Expr::Call(Identifier::Named(String::from("input_int")), vec![]),
            ),
            Statement::Assign(
                Identifier::Named(String::from("bar")),
                Expr::BinaryOp(
                    Box::new(Expr::Call(
                        Identifier::Named(String::from("input_int")),
                        vec![],
                    )),
                    BinaryOperator::Add,
                    Box::new(Expr::Id(Identifier::Named(String::from("foo")))),
                ),
            ),
            Statement::Assign(
                Identifier::Named(String::from("baz")),
                Expr::BinaryOp(
                    Box::new(Expr::Call(
                        Identifier::Named(String::from("input_int")),
                        vec![],
                    )),
                    BinaryOperator::Add,
                    Box::new(Expr::Id(Identifier::Named(String::from("bar")))),
                ),
            ),
            Statement::Assign(
                Identifier::Named(String::from("bop")),
                Expr::BinaryOp(
                    Box::new(Expr::BinaryOp(
                        Box::new(Expr::Id(Identifier::Named(String::from("foo")))),
                        BinaryOperator::Add,
                        Box::new(Expr::Id(Identifier::Named(String::from("bar")))),
                    )),
                    BinaryOperator::Add,
                    Box::new(Expr::Id(Identifier::Named(String::from("baz")))),
                ),
            ),
            Statement::Expr(Expr::Call(
                Identifier::Named(String::from("print")),
                vec![Expr::BinaryOp(
                    Box::new(Expr::Call(
                        Identifier::Named(String::from("input_int")),
                        vec![],
                    )),
                    BinaryOperator::Add,
                    Box::new(Expr::Id(Identifier::Named(String::from("bop")))),
                )],
            )),
        ]),
        inputs: VecDeque::from(vec![10, 20, 30, 40]),
        expected_outputs: VecDeque::from(vec![10 + (10 + 20) + (10 + 20 + 30) + 40]),
    });
}
