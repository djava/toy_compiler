use crate::{
    passes::ASTtoIRPass,
    syntax_trees::{
        ast,
        ir::{self, BlockMap},
        shared::*,
    },
};

pub struct TranslateASTtoIR;

impl ASTtoIRPass for TranslateASTtoIR {
    fn run_pass(self, m: ast::Program) -> ir::IRProgram {
        let mut ir_functions = vec![];
        for f in m.functions {
            let mut blocks = BlockMap::new();

            let entry_id = Identifier::new_ephemeral();
            let exit_id = Identifier::new_ephemeral();
            let mut main_body = ir::Block {
                statements: vec![ir::Statement::Goto(exit_id.clone())],
            };

            for s in f.body.iter().rev() {
                main_body.statements = generate_for_statement(s, main_body.statements, &mut blocks);
            }

            blocks.insert(entry_id.clone(), main_body);

            // TODO: support return statements
            let exit_block = ir::Block {
                statements: vec![ir::Statement::Return(ir::Atom::Constant(Value::I64(0)))],
            };
            blocks.insert(exit_id, exit_block);

            ir_functions.push(ir::Function {
                name: f.name,
                blocks,
                entry_block: entry_id,
                types: f.types.clone(),
            })
        }

        ir::IRProgram {
            functions: ir_functions,
        }
    }
}

fn generate_for_statement(
    s: &ast::Statement,
    cont: Vec<ir::Statement>,
    blocks: &mut BlockMap,
) -> Vec<ir::Statement> {
    match s {
        ast::Statement::Assign(dest_id, expr) => {
            generate_for_assign(expr, dest_id.clone(), cont, blocks)
        }
        ast::Statement::Expr(expr) => generate_for_effect(expr, cont, blocks),
        ast::Statement::Conditional(cond, pos, neg) => {
            let cont_label = new_block(cont, blocks);

            let mut pos_ir = vec![ir::Statement::Goto(cont_label.clone())];
            for i in pos.iter().rev() {
                pos_ir = generate_for_statement(i, pos_ir, blocks);
            }

            let mut neg_ir = vec![ir::Statement::Goto(cont_label)];
            for i in neg.iter().rev() {
                neg_ir = generate_for_statement(i, neg_ir, blocks);
            }

            let pos_label = new_block(pos_ir, blocks);
            let neg_label = new_block(neg_ir, blocks);
            generate_for_predicate(cond, pos_label, neg_label, blocks)
        }
        ast::Statement::WhileLoop(cond, body) => {
            let cont_label = new_block(cont, blocks);

            let cond_label = new_block(vec![], blocks);

            let mut body_ir = vec![ir::Statement::Goto(cond_label.clone())];
            for i in body.iter().rev() {
                body_ir = generate_for_statement(i, body_ir, blocks);
            }

            let body_label = new_block(body_ir, blocks);

            let cond_ir = generate_for_predicate(cond, body_label.clone(), cont_label, blocks);
            let cond_block = blocks.get_mut(&cond_label).unwrap();
            cond_block.statements = cond_ir;

            vec![ir::Statement::Goto(cond_label)]
        }
    }
}

fn generate_for_effect(
    e: &ast::Expr,
    cont: Vec<ir::Statement>,
    blocks: &mut BlockMap,
) -> Vec<ir::Statement> {
    // Keep only the side effects of an expr statement, the result
    // doesn't matter
    match e {
        ast::Expr::Call(identifier, exprs) => {
            let mut ret = vec![ir::Statement::Expr(ir::Expr::Call(
                identifier.clone(),
                exprs.iter().map(expr_to_atom).collect(),
            ))];

            ret.extend(cont);
            ret
        }
        ast::Expr::Ternary(cond, pos, neg) => {
            let cont_label = new_block(cont, blocks);
            let pos_ir =
                generate_for_effect(pos, vec![ir::Statement::Goto(cont_label.clone())], blocks);
            let neg_ir = generate_for_effect(neg, vec![ir::Statement::Goto(cont_label)], blocks);

            let pos_label = new_block(pos_ir, blocks);
            let neg_label = new_block(neg_ir, blocks);
            generate_for_predicate(cond, pos_label, neg_label, blocks)
        }
        ast::Expr::StatementBlock(statements, expr) => {
            let mut ret = generate_for_effect(expr, cont, blocks);
            for s in statements.iter().rev() {
                ret = generate_for_statement(s, ret, blocks);
            }

            ret
        }
        ast::Expr::Constant(_)
        | ast::Expr::BinaryOp(_, _, _)
        | ast::Expr::UnaryOp(_, _)
        | ast::Expr::Subscript(_, _)
        | ast::Expr::GlobalSymbol(_)
        | ast::Expr::Id(_) => {
            // No side effects, disregard this expression
            cont
        }

        ast::Expr::Allocate(_, _value_type) => panic!(
            "Having an allocate in a non-assign context is probably a bug in inject_allocation?"
        ),
        ast::Expr::Tuple(_exprs) => {
            panic!("All tuple ast-exprs should have been removed by inject_allocation")
        }
    }
}

fn generate_for_assign(
    e: &ast::Expr,
    dest: AssignDest,
    cont: Vec<ir::Statement>,
    blocks: &mut BlockMap,
) -> Vec<ir::Statement> {
    match e {
        ast::Expr::Constant(value) => {
            let mut ret = vec![ir::Statement::Assign(
                dest,
                ir::Expr::Atom(ir::Atom::Constant(value.clone())),
            )];
            ret.extend(cont);
            ret
        }
        ast::Expr::BinaryOp(left, op, right) => {
            let l_atom = expr_to_atom(&*left);
            let r_atom = expr_to_atom(&*right);

            let mut ret = vec![ir::Statement::Assign(
                dest,
                ir::Expr::BinaryOp(l_atom, *op, r_atom),
            )];
            ret.extend(cont);
            ret
        }
        ast::Expr::UnaryOp(op, expr) => {
            let atom = expr_to_atom(&*expr);

            let mut ret = vec![ir::Statement::Assign(dest, ir::Expr::UnaryOp(*op, atom))];
            ret.extend(cont);
            ret
        }
        ast::Expr::Call(func, exprs) => {
            let args = exprs.iter().map(expr_to_atom).collect();

            let mut ret = vec![ir::Statement::Assign(
                dest,
                ir::Expr::Call(func.clone(), args),
            )];
            ret.extend(cont);
            ret
        }
        ast::Expr::Id(src_id) => {
            let mut ret = vec![ir::Statement::Assign(
                dest,
                ir::Expr::Atom(ir::Atom::Variable(src_id.clone())),
            )];
            ret.extend(cont);
            ret
        }
        ast::Expr::Ternary(cond, pos, neg) => {
            let cont_label = new_block(cont, blocks);

            let pos_ir = generate_for_assign(
                pos,
                dest.clone(),
                vec![ir::Statement::Goto(cont_label.clone())],
                blocks,
            );
            let neg_ir =
                generate_for_assign(neg, dest, vec![ir::Statement::Goto(cont_label)], blocks);

            let pos_label = new_block(pos_ir, blocks);
            let neg_label = new_block(neg_ir, blocks);
            generate_for_predicate(&*cond, pos_label, neg_label, blocks)
        }
        ast::Expr::StatementBlock(statements, expr) => {
            let mut ret = generate_for_assign(expr, dest, cont, blocks);
            for s in statements.iter().rev() {
                ret = generate_for_statement(s, ret, blocks);
            }
            ret
        }
        ast::Expr::Subscript(tup, idx) => {
            let mut ret = vec![ir::Statement::Assign(
                dest,
                ir::Expr::Subscript(expr_to_atom(tup), *idx),
            )];
            ret.extend(cont);

            ret
        }
        ast::Expr::Allocate(bytes, value_type) => {
            let mut ret = vec![ir::Statement::Assign(
                dest,
                ir::Expr::Allocate(*bytes, value_type.clone()),
            )];
            ret.extend(cont);
            ret
        }
        ast::Expr::GlobalSymbol(name) => {
            let mut ret = vec![ir::Statement::Assign(
                dest,
                ir::Expr::Atom(ir::Atom::GlobalSymbol(name.clone())),
            )];
            ret.extend(cont);
            ret
        }
        ast::Expr::Tuple(_) => panic!("All tuples should've been removed by inject_allocations"),
    }
}

fn generate_for_predicate(
    cond: &ast::Expr,
    pos_label: Identifier,
    neg_label: Identifier,
    blocks: &mut BlockMap,
) -> Vec<ir::Statement> {
    match cond {
        ast::Expr::BinaryOp(left, op, right) => {
            let l_atom = expr_to_atom(&*left);
            let r_atom = expr_to_atom(&*right);

            vec![ir::Statement::If(
                ir::Expr::BinaryOp(l_atom, *op, r_atom),
                pos_label,
                neg_label,
            )]
        }
        ast::Expr::Constant(val) => {
            if val.into() {
                blocks[&pos_label].statements.clone()
            } else {
                blocks[&neg_label].statements.clone()
            }
        }
        ast::Expr::UnaryOp(op, val) => {
            vec![ir::Statement::If(
                ir::Expr::UnaryOp(*op, expr_to_atom(&*val)),
                pos_label,
                neg_label,
            )]
        }
        ast::Expr::Ternary(sub_cond, sub_pos, sub_neg) => {
            let sub_pos_ir =
                generate_for_predicate(sub_pos, pos_label.clone(), neg_label.clone(), blocks);
            let sub_neg_ir = generate_for_predicate(sub_neg, pos_label, neg_label, blocks);

            let sub_pos_label = new_block(sub_pos_ir, blocks);
            let sub_neg_label = new_block(sub_neg_ir, blocks);
            generate_for_predicate(sub_cond, sub_pos_label, sub_neg_label, blocks)
        }
        ast::Expr::StatementBlock(statements, expr) => {
            let mut ret = vec![];
            for s in statements.iter().rev() {
                ret = generate_for_statement(s, ret, blocks);
            }

            ret.extend(generate_for_predicate(expr, pos_label, neg_label, blocks));

            ret
        }
        ast::Expr::Call(func_name, args) => {
            vec![ir::Statement::If(
                ir::Expr::Call(func_name.clone(), args.iter().map(expr_to_atom).collect()),
                pos_label,
                neg_label,
            )]
        }
        ast::Expr::Id(identifier) => {
            vec![ir::Statement::If(
                ir::Expr::Atom(ir::Atom::Variable(identifier.clone())),
                pos_label,
                neg_label,
            )]
        }
        ast::Expr::Subscript(tup, idx) => {
            vec![ir::Statement::If(
                ir::Expr::Subscript(expr_to_atom(&*tup), *idx),
                pos_label,
                neg_label,
            )]
        }
        ast::Expr::GlobalSymbol(name) => {
            vec![ir::Statement::If(
                ir::Expr::Atom(ir::Atom::GlobalSymbol(name.clone())),
                pos_label,
                neg_label,
            )]
        }
        ast::Expr::Allocate(_, _value_type) => panic!("Allocate is not a valid predicate"),
        ast::Expr::Tuple(_) => panic!("A tuple is not a valid predicate"),
    }
}

fn expr_to_atom(e: &ast::Expr) -> ir::Atom {
    match e {
        ast::Expr::Constant(value) => ir::Atom::Constant(value.clone()),
        ast::Expr::Id(id) => ir::Atom::Variable(id.clone()),
        ast::Expr::GlobalSymbol(name) => ir::Atom::GlobalSymbol(name.clone()),
        _ => panic!("Expr `{e:?}` cannot be converted to atom"),
    }
}

fn new_block(statements: Vec<ir::Statement>, blocks: &mut BlockMap) -> Identifier {
    if statements.len() == 1
        && let ir::Statement::Goto(label) = &statements[0]
    {
        return label.clone();
    }

    let label = Identifier::new_ephemeral();
    blocks.insert(label.clone(), ir::Block { statements });
    label
}

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;

    use crate::utils::t_id;
    use test_support::{
        ast_const_int, ast_print_int, ast_read_int,
        compiler::{
            constants::LABEL_MAIN,
            passes::{
                ASTPass, ASTtoIRPass, ShortCircuiting, TranslateASTtoIR, TypeCheck,
                RemoveComplexOperands,
            },
            syntax_trees::{ast::*, shared::*},
        },
        ir_interpreter::interpret_irprogram,
    };

    struct TestCase {
        ast: Program,
        inputs: VecDeque<i64>,
        expected_outputs: VecDeque<i64>,
    }

    fn execute_test_case(mut tc: TestCase) {
        let type_checked = TypeCheck.run_pass(tc.ast);
        let post_short_circuiting = ShortCircuiting.run_pass(type_checked);
        let post_rco_ast = RemoveComplexOperands.run_pass(post_short_circuiting);

        println!("-- AST before ASTtoIR:\n{post_rco_ast:?}");
        let post_translation = TranslateASTtoIR.run_pass(post_rco_ast);
        println!("-- AST after ASTtoIR:\n{post_translation:?}");

        let mut outputs = VecDeque::<i64>::new();
        interpret_irprogram(&post_translation, &mut tc.inputs, &mut outputs);

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
                }],
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
                }],
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
                }],
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
                }],
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
                }],
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
                }],
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
                }],
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
                }],
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
                }],
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
                }],
            },
            inputs: VecDeque::from(vec![10, 20, 30, 40]),
            expected_outputs: VecDeque::from(vec![10 + (10 + 20) + (10 + 20 + 30) + 40]),
        });
    }

    #[test]
    fn test_condition_statement() {
        execute_test_case(TestCase {
            ast: Program {
                functions: vec![Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![
                        Statement::Conditional(
                            Expr::BinaryOp(
                                Box::new(ast_read_int()),
                                BinaryOperator::Equals,
                                Box::new(ast_const_int(1)),
                            ),
                            vec![Statement::Expr(ast_print_int(ast_const_int(10)))],
                            vec![Statement::Expr(ast_print_int(ast_const_int(20)))],
                        ),
                        Statement::Conditional(
                            Expr::BinaryOp(
                                Box::new(ast_read_int()),
                                BinaryOperator::Equals,
                                Box::new(ast_const_int(1)),
                            ),
                            vec![Statement::Expr(ast_print_int(ast_const_int(10)))],
                            vec![Statement::Expr(ast_print_int(ast_const_int(20)))],
                        ),
                    ],
                    types: TypeEnv::new(),
                }],
            },
            inputs: VecDeque::from([1, 2]),
            expected_outputs: VecDeque::from([10, 20]),
        });
    }

    #[test]
    fn test_ternary() {
        execute_test_case(TestCase {
            ast: Program {
                functions: vec![Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![
                        Statement::Expr(Expr::Ternary(
                            Box::new(Expr::BinaryOp(
                                Box::new(ast_read_int()),
                                BinaryOperator::Equals,
                                Box::new(ast_const_int(1)),
                            )),
                            Box::new(ast_print_int(ast_const_int(10))),
                            Box::new(ast_print_int(ast_const_int(20))),
                        )),
                        Statement::Expr(Expr::Ternary(
                            Box::new(Expr::BinaryOp(
                                Box::new(ast_read_int()),
                                BinaryOperator::Equals,
                                Box::new(ast_const_int(1)),
                            )),
                            Box::new(ast_print_int(ast_const_int(10))),
                            Box::new(ast_print_int(ast_const_int(20))),
                        )),
                    ],
                    types: TypeEnv::new(),
                }],
            },
            inputs: VecDeque::from([1, 2]),
            expected_outputs: VecDeque::from([10, 20]),
        });
    }

    #[test]
    fn test_ternary_complex() {
        execute_test_case(TestCase {
            ast: Program {
                functions: vec![Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![
                        Statement::Expr(Expr::Ternary(
                            Box::new(Expr::BinaryOp(
                                Box::new(Expr::BinaryOp(
                                    Box::new(ast_read_int()),
                                    BinaryOperator::Equals,
                                    Box::new(ast_const_int(1)),
                                )),
                                BinaryOperator::And,
                                Box::new(Expr::BinaryOp(
                                    Box::new(ast_read_int()),
                                    BinaryOperator::Equals,
                                    Box::new(ast_const_int(2)),
                                )),
                            )),
                            Box::new(ast_print_int(Expr::BinaryOp(
                                Box::new(ast_read_int()),
                                BinaryOperator::Add,
                                Box::new(ast_const_int(10)),
                            ))),
                            Box::new(ast_print_int(Expr::BinaryOp(
                                Box::new(ast_read_int()),
                                BinaryOperator::Add,
                                Box::new(ast_read_int()),
                            ))),
                        )),
                        Statement::Expr(Expr::Ternary(
                            Box::new(Expr::BinaryOp(
                                Box::new(Expr::BinaryOp(
                                    Box::new(ast_read_int()),
                                    BinaryOperator::Equals,
                                    Box::new(ast_const_int(1)),
                                )),
                                BinaryOperator::And,
                                Box::new(Expr::BinaryOp(
                                    Box::new(ast_read_int()),
                                    BinaryOperator::Equals,
                                    Box::new(ast_const_int(2)),
                                )),
                            )),
                            Box::new(ast_print_int(Expr::BinaryOp(
                                Box::new(ast_read_int()),
                                BinaryOperator::Add,
                                Box::new(ast_const_int(10)),
                            ))),
                            Box::new(ast_print_int(Expr::BinaryOp(
                                Box::new(ast_read_int()),
                                BinaryOperator::Add,
                                Box::new(ast_read_int()),
                            ))),
                        )),
                    ],
                    types: TypeEnv::new(),
                }],
            },
            inputs: VecDeque::from([1, 2, 2, 8, 10, 15]),
            expected_outputs: VecDeque::from([12, 25]),
        });
    }

    #[test]
    fn test_while_loop_simple() {
        // x = 5
        // while x > 0 {
        //     print_int(x)
        //     x = x - 1
        // }
        execute_test_case(TestCase {
            ast: Program {
                functions: vec![Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![
                        Statement::Assign(
                            AssignDest::Id(t_id!("x")),
                            Expr::Constant(Value::I64(5)),
                        ),
                        Statement::WhileLoop(
                            Expr::BinaryOp(
                                Box::new(Expr::Id(t_id!("x"))),
                                BinaryOperator::Greater,
                                Box::new(Expr::Constant(Value::I64(0))),
                            ),
                            vec![
                                Statement::Expr(Expr::Call(
                                    t_id!("print_int"),
                                    vec![Expr::Id(t_id!("x"))],
                                )),
                                Statement::Assign(
                                    AssignDest::Id(t_id!("x")),
                                    Expr::BinaryOp(
                                        Box::new(Expr::Id(t_id!("x"))),
                                        BinaryOperator::Subtract,
                                        Box::new(Expr::Constant(Value::I64(1))),
                                    ),
                                ),
                            ],
                        ),
                    ],
                    types: TypeEnv::new(),
                }],
            },
            inputs: VecDeque::new(),
            expected_outputs: VecDeque::from(vec![5, 4, 3, 2, 1]),
        });
    }

    #[test]
    fn test_while_loop_zero_iterations() {
        // x = 0
        // while x > 0 {
        //     print_int(x)
        // }
        // print_int(42)
        execute_test_case(TestCase {
            ast: Program {
                functions: vec![Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![
                        Statement::Assign(
                            AssignDest::Id(t_id!("x")),
                            Expr::Constant(Value::I64(0)),
                        ),
                        Statement::WhileLoop(
                            Expr::BinaryOp(
                                Box::new(Expr::Id(t_id!("x"))),
                                BinaryOperator::Greater,
                                Box::new(Expr::Constant(Value::I64(0))),
                            ),
                            vec![Statement::Expr(Expr::Call(
                                t_id!("print_int"),
                                vec![Expr::Id(t_id!("x"))],
                            ))],
                        ),
                        Statement::Expr(Expr::Call(
                            t_id!("print_int"),
                            vec![Expr::Constant(Value::I64(42))],
                        )),
                    ],
                    types: TypeEnv::new(),
                }],
            },
            inputs: VecDeque::new(),
            expected_outputs: VecDeque::from(vec![42]),
        });
    }

    #[test]
    fn test_while_loop_accumulator() {
        // sum = 0
        // i = 1
        // while i <= 5 {
        //     sum = sum + i
        //     i = i + 1
        // }
        // print_int(sum)
        execute_test_case(TestCase {
            ast: Program {
                functions: vec![Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![
                        Statement::Assign(
                            AssignDest::Id(t_id!("sum")),
                            Expr::Constant(Value::I64(0)),
                        ),
                        Statement::Assign(
                            AssignDest::Id(t_id!("i")),
                            Expr::Constant(Value::I64(1)),
                        ),
                        Statement::WhileLoop(
                            Expr::BinaryOp(
                                Box::new(Expr::Id(t_id!("i"))),
                                BinaryOperator::LessEquals,
                                Box::new(Expr::Constant(Value::I64(5))),
                            ),
                            vec![
                                Statement::Assign(
                                    AssignDest::Id(t_id!("sum")),
                                    Expr::BinaryOp(
                                        Box::new(Expr::Id(t_id!("sum"))),
                                        BinaryOperator::Add,
                                        Box::new(Expr::Id(t_id!("i"))),
                                    ),
                                ),
                                Statement::Assign(
                                    AssignDest::Id(t_id!("i")),
                                    Expr::BinaryOp(
                                        Box::new(Expr::Id(t_id!("i"))),
                                        BinaryOperator::Add,
                                        Box::new(Expr::Constant(Value::I64(1))),
                                    ),
                                ),
                            ],
                        ),
                        Statement::Expr(Expr::Call(
                            t_id!("print_int"),
                            vec![Expr::Id(t_id!("sum"))],
                        )),
                    ],
                    types: TypeEnv::new(),
                }],
            },
            inputs: VecDeque::new(),
            expected_outputs: VecDeque::from(vec![15]), // sum of 1..5
        });
    }

    #[test]
    fn test_while_loop_nested() {
        // i = 0
        // while i < 2 {
        //     j = 0
        //     while j < 2 {
        //         print_int(i)
        //         print_int(j)
        //         j = j + 1
        //     }
        //     i = i + 1
        // }
        execute_test_case(TestCase {
            ast: Program {
                functions: vec![Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![
                        Statement::Assign(
                            AssignDest::Id(t_id!("i")),
                            Expr::Constant(Value::I64(0)),
                        ),
                        Statement::WhileLoop(
                            Expr::BinaryOp(
                                Box::new(Expr::Id(t_id!("i"))),
                                BinaryOperator::Less,
                                Box::new(Expr::Constant(Value::I64(2))),
                            ),
                            vec![
                                Statement::Assign(
                                    AssignDest::Id(t_id!("j")),
                                    Expr::Constant(Value::I64(0)),
                                ),
                                Statement::WhileLoop(
                                    Expr::BinaryOp(
                                        Box::new(Expr::Id(t_id!("j"))),
                                        BinaryOperator::Less,
                                        Box::new(Expr::Constant(Value::I64(2))),
                                    ),
                                    vec![
                                        Statement::Expr(Expr::Call(
                                            t_id!("print_int"),
                                            vec![Expr::Id(t_id!("i"))],
                                        )),
                                        Statement::Expr(Expr::Call(
                                            t_id!("print_int"),
                                            vec![Expr::Id(t_id!("j"))],
                                        )),
                                        Statement::Assign(
                                            AssignDest::Id(t_id!("j")),
                                            Expr::BinaryOp(
                                                Box::new(Expr::Id(t_id!("j"))),
                                                BinaryOperator::Add,
                                                Box::new(Expr::Constant(Value::I64(1))),
                                            ),
                                        ),
                                    ],
                                ),
                                Statement::Assign(
                                    AssignDest::Id(t_id!("i")),
                                    Expr::BinaryOp(
                                        Box::new(Expr::Id(t_id!("i"))),
                                        BinaryOperator::Add,
                                        Box::new(Expr::Constant(Value::I64(1))),
                                    ),
                                ),
                            ],
                        ),
                    ],
                    types: TypeEnv::new(),
                }],
            },
            inputs: VecDeque::new(),
            expected_outputs: VecDeque::from(vec![0, 0, 0, 1, 1, 0, 1, 1]),
        });
    }

    #[test]
    fn test_while_loop_with_conditional() {
        // i = 0
        // while i < 5 {
        //     if i == 2 {
        //         print_int(100)
        //     } else {
        //         print_int(i)
        //     }
        //     i = i + 1
        // }
        execute_test_case(TestCase {
            ast: Program {
                functions: vec![Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![
                        Statement::Assign(
                            AssignDest::Id(t_id!("i")),
                            Expr::Constant(Value::I64(0)),
                        ),
                        Statement::WhileLoop(
                            Expr::BinaryOp(
                                Box::new(Expr::Id(t_id!("i"))),
                                BinaryOperator::Less,
                                Box::new(Expr::Constant(Value::I64(5))),
                            ),
                            vec![
                                Statement::Conditional(
                                    Expr::BinaryOp(
                                        Box::new(Expr::Id(t_id!("i"))),
                                        BinaryOperator::Equals,
                                        Box::new(Expr::Constant(Value::I64(2))),
                                    ),
                                    vec![Statement::Expr(Expr::Call(
                                        t_id!("print_int"),
                                        vec![Expr::Constant(Value::I64(100))],
                                    ))],
                                    vec![Statement::Expr(Expr::Call(
                                        t_id!("print_int"),
                                        vec![Expr::Id(t_id!("i"))],
                                    ))],
                                ),
                                Statement::Assign(
                                    AssignDest::Id(t_id!("i")),
                                    Expr::BinaryOp(
                                        Box::new(Expr::Id(t_id!("i"))),
                                        BinaryOperator::Add,
                                        Box::new(Expr::Constant(Value::I64(1))),
                                    ),
                                ),
                            ],
                        ),
                    ],
                    types: TypeEnv::new(),
                }],
            },
            inputs: VecDeque::new(),
            expected_outputs: VecDeque::from(vec![0, 1, 100, 3, 4]),
        });
    }
}
