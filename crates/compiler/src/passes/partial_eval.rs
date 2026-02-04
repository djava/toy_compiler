use crate::{syntax_trees::{ast::*, shared::*}, passes::ASTPass};

pub struct PartialEval;

impl ASTPass for PartialEval {
    fn run_pass(self, mut m: Module) -> Module {
        let mut new_body = Vec::new();
        for s in m.body {
            partial_eval_statement(s, &mut new_body);
        }

        m.body = new_body;
        m
    }
}

fn partial_eval_statement(s: Statement, new_statements: &mut Vec<Statement>) {
    match s {
        Statement::Assign(dest, mut e) => {
            partial_eval_expr(&mut e);
            new_statements.push(Statement::Assign(dest, e));
        }
        Statement::Expr(mut e) => {
            partial_eval_expr(&mut e);
            new_statements.push(Statement::Expr(e));
        }
        Statement::Conditional(mut cond, pos, neg) => {
            partial_eval_expr(&mut cond);

            if let Expr::Constant(val) = cond {
                if val.into() {
                    let mut pos_statements = Vec::new();
                    pos.into_iter()
                        .for_each(|s| partial_eval_statement(s, &mut pos_statements));
                    new_statements.extend(pos_statements);
                } else {
                    let mut neg_statements = Vec::new();
                    neg.into_iter()
                        .for_each(|s| partial_eval_statement(s, &mut neg_statements));
                    new_statements.extend(neg_statements);
                }
            } else {
                let mut pos_pe = Vec::new();
                pos.into_iter()
                    .for_each(|s| partial_eval_statement(s, &mut pos_pe));

                let mut neg_pe = Vec::new();
                neg.into_iter()
                    .for_each(|s| partial_eval_statement(s, &mut neg_pe));

                new_statements.push(Statement::Conditional(cond, pos_pe, neg_pe));
            }
        }
        Statement::WhileLoop(mut cond, body) => {
            partial_eval_expr(&mut cond);
            let mut body_pe = vec![];
            body.into_iter()
                .for_each(|s| partial_eval_statement(s, &mut body_pe));

            if let Expr::Constant(val) = cond {
                if val.into() {
                    println!(
                        "Contains an infinite loop - careful because I don't know if they'll respond to ctrl-c :)"
                    );
                } else {
                    // Cond is always false - don't even generate the loop
                }
            } else {
                new_statements.push(Statement::WhileLoop(cond, body_pe));
            }
        }
    }
}

fn partial_eval_expr(e: &mut Expr) {
    use Expr::*;

    match e {
        BinaryOp(left, op, right) => {
            // Try and evaluate both operands recursively, then if
            // they're both constants we can evaluate the whole
            // expression
            partial_eval_expr(&mut *left);
            partial_eval_expr(&mut *right);

            if let Constant(l_val) = &**left
                && let Constant(r_val) = &**right
            {
                *e = Constant(op.eval(l_val, r_val))
            }
        }
        UnaryOp(op, expr) => {
            // Try and evaluate the operand recursively, then if its
            // constant we can evaluate the whole expression
            partial_eval_expr(&mut *expr);
            if let Constant(val) = &**expr {
                *e = Constant(op.eval(&val));
            }
        }
        Call(_name, args) => {
            // Can't evaluate through function calls right now, but try
            // to evaluate the arguments regardless
            for i in args {
                partial_eval_expr(i);
            }
        }
        StatementBlock(statements, expr) => {
            let mut new_statements = Vec::new();
            statements
                .iter()
                .for_each(|s| partial_eval_statement(s.clone(), &mut new_statements));
            *statements = new_statements;
            partial_eval_expr(expr);
        }
        Constant(_val) => {
            // Already a constant, nothing to evaluate
        }
        Id(_name) => {
            // Can't do anything
        }
        Ternary(cond, pos, neg) => {
            partial_eval_expr(&mut *cond);
            partial_eval_expr(&mut *pos);
            partial_eval_expr(&mut *neg);

            if let Constant(val) = cond.as_ref() {
                if val.into() {
                    *e = (**pos).clone();
                } else {
                    *e = (**neg).clone();
                }
            }
        }
        Tuple(elems) => elems.iter_mut().for_each(partial_eval_expr),
        Subscript(tup, idx) => {
            partial_eval_expr(tup.as_mut());
            if let Tuple(elems) = &**tup {
                if let Constant(elem_val) = &elems[*idx as usize] {
                    *e = Constant(elem_val.clone());
                }
            }
        }
        GlobalSymbol(_) => {}
        Allocate(_, _) => {
            panic!("This pass should've happened before any Allocate calls are injected")
        }
    }
}

trait BinaryOperatorExt {
    fn eval(&self, l: &Value, r: &Value) -> Value;
}

impl BinaryOperatorExt for BinaryOperator {
    fn eval(&self, l: &Value, r: &Value) -> Value {
        use ValueType::*;

        match (ValueType::from(l), ValueType::from(r)) {
            (IntType, IntType) => {
                if let Value::I64(l_val) = l
                    && let Value::I64(r_val) = r
                {
                    match self {
                        BinaryOperator::Add => Value::I64(l_val + r_val),
                        BinaryOperator::Subtract => Value::I64(l_val - r_val),
                        BinaryOperator::Multiply => Value::I64(l_val * r_val),
                        BinaryOperator::Equals => Value::Bool(l_val == r_val),
                        BinaryOperator::NotEquals => Value::Bool(l_val != r_val),
                        BinaryOperator::Greater => Value::Bool(l_val > r_val),
                        BinaryOperator::GreaterEquals => Value::Bool(l_val >= r_val),
                        BinaryOperator::Less => Value::Bool(l_val < r_val),
                        BinaryOperator::LessEquals => Value::Bool(l_val <= r_val),
                        _ => panic!(
                            "Unsupported operand types (int, int) to BinaryOperator {self:?}"
                        ),
                    }
                } else {
                    panic!("ValueType didnt match Value variant - bug in ValueType::from()?");
                }
            }
            (BoolType, BoolType) => {
                if let Value::Bool(l_val) = l
                    && let Value::Bool(r_val) = r
                {
                    match self {
                        BinaryOperator::And => Value::Bool(*l_val && *r_val),
                        BinaryOperator::Or => Value::Bool(*l_val || *r_val),
                        BinaryOperator::Equals => Value::Bool(*l_val == *r_val),
                        BinaryOperator::NotEquals => Value::Bool(*l_val != *r_val),
                        _ => panic!(
                            "Unsupported operand types (bool, bool) to BinaryOperator {self:?}"
                        ),
                    }
                } else {
                    panic!("ValueType didnt match Value variant - bug in ValueType::from()?");
                }
            }
            _ => panic!(
                "Unsupported operand type ({:?}, {:?}) to UnaryOperator {self:?}",
                ValueType::from(l),
                ValueType::from(r)
            ),
        }
    }
}
trait UnaryOperatorExt {
    fn eval(&self, v: &Value) -> Value;
}

impl UnaryOperatorExt for UnaryOperator {
    fn eval(&self, v: &Value) -> Value {
        use ValueType::*;

        match ValueType::from(v) {
            IntType => {
                if let Value::I64(val) = v {
                    match self {
                        UnaryOperator::Minus => Value::I64(-val),
                        UnaryOperator::Plus => Value::I64(*val),
                        _ => panic!("Unsupported operand type (int) to UnaryOperator {self:?}"),
                    }
                } else {
                    panic!("ValueType didnt match Value variant - bug in ValueType::from()?");
                }
            }
            BoolType => {
                if let Value::Bool(val) = v {
                    match self {
                        UnaryOperator::Not => Value::Bool(!val),
                        _ => panic!("Unsupported operand type bool to UnaryOperator {self:?}"),
                    }
                } else {
                    panic!("ValueType didnt match Value variant - bug in ValueType::from()?");
                }
            }
            _ => panic!(
                "Unsupported operand type ({:?}) to UnaryOperator {self:?}",
                ValueType::from(v)
            ),
        }
    }
}

#[cfg(test)]
mod test {
    use std::collections::VecDeque;

    use test_support::{
        ast_interpreter::interpret,
        compiler::{
            syntax_trees::{ast::*, shared::*},
            passes::{ASTPass, partial_eval::PartialEval},
            utils::type_check_ast_statements
        },
    };

    struct TestCase {
        ast: Module,
        inputs: VecDeque<i64>,
        expected_outputs: VecDeque<i64>,
    }

    fn check_expr_invariants(e: &Expr) {
        match e {
            Expr::BinaryOp(left, _, right) => {
                check_expr_invariants(left);
                check_expr_invariants(right);
                assert!(
                    !matches!((&**left, &**right), (Expr::Constant(_), Expr::Constant(_))),
                    "BinaryOp with two Constant operands should have been folded: {e:?}"
                );
            }
            Expr::UnaryOp(_, val) => {
                check_expr_invariants(val);
                assert!(
                    !matches!(&**val, Expr::Constant(_)),
                    "UnaryOp with Constant operand should have been folded: {e:?}"
                );
            }
            Expr::Call(_, args) => {
                args.iter().for_each(check_expr_invariants);
            }
            Expr::Ternary(cond, pos, neg) => {
                check_expr_invariants(cond);
                check_expr_invariants(pos);
                check_expr_invariants(neg);
                assert!(
                    !matches!(&**cond, Expr::Constant(_)),
                    "Ternary with Constant condition should have been resolved: {e:?}"
                );
            }
            Expr::Tuple(elems) => {
                elems.iter().for_each(check_expr_invariants);
            }
            Expr::Subscript(tup, idx) => {
                check_expr_invariants(tup);
                if let Expr::Tuple(elems) = &**tup {
                    assert!(
                        !matches!(&elems[*idx as usize], Expr::Constant(_)),
                        "Subscript of Tuple with Constant element should have been folded: {e:?}"
                    );
                }
            }
            Expr::StatementBlock(stmts, expr) => {
                stmts.iter().for_each(check_statement_invariants);
                check_expr_invariants(expr);
            }
            Expr::Constant(_) | Expr::Id(_) | Expr::GlobalSymbol(_) | Expr::Allocate(_, _) => {}
        }
    }

    fn check_statement_invariants(s: &Statement) {
        match s {
            Statement::Assign(_, expr) => check_expr_invariants(expr),
            Statement::Expr(expr) => check_expr_invariants(expr),
            Statement::Conditional(cond, pos, neg) => {
                check_expr_invariants(cond);
                assert!(
                    !matches!(cond, Expr::Constant(_)),
                    "Conditional with Constant condition should have been inlined: {s:?}"
                );
                pos.iter().for_each(check_statement_invariants);
                neg.iter().for_each(check_statement_invariants);
            }
            Statement::WhileLoop(cond, body) => {
                check_expr_invariants(cond);
                assert!(
                    !matches!(cond, Expr::Constant(_)),
                    "WhileLoop with Constant condition should have been eliminated: {s:?}"
                );
                body.iter().for_each(check_statement_invariants);
            }
        }
    }

    fn check_invariants(m: &Module) {
        m.body.iter().for_each(check_statement_invariants);
    }

    fn execute_test_case(mut tc: TestCase) {
        type_check_ast_statements(&tc.ast.body, &mut TypeEnv::new());

        println!("AST before Partial Eval: {:?}", tc.ast);
        let post_run_ast = PartialEval.run_pass(tc.ast);
        println!("AST after Partial Eval: {:?}", post_run_ast);

        type_check_ast_statements(&post_run_ast.body, &mut TypeEnv::new());
        check_invariants(&post_run_ast);

        let mut outputs = VecDeque::<i64>::new();
        interpret(&post_run_ast, &mut tc.inputs, &mut outputs);

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
    fn test_while_loop_simple() {
        // x = 5
        // while x > 0 {
        //     print_int(x)
        //     x = x - 1
        // }
        execute_test_case(TestCase {
            ast: Module {
                body: vec![
                    Statement::Assign(
                        AssignDest::Id(Identifier::from("x")),
                        Expr::Constant(Value::I64(5)),
                    ),
                    Statement::WhileLoop(
                        Expr::BinaryOp(
                            Box::new(Expr::Id(Identifier::from("x"))),
                            BinaryOperator::Greater,
                            Box::new(Expr::Constant(Value::I64(0))),
                        ),
                        vec![
                            Statement::Expr(Expr::Call(
                                Identifier::from("print_int"),
                                vec![Expr::Id(Identifier::from("x"))],
                            )),
                            Statement::Assign(
                                AssignDest::Id(Identifier::from("x")),
                                Expr::BinaryOp(
                                    Box::new(Expr::Id(Identifier::from("x"))),
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
            expected_outputs: VecDeque::from(vec![5, 4, 3, 2, 1]),
        });
    }

    #[test]
    fn test_while_loop_constant_false() {
        // while false {
        //     print_int(100)
        // }
        // print_int(42)
        execute_test_case(TestCase {
            ast: Module {
                body: vec![
                    Statement::WhileLoop(
                        Expr::Constant(Value::Bool(false)),
                        vec![Statement::Expr(Expr::Call(
                            Identifier::from("print_int"),
                            vec![Expr::Constant(Value::I64(100))],
                        ))],
                    ),
                    Statement::Expr(Expr::Call(
                        Identifier::from("print_int"),
                        vec![Expr::Constant(Value::I64(42))],
                    )),
                ],
                types: TypeEnv::new(),
            },
            inputs: VecDeque::new(),
            expected_outputs: VecDeque::from(vec![42]), // Loop should be removed entirely
        });
    }

    #[test]
    fn test_while_loop_with_partial_eval_in_body() {
        // x = 3
        // while x > 0 {
        //     print_int(10 + 10)  // Should be partial evaluated to 20
        //     x = x - 1
        // }
        execute_test_case(TestCase {
            ast: Module {
                body: vec![
                    Statement::Assign(
                        AssignDest::Id(Identifier::from("x")),
                        Expr::Constant(Value::I64(3)),
                    ),
                    Statement::WhileLoop(
                        Expr::BinaryOp(
                            Box::new(Expr::Id(Identifier::from("x"))),
                            BinaryOperator::Greater,
                            Box::new(Expr::Constant(Value::I64(0))),
                        ),
                        vec![
                            Statement::Expr(Expr::Call(
                                Identifier::from("print_int"),
                                vec![Expr::BinaryOp(
                                    Box::new(Expr::Constant(Value::I64(10))),
                                    BinaryOperator::Add,
                                    Box::new(Expr::Constant(Value::I64(10))),
                                )],
                            )),
                            Statement::Assign(
                                AssignDest::Id(Identifier::from("x")),
                                Expr::BinaryOp(
                                    Box::new(Expr::Id(Identifier::from("x"))),
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
            expected_outputs: VecDeque::from(vec![20, 20, 20]),
        });
    }
}
