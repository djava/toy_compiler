use crate::{
    passes::ASTPass,
    syntax_trees::{ast::*, shared::*},
};

pub struct ShortCircuiting;

impl ASTPass for ShortCircuiting {
    fn run_pass(self, mut m: Program) -> Program {
        for f in m.functions.iter_mut() {
            for s in f.body.iter_mut() {
                shortcircuit_statement(s)
            }
        }

        m
    }
}

fn shortcircuit_statement(s: &mut Statement) {
    match s {
        Statement::Assign(_, expr)
        | Statement::Expr(expr)
        | Statement::Conditional(expr, _, _)
        | Statement::Return(expr)
        | Statement::WhileLoop(expr, _) => shortcircuit_expr(expr),
    }
}

fn shortcircuit_expr(e: &mut Expr) {
    // Recurse sub-exprs
    match e {
        Expr::BinaryOp(e1, _, e2) => {
            shortcircuit_expr(&mut *e1);
            shortcircuit_expr(&mut *e2);
        }
        Expr::UnaryOp(_, e) => {
            shortcircuit_expr(&mut *e);
        }
        Expr::Call(_, es) => {
            es.into_iter().for_each(shortcircuit_expr);
        }
        Expr::Ternary(e1, e2, e3) => {
            shortcircuit_expr(&mut *e1);
            shortcircuit_expr(&mut *e2);
            shortcircuit_expr(&mut *e3);
        }
        Expr::StatementBlock(ss, e) => {
            ss.iter_mut().for_each(shortcircuit_statement);
            shortcircuit_expr(&mut *e);
        }
        Expr::Constant(_) | Expr::Id(_) => {}
        Expr::Tuple(elems) => {
            elems.iter_mut().for_each(shortcircuit_expr);
        }
        Expr::Subscript(tup, _idx) => {
            shortcircuit_expr(tup);
        }
        Expr::Allocate(_, _) => {}
        Expr::GlobalSymbol(_) => {}
    }

    // Apply transformation, only applies to expressions with And/Or
    // BinaryOperator
    if let Expr::BinaryOp(left, BinaryOperator::And, right) = e {
        // (A && B) is equivalent to (A ? B : false)
        *e = Expr::Ternary(
            left.clone(),
            right.clone(),
            Box::new(Expr::Constant(Value::Bool(false))),
        );
    } else if let Expr::BinaryOp(left, BinaryOperator::Or, right) = e {
        // (A || B) is equivalent to (A ? true : B)
        *e = Expr::Ternary(
            left.clone(),
            Box::new(Expr::Constant(Value::Bool(true))),
            right.clone(),
        );
    }
}

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;

    use indexmap::IndexMap;
    use crate::utils::t_id;
    use test_support::{
        compiler::{
            constants::LABEL_MAIN,
            passes::{ASTPass, ShortCircuiting},
            syntax_trees::{ast::*, shared::*},
        },
        *,
    };

    struct TestCase {
        ast: Program,
        inputs: VecDeque<i64>,
        expected_outputs: VecDeque<i64>,
    }

    fn assert_expr_no_and_or(e: &Expr) {
        match e {
            Expr::BinaryOp(_, BinaryOperator::And, _)
            | Expr::BinaryOp(_, BinaryOperator::Or, _) => {
                panic!("short circuiting should have removed and And/Or operators");
            }
            Expr::BinaryOp(left, _, right) => {
                assert_expr_no_and_or(&*left);
                assert_expr_no_and_or(&*right);
            }
            Expr::UnaryOp(_, expr) => {
                assert_expr_no_and_or(&*expr);
            }
            Expr::Call(_, exprs) => {
                exprs.iter().for_each(assert_expr_no_and_or);
            }
            Expr::Ternary(cond, pos, neg) => {
                assert_expr_no_and_or(cond);
                assert_expr_no_and_or(pos);
                assert_expr_no_and_or(neg);
            }
            _ => {}
        }
    }

    fn assert_statement_no_and_or(s: &Statement) {
        match s {
            Statement::Assign(_, expr) => assert_expr_no_and_or(&expr),
            Statement::Expr(expr) => assert_expr_no_and_or(&expr),
            Statement::Conditional(expr, pos, neg) => {
                assert_expr_no_and_or(&expr);
                pos.iter().for_each(assert_statement_no_and_or);
                neg.iter().for_each(assert_statement_no_and_or);
            }
            Statement::WhileLoop(expr, body) => {
                assert_expr_no_and_or(&expr);
                body.iter().for_each(assert_statement_no_and_or);
            }
            Statement::Return(expr) => {
                assert_expr_no_and_or(&expr);
            }
        }
    }

    fn execute_test_case(mut tc: TestCase) {
        tc.ast.type_check();

        println!("AST before Short Circuiting: {:?}", tc.ast);
        let mut post_run_ast = ShortCircuiting.run_pass(tc.ast);
        println!("AST after Short Circuiting: {:?}", post_run_ast);

        post_run_ast.type_check();

        post_run_ast
            .functions
            .iter()
            .flat_map(|f| &f.body)
            .for_each(assert_statement_no_and_or);

        let mut outputs = VecDeque::<i64>::new();
        ast_interpreter::interpret(&post_run_ast, &mut tc.inputs, &mut outputs);

        assert!(tc.inputs.is_empty());
        assert_eq!(outputs, tc.expected_outputs);
    }

    #[test]
    fn test_simple() {
        let tc = TestCase {
            ast: Program { functions: vec![Function { name: t_id!(LABEL_MAIN),
                body: vec![Statement::Expr(Expr::BinaryOp(
                    Box::new(Expr::Constant(Value::Bool(true))),
                    BinaryOperator::And,
                    Box::new(Expr::Constant(Value::Bool(false))),
                ))],
                types: TypeEnv::new(),
                params: IndexMap::new(),
                return_type: ValueType::IntType,
            }], function_types: TypeEnv::new()},
            inputs: VecDeque::new(),
            expected_outputs: VecDeque::new(),
        };

        execute_test_case(tc);
    }

    #[test]
    fn test_nested() {
        let tc = TestCase {
            ast: Program { functions: vec![Function { name: t_id!(LABEL_MAIN),
                body: vec![Statement::Expr(Expr::BinaryOp(
                    Box::new(Expr::Constant(Value::Bool(true))),
                    BinaryOperator::And,
                    Box::new(Expr::BinaryOp(
                        Box::new(Expr::Constant(Value::Bool(true))),
                        BinaryOperator::And,
                        Box::new(Expr::BinaryOp(
                            Box::new(Expr::Constant(Value::Bool(true))),
                            BinaryOperator::And,
                            Box::new(Expr::BinaryOp(
                                Box::new(Expr::Constant(Value::Bool(true))),
                                BinaryOperator::And,
                                Box::new(Expr::BinaryOp(
                                    Box::new(Expr::Constant(Value::Bool(true))),
                                    BinaryOperator::And,
                                    Box::new(Expr::Constant(Value::Bool(false))),
                                )),
                            )),
                        )),
                    )),
                ))],
                types: TypeEnv::new(),
                params: IndexMap::new(),
                return_type: ValueType::IntType,
            }], function_types: TypeEnv::new()},
            inputs: VecDeque::new(),
            expected_outputs: VecDeque::new(),
        };

        execute_test_case(tc);
    }

    #[test]
    fn test_comparisons_and() {
        let tc = TestCase {
            ast: Program { functions: vec![Function { name: t_id!(LABEL_MAIN),
                body: vec![
                    Statement::Expr(Expr::BinaryOp(
                        Box::new(Expr::Constant(Value::Bool(true))),
                        BinaryOperator::And,
                        Box::new(Expr::BinaryOp(
                            Box::new(Expr::Constant(Value::I64(1))),
                            BinaryOperator::Equals,
                            Box::new(Expr::Call(t_id!("read_int"), vec![])),
                        )),
                    )),
                    Statement::Expr(Expr::BinaryOp(
                        Box::new(Expr::Constant(Value::Bool(false))),
                        BinaryOperator::And,
                        Box::new(Expr::BinaryOp(
                            Box::new(Expr::Constant(Value::I64(1))),
                            BinaryOperator::Equals,
                            Box::new(Expr::Call(t_id!("read_int"), vec![])),
                        )),
                    )),
                ],
                types: TypeEnv::new(),
                params: IndexMap::new(),
                return_type: ValueType::IntType,
            }], function_types: TypeEnv::new()},
            inputs: VecDeque::from([1]),
            expected_outputs: VecDeque::new(),
        };

        execute_test_case(tc);
    }

    #[test]
    fn test_comparisons_or() {
        let tc = TestCase {
            ast: Program { functions: vec![Function { name: t_id!(LABEL_MAIN),
                body: vec![
                    Statement::Expr(Expr::BinaryOp(
                        Box::new(Expr::Constant(Value::Bool(true))),
                        BinaryOperator::Or,
                        Box::new(Expr::BinaryOp(
                            Box::new(Expr::Constant(Value::I64(1))),
                            BinaryOperator::Equals,
                            Box::new(Expr::Call(t_id!("read_int"), vec![])),
                        )),
                    )),
                    Statement::Expr(Expr::BinaryOp(
                        Box::new(Expr::Constant(Value::Bool(false))),
                        BinaryOperator::Or,
                        Box::new(Expr::BinaryOp(
                            Box::new(Expr::Constant(Value::I64(1))),
                            BinaryOperator::Equals,
                            Box::new(Expr::Call(t_id!("read_int"), vec![])),
                        )),
                    )),
                ],
                types: TypeEnv::new(),
                params: IndexMap::new(),
                return_type: ValueType::IntType,
            }], function_types: TypeEnv::new()},
            inputs: VecDeque::from([1]),
            expected_outputs: VecDeque::new(),
        };

        execute_test_case(tc);
    }
}
