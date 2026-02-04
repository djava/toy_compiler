use std::sync::Arc;

use crate::{constants::*, syntax_trees::{shared::*, ast::*}, passes::ASTPass, utils::type_check_ast_expr};

pub struct InjectAllocations;

impl ASTPass for InjectAllocations {
    fn run_pass(self, mut m: Module) -> Module {
        for s in m.body.iter_mut() {
            replace_tuples_in_statement(s, &mut m.types);
        }

        m
    }
}

fn replace_tuples_in_statement(statement: &mut Statement, type_env: &mut TypeEnv) {
    match statement {
        Statement::Conditional(cond, pos_body, neg_body) => {
            replace_tuples_in_expr(cond, type_env);

            for s in pos_body {
                replace_tuples_in_statement(s, type_env);
            }

            for s in neg_body {
                replace_tuples_in_statement(s, type_env);
            }
        }
        Statement::WhileLoop(cond, body) => {
            replace_tuples_in_expr(cond, type_env);

            for s in body {
                replace_tuples_in_statement(s, type_env);
            }
        }
        Statement::Assign(_, expr)
        | Statement::Expr(expr) => replace_tuples_in_expr(expr, type_env),
    }
}

fn replace_tuples_in_expr(expr: &mut Expr, type_env: &mut TypeEnv) {
    match expr {
        Expr::BinaryOp(l, _, r) => {
            replace_tuples_in_expr(l, type_env);
            replace_tuples_in_expr(r, type_env);
        },
        Expr::UnaryOp(_, expr) => {
            replace_tuples_in_expr(expr, type_env);
        },
        Expr::Call(_, args) => {
            for a in args {
                replace_tuples_in_expr(a, type_env);
            }
        },
        Expr::Ternary(cond, pos, neg) => {
            replace_tuples_in_expr(cond, type_env);
            replace_tuples_in_expr(pos, type_env);
            replace_tuples_in_expr(neg, type_env);
        },
        Expr::StatementBlock(body, expr) => {
            for s in body {
                replace_tuples_in_statement(s, type_env);
            }
            replace_tuples_in_expr(expr, type_env);
        }

        Expr::Tuple(elems) => {
            let tup_type = ValueType::TupleType(
                elems
                    .iter()
                    .map(|e| type_check_ast_expr(e, type_env))
                    .collect(),
            );
            *expr = get_initialize_tuple_expr(elems, tup_type);
        }

        Expr::Subscript(expr, _) => {
            replace_tuples_in_expr(expr, type_env);
        }

        Expr::Constant(_) | Expr::Id(_) | Expr::Allocate(_, _) | Expr::GlobalSymbol(_) => {}
    }
}

fn get_initialize_tuple_expr(elems: &mut Vec<Expr>, tup_type: ValueType) -> Expr {
    let free_ptr = Expr::GlobalSymbol(Arc::from(GC_FREE_PTR));
    let fromspace_end = Expr::GlobalSymbol(Arc::from(GC_FROMSPACE_END));
    let collect = |n| {
        Expr::Call(
            Identifier::from(GC_COLLECT),
            vec![Expr::Constant(Value::I64(n))],
        )
    };

    let bytes = WORD_SIZE + WORD_SIZE * elems.len() as i64;

    let cmp_ephemeral = Identifier::new_ephemeral();
    let out_ephemeral = Identifier::new_ephemeral();

    // Important: We're going to run this pass AFTER
    // RemoveComplexOperands, so any of the expressions in statements
    // must not include complex operands.
    let mut statements = vec![
        Statement::Assign(
            AssignDest::Id(cmp_ephemeral.clone()),
            Expr::BinaryOp(
                Box::new(free_ptr),
                BinaryOperator::Add,
                Box::new(Expr::Constant(Value::I64(bytes))),
            ),
        ),
        Statement::Conditional(
            Expr::BinaryOp(
                Box::new(Expr::Id(cmp_ephemeral)),
                BinaryOperator::Less,
                Box::new(fromspace_end),
            ),
            vec![Statement::Expr(Expr::Constant(Value::I64(0)))],
            vec![Statement::Expr(collect(bytes))],
        ),
        Statement::Assign(
            AssignDest::Id(out_ephemeral.clone()),
            Expr::Allocate(bytes as usize, tup_type),
        ),
    ];

    statements.extend(
        elems.iter().enumerate().map(|(idx, e)| {
            Statement::Assign(AssignDest::Subscript(out_ephemeral.clone(), idx as i64), e.clone())
        }),
    );

    Expr::StatementBlock(statements, Box::new(Expr::Id(out_ephemeral)))
}
