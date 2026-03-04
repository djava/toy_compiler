
use crate::{
    constants::*,
    passes::ASTPass,
    syntax_trees::{ast::*, shared::*},
    utils::global,
};

/// `InjectAllocations` Pass
///
/// Replaces tuple, array, and closure construction expressions with
/// explicit GC-aware heap allocation sequences:
/// 1. Checks `free_ptr` against `fromspace_end`
/// 2. Calls `gc_collect` if necessary
/// 3. Bumps the allocation pointer via `Expr::Allocate`
/// 4. Initializes each element via subscript assignments.
///
/// Must run BEFORE `DeclosurizeCalls`, or else closures won't have
/// their allocations injected.
///
/// It is mandatory to run this pass
///
/// Pre-conditions:
/// - `RemoveComplexOperands` (generated statements must not contain
///   complex operands)
/// - `ClosurizeFunctions`
/// - `ClosurizeLambdas`
/// - HAS NOT YET RUN `DeclosurizeCalls`
///
/// Post-conditions:
/// - No `Expr::Tuple`, `Expr::Array`, or `Expr::Closure` remain, and all are
///   replaced with `StatementBlock` allocation sequences using
///   `Expr::Allocate`
#[derive(Debug)]
pub struct InjectAllocations;

impl ASTPass for InjectAllocations {
    fn run_pass(self, mut m: Program) -> Program {
        for f in m.functions.iter_mut() {
            for s in f.body.iter_mut() {
                replace_tuples_in_statement(s, &mut f.types);
            }
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
        Statement::Assign(_, expr, _) | Statement::Return(expr) | Statement::Expr(expr) => {
            replace_tuples_in_expr(expr, type_env)
        }
    }
}

fn replace_tuples_in_expr(expr: &mut Expr, type_env: &mut TypeEnv) {
    match expr {
        Expr::BinaryOp(l, _, r) => {
            replace_tuples_in_expr(l, type_env);
            replace_tuples_in_expr(r, type_env);
        }
        Expr::UnaryOp(_, expr) => {
            replace_tuples_in_expr(expr, type_env);
        }
        Expr::Call(_, args) => {
            for a in args {
                replace_tuples_in_expr(a, type_env);
            }
        }
        Expr::Ternary(cond, pos, neg) => {
            replace_tuples_in_expr(cond, type_env);
            replace_tuples_in_expr(pos, type_env);
            replace_tuples_in_expr(neg, type_env);
        }
        Expr::StatementBlock(body, expr) => {
            for s in body {
                replace_tuples_in_statement(s, type_env);
            }
            replace_tuples_in_expr(expr, type_env);
        }
        Expr::Tuple(elems) => {
            let tup_type = ValueType::TupleType(
                elems
                    .iter_mut()
                    .map(|e| e.type_check(type_env, &None))
                    .collect(),
            );
            *expr = get_initialize_allocation_expr(elems, tup_type);
        }
        Expr::Array(elems) => {
            let arr_type = ValueType::ArrayType(
                Box::new(
                    elems
                        .get_mut(0)
                        .map(|e| e.type_check(type_env, &None))
                        .unwrap_or(ValueType::Indeterminate),
                ),
                elems.len(),
            );
            *expr = get_initialize_allocation_expr(elems, arr_type);
        }
        Expr::Closure(id, captures) => {
            let tup_type = ValueType::TupleType(
                std::iter::once(type_env[id].clone())
                    .chain(captures.iter().map(|c| type_env[c].clone()))
                    .collect(),
            );
            let mut elems = std::iter::once(Expr::GlobalSymbol(id.clone()))
                .chain(captures.iter().map(|i| Expr::Id(i.clone())))
                .collect();
            *expr = get_initialize_allocation_expr(&mut elems, tup_type);
        }
        Expr::Subscript(expr, _) => {
            replace_tuples_in_expr(expr, type_env);
        }
        Expr::Constant(_) | Expr::Id(_) | Expr::Allocate(_, _) | Expr::GlobalSymbol(_) => {}
        Expr::Lambda(_) => panic!("Should've been removed already"),
    }
}

fn get_initialize_allocation_expr(elems: &mut Vec<Expr>, tup_type: ValueType) -> Expr {
    let free_ptr = Expr::GlobalSymbol(global!(GC_FREE_PTR));
    let fromspace_end = Expr::GlobalSymbol(global!(GC_FROMSPACE_END));
    let collect = |n| {
        Expr::Call(
            Box::new(Expr::GlobalSymbol(global!(FN_GC_COLLECT))),
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
            None,
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
            None,
        ),
    ];

    statements.extend(elems.iter().enumerate().map(|(idx, e)| {
        Statement::Assign(
            AssignDest::Subscript(out_ephemeral.clone(), idx as i64),
            e.clone(),
            None,
        )
    }));

    Expr::StatementBlock(statements, Box::new(Expr::Id(out_ephemeral)))
}
