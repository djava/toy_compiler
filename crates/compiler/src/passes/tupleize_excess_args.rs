use indexmap::IndexMap;

use crate::{
    constants::MAX_REGISTER_ARGS,
    passes::ASTPass,
    syntax_trees::{ast::*, shared::*},
};

pub struct TupleizeExcessArgs;

impl ASTPass for TupleizeExcessArgs {
    fn run_pass(self, mut m: Program) -> Program {
        for f in m.functions.iter_mut() {
            // Any function with more than 6 args has its 6th-n'th
            // args collected into a tuple to pass as one arg,
            // instead of doing the typical stack parameter passing.
            if f.params.len() > MAX_REGISTER_ARGS {
                // The first `MAX_REGISTER_ARGS - 1` args are kept normal
                let mut new_params: IndexMap<_, _> = f
                    .params
                    .iter()
                    .take(MAX_REGISTER_ARGS - 1)
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .collect();

                // Any args past the `MAX_REGISTER_ARGS - 1`-th is
                // "excess" and grouped up into a tuple
                let (excess_names, excess_types): (Vec<_>, Vec<_>) =
                    f.params.iter().skip(MAX_REGISTER_ARGS - 1).unzip();

                let tuple_id = Identifier::new_ephemeral();
                new_params.insert(
                    tuple_id.clone(),
                    ValueType::TupleType(excess_types.into_iter().cloned().collect()),
                );

                // Replace any uses of an excess arg with a subscript
                // into the excess-args tuple
                for s in f.body.iter_mut() {
                    replace_excess_use_for_statement(s, &excess_names, &tuple_id);
                }

                f.params = new_params;

                // Update function_types map for the new type
                if let ValueType::FunctionType(old_args_types) = &m.function_types[&f.name] {
                    let mut new_args_types: Vec<_> = old_args_types.iter().take(MAX_REGISTER_ARGS - 1).cloned().collect();
                    
                    let excess_args_types = old_args_types.iter().skip(MAX_REGISTER_ARGS - 1).cloned().collect();
                    new_args_types.push(ValueType::TupleType(excess_args_types));

                    m.function_types.insert(f.name.clone(), ValueType::FunctionType(new_args_types));
                } else {
                    panic!("Couldn't find type-checker entry for {:?}", f.name);
                }
            }

            // Also, any calls to functions with >6 args will have the
            // 6th through n'th collected into a tuple. We know the
            // called function is expecting this, because of the
            // operation that happens right above this block
            for s in f.body.iter_mut() {
                replace_excess_calls_for_statement(s);
            }
        }

        m
    }
}

fn replace_excess_use_for_statement(
    s: &mut Statement,
    excess_names: &Vec<&Identifier>,
    tuple_id: &Identifier,
) {
    match s {
        Statement::Assign(assign_dest, expr) => {
            match assign_dest {
                AssignDest::Id(id) => {
                    if let Some(subscript_idx) = excess_names.iter().position(|x| *x == id) {
                        *assign_dest =
                            AssignDest::Subscript(tuple_id.clone(), subscript_idx as i64);
                    }
                }
                AssignDest::Subscript(id, _idx) => {
                    if let Some(_subscript_idx) = excess_names.iter().position(|x| *x == id) {
                        // *assign_dest = AssignDest::Subscript((), ())
                        todo!(
                            "Need to replace AssignDest::Subscript first arg with Box<AssignDest> to allow x[0][0] = .."
                        );
                    }
                }
            }

            replace_excess_use_for_expr(expr, excess_names, tuple_id);
        }
        Statement::Expr(expr) => replace_excess_use_for_expr(expr, excess_names, tuple_id),
        Statement::Conditional(cond, pos_body, neg_body) => {
            replace_excess_use_for_expr(cond, excess_names, tuple_id);
            for s in pos_body {
                replace_excess_use_for_statement(s, excess_names, tuple_id);
            }
            for s in neg_body {
                replace_excess_use_for_statement(s, excess_names, tuple_id);
            }
        }
        Statement::WhileLoop(cond, body) => {
            replace_excess_use_for_expr(cond, excess_names, tuple_id);
            for s in body {
                replace_excess_use_for_statement(s, excess_names, tuple_id);
            }
        }
    }
}

fn replace_excess_use_for_expr(
    expr: &mut Expr,
    excess_names: &Vec<&Identifier>,
    tuple_id: &Identifier,
) {
    match expr {
        Expr::Id(id) => {
            if let Some(subscript_idx) = excess_names.iter().position(|x| *x == id) {
                *expr = Expr::Subscript(Box::new(Expr::Id(tuple_id.clone())), subscript_idx as i64);
            }
        }
        Expr::BinaryOp(l, _, r) => {
            replace_excess_use_for_expr(l, excess_names, tuple_id);
            replace_excess_use_for_expr(r, excess_names, tuple_id);
        }
        Expr::UnaryOp(_, e) => {
            replace_excess_use_for_expr(e, excess_names, tuple_id);
        }
        Expr::Call(_, args) => {
            for a in args {
                replace_excess_use_for_expr(a, excess_names, tuple_id);
            }
        }
        Expr::Ternary(cond, pos, neg) => {
            replace_excess_use_for_expr(cond, excess_names, tuple_id);
            replace_excess_use_for_expr(pos, excess_names, tuple_id);
            replace_excess_use_for_expr(neg, excess_names, tuple_id);
        }
        Expr::StatementBlock(statements, expr) => {
            for s in statements {
                replace_excess_use_for_statement(s, excess_names, tuple_id);
            }
            replace_excess_use_for_expr(expr, excess_names, tuple_id);
        }
        Expr::Tuple(elems) => {
            for e in elems {
                replace_excess_use_for_expr(e, excess_names, tuple_id);
            }
        }
        Expr::Subscript(expr, _) => {
            replace_excess_use_for_expr(expr, excess_names, tuple_id);
        }

        Expr::Allocate(_, _) | Expr::Constant(_) | Expr::GlobalSymbol(_) => {}
    }
}

fn replace_excess_calls_for_statement(s: &mut Statement) {
    match s {
        Statement::Assign(_, expr) | Statement::Expr(expr) => {
            replace_excess_calls_for_expr(expr);
        }
        Statement::Conditional(cond, pos_body, neg_body) => {
            replace_excess_calls_for_expr(cond);
            for s in pos_body {
                replace_excess_calls_for_statement(s);
            }
            for s in neg_body {
                replace_excess_calls_for_statement(s);
            }
        }
        Statement::WhileLoop(cond, body) => {
            replace_excess_calls_for_expr(cond);
            for s in body {
                replace_excess_calls_for_statement(s);
            }
        }
    }
}

fn replace_excess_calls_for_expr(e: &mut Expr) {
    match e {
        Expr::Call(_, args) => {
            if args.len() > MAX_REGISTER_ARGS {
                let mut new_args: Vec<_> = args.iter().take(MAX_REGISTER_ARGS - 1).cloned().collect();

                let excess_args = args.iter().skip(MAX_REGISTER_ARGS - 1).cloned();
                let excess_tup = Expr::Tuple(excess_args.collect());
                new_args.push(excess_tup);

                *args = new_args;
            }
        },
        Expr::BinaryOp(l, _, r) => {
            replace_excess_calls_for_expr(l);
            replace_excess_calls_for_expr(r);
        }
        Expr::UnaryOp(_, expr) => {
            replace_excess_calls_for_expr(expr);
        }
        Expr::Ternary(cond, pos, neg) => {
            replace_excess_calls_for_expr(cond);
            replace_excess_calls_for_expr(pos);
            replace_excess_calls_for_expr(neg);
        }
        Expr::StatementBlock(statements, expr) => {
            for s in statements {
                replace_excess_calls_for_statement(s);
            }
            replace_excess_calls_for_expr(expr);
        }
        Expr::Tuple(elems) => {
            for e in elems {
                replace_excess_calls_for_expr(e);
            }
        }
        Expr::Subscript(expr, _) => {
            replace_excess_calls_for_expr(expr);
        }

        Expr::Id(_) | Expr::Allocate(_, _) | Expr::Constant(_) | Expr::GlobalSymbol(_) => {}
    }
}
