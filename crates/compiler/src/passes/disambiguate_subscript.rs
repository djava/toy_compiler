use crate::{
    constants::{FN_ASSIGN_TO_ARRAY_ELEM, FN_SUBSCRIPT_ARRAY},
    passes::ASTPass,
    syntax_trees::{ast::*, shared::*},
    utils::global,
};

#[derive(Debug)]
pub struct DisambiguateSubscript;

impl ASTPass for DisambiguateSubscript {
    fn run_pass(self, mut m: Program) -> Program {
        for f in m.functions.iter_mut() {
            for s in f.body.iter_mut() {
                disambiguate_for_statement(s, &mut f.types);
            }
        }

        m
    }
}

fn disambiguate_for_statement(s: &mut Statement, type_env: &mut TypeEnv) {
    match s {
        Statement::Assign(dest, expr, type_hint) => {
            disambiguate_for_expr(expr, type_hint.as_ref(), type_env);

            if let AssignDest::ComplexSubscript(complex) = dest {
                let container = &mut complex.container;
                let container_type = container.type_check(type_env, &None);
                if let ValueType::TupleType(elem_types) = &container_type {
                    simplify_subscript_assign_for_tuple(s, elem_types);
                } else if let ValueType::ArrayType(_, _) = &container_type {
                    *s = Statement::Expr(Expr::Call(
                        Box::new(Expr::GlobalSymbol(global!(FN_ASSIGN_TO_ARRAY_ELEM))),
                        vec![
                            complex.container.clone(),
                            complex.index.clone(),
                            expr.clone(),
                        ],
                    ));
                }
            } else if let AssignDest::Subscript(dest_id, idx) = dest {
                let dest_id_type = type_env
                    .get(dest_id)
                    .expect("Couldn't find dest type for subscript assign");
                if let ValueType::ArrayType(_, _) = dest_id_type {
                    *s = Statement::Expr(Expr::Call(
                        Box::new(Expr::GlobalSymbol(global!(FN_ASSIGN_TO_ARRAY_ELEM))),
                        vec![
                            Expr::Id(dest_id.clone()),
                            Expr::Constant(Value::I64(*idx)),
                            expr.clone(),
                        ],
                    ));
                }
            }
        }
        Statement::Expr(expr) => {
            disambiguate_for_expr(expr, None, type_env);
        }
        Statement::Conditional(expr, statements, statements1) => {
            disambiguate_for_expr(expr, None, type_env);
            for s in statements {
                disambiguate_for_statement(s, type_env);
            }
            for s in statements1 {
                disambiguate_for_statement(s, type_env);
            }
        }
        Statement::WhileLoop(expr, statements) => {
            disambiguate_for_expr(expr, None, type_env);
            for s in statements {
                disambiguate_for_statement(s, type_env);
            }
        }
        Statement::Return(expr) => {
            disambiguate_for_expr(expr, None, type_env);
        }
    }
}

fn disambiguate_for_expr(e: &mut Expr, type_hint: Option<&ValueType>, type_env: &mut TypeEnv) {
    match e {
        Expr::BinaryOp(l, _, r) => {
            disambiguate_for_expr(l, None, type_env);
            disambiguate_for_expr(r, None, type_env);
        }
        Expr::UnaryOp(_, expr) => {
            disambiguate_for_expr(expr, None, type_env);
        }
        Expr::Call(func, args) => {
            disambiguate_for_expr(func, None, type_env);
            for a in args {
                disambiguate_for_expr(a, None, type_env);
            }
        }
        Expr::Ternary(cond, pos, neg) => {
            disambiguate_for_expr(cond, None, type_env);
            disambiguate_for_expr(pos, type_hint, type_env);
            disambiguate_for_expr(neg, type_hint, type_env);
        }
        Expr::StatementBlock(statements, expr) => {
            for s in statements {
                disambiguate_for_statement(s, type_env);
            }
            disambiguate_for_expr(expr, type_hint, type_env);
        }
        Expr::Tuple(exprs) => {
            let elem_type_hints: Vec<_> = if let Some(ValueType::TupleType(elems_types)) = type_hint
            {
                elems_types.iter().map(Some).collect()
            } else {
                std::iter::repeat_n(None, exprs.len()).collect()
            };

            for (elem, hint) in exprs.iter_mut().zip(elem_type_hints) {
                disambiguate_for_expr(elem, hint, type_env);
            }
        }
        Expr::Array(exprs) => {
            let elem_type_hint = if let Some(ValueType::ArrayType(elems_type, _)) = type_hint {
                Some(&**elems_type)
            } else {
                None
            };

            for e in exprs {
                disambiguate_for_expr(e, elem_type_hint, type_env);
            }
        }
        Expr::Subscript(expr, idx) => {
            disambiguate_for_expr(expr, None, type_env);

            let expr_type = expr.type_check(type_env, &type_hint.cloned());
            if let ValueType::ArrayType(_, _) = expr_type {
                *e = Expr::Call(
                    Box::new(Expr::GlobalSymbol(global!(FN_SUBSCRIPT_ARRAY))),
                    vec![*expr.clone(), *idx.clone()],
                );
            }
        }

        Expr::Constant(_)
        | Expr::Allocate(_, _)
        | Expr::GlobalSymbol(_)
        | Expr::Id(_)
        | Expr::Closure(..) => {}

        Expr::Lambda(_) => panic!("Lambda's shouldnt still exist"),
    }
}

fn simplify_subscript_assign_for_tuple(s: &mut Statement, elem_types: &Vec<ValueType>) {
    match s {
        Statement::Assign(AssignDest::ComplexSubscript(complex), val, _) => {
            let idx_const = if let Expr::Constant(Value::I64(val)) = complex.index {
                val
            } else {
                panic!("Non-const-int index to tuple");
            };

            if let Expr::Id(id) = &complex.container {
                // Simple case
                *s = Statement::Assign(
                    AssignDest::Subscript(id.clone(), idx_const),
                    val.clone(),
                    Some(elem_types[idx_const as usize].clone()),
                );
            } else {
                // Complex case - use intermediary variable
                let intermediary_id = Identifier::new_ephemeral();
                let elem_type_hint = elem_types.get(idx_const as usize).cloned();

                *s = Statement::Expr(Expr::StatementBlock(
                    vec![
                        Statement::Assign(
                            AssignDest::Id(intermediary_id.clone()),
                            complex.container.clone(),
                            Some(ValueType::TupleType(elem_types.clone())),
                        ),
                        Statement::Assign(
                            AssignDest::Subscript(intermediary_id, idx_const),
                            val.clone(),
                            elem_type_hint,
                        ),
                    ],
                    Box::new(Expr::Constant(Value::None)),
                ));
            };
        }
        _ => panic!("wrong args to simplify_subscript_assign_for_tuple"),
    }
}
