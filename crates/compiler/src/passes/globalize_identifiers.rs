
use crate::{
    passes::ASTPass,
    syntax_trees::{ast::*, shared::*},
};

/// `GlobalizeIdentifiers` Pass
///
/// Converts local identifiers to global ones when they are present in
/// the global namespace, and their surrounding `ast::Expr::Id`'s into
/// `ast::Expr::GlobalSymbol`. Relies on
/// `ast::Program::populate_globals` from `type_check` to determine
/// what's in the global namespace.
///
/// It is mandatory to run this pass
///
/// Pre-conditions: None
///
/// Post-conditions:
/// - Any Identifier used in the program whose name is in the global
///   namespace will be replaced with a global identifier and its
///   encoding `Expr::Id` with an `Expr::GlobalSymbol`
#[derive(Debug)]
pub struct GlobalizeIdentifiers;

impl ASTPass for GlobalizeIdentifiers {
    fn run_pass(self, mut m: Program) -> Program {
        m.populate_globals();
        let global_types = &m.global_types;
        for f in m.functions.iter_mut() {
            for s in f.body.iter_mut() {
                globalize_for_statement(s, global_types);
            }
        }

        m
    }
}

fn globalize_for_statement(s: &mut Statement, global_types: &TypeEnv) {
    match s {
        Statement::Assign(_, expr, _) | Statement::Expr(expr) | Statement::Return(expr) => {
            globalize_for_expr(expr, global_types);
        }
        Statement::Conditional(cond, pos_body, neg_body) => {
            globalize_for_expr(cond, global_types);
            for s in pos_body {
                globalize_for_statement(s, global_types);
            }
            for s in neg_body {
                globalize_for_statement(s, global_types);
            }
        }
        Statement::WhileLoop(cond, body) => {
            globalize_for_expr(cond, global_types);
            for s in body {
                globalize_for_statement(s, global_types);
            }
        }
    }
}

fn globalize_for_expr(e: &mut Expr, global_types: &TypeEnv) {
    match e {
        Expr::Id(id) => {
            if let Identifier::Local(name, _) = id
                && global_types.contains_key(&Identifier::Global(name.clone()))
            {
                *id = Identifier::Global(name.clone());
            }

            if global_types.contains_key(id) {
                *e = Expr::GlobalSymbol(id.clone());
            }
        }
        Expr::Call(func, args) => {
            for a in args.iter_mut() {
                globalize_for_expr(a, global_types);
            }

            globalize_for_expr(func, global_types);
        }
        Expr::BinaryOp(l, _, r) => {
            globalize_for_expr(l, global_types);
            globalize_for_expr(r, global_types);
        }
        Expr::UnaryOp(_, expr) => {
            globalize_for_expr(expr, global_types);
        }
        Expr::Ternary(cond, pos, neg) => {
            globalize_for_expr(cond, global_types);
            globalize_for_expr(pos, global_types);
            globalize_for_expr(neg, global_types);
        }
        Expr::StatementBlock(statements, expr) => {
            for s in statements {
                globalize_for_statement(s, global_types);
            }
            globalize_for_expr(expr, global_types);
        }
        Expr::Tuple(elems) | Expr::Array(elems) => {
            for e in elems {
                globalize_for_expr(e, global_types);
            }
        }
        Expr::Subscript(expr, _) => {
            globalize_for_expr(expr, global_types);
        }

        Expr::Lambda(_)
        | Expr::Closure(..)
        | Expr::Allocate(_, _)
        | Expr::Constant(_)
        | Expr::GlobalSymbol(_) => {}
    }
}

#[cfg(test)]
mod tests {
    use crate::utils::{t_global, t_local};
    use indexmap::IndexMap;
    use test_support::compiler::{
        constants::LABEL_MAIN,
        passes::{ASTPass, GlobalizeIdentifiers},
        syntax_trees::{ast::*, shared::*},
    };

    #[test]
    fn test_globalize_call_func() {
        // Call(Id("foo"), [Const(1)]) should become Call(GlobalSymbol("foo"), [Const(1)])
        // when "foo" is in global_types
        let program = Program {
            functions: vec![
                Function {
                    name: t_global!(LABEL_MAIN),
                    body: vec![Statement::Expr(Expr::Call(
                        Box::new(Expr::Id(t_local!("foo", t_global!(LABEL_MAIN)))),
                        vec![Expr::Constant(Value::I64(1))],
                    ))],
                    types: TypeEnv::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                },
                Function {
                    name: t_global!("foo"),
                    body: vec![],
                    types: TypeEnv::new(),
                    params: IndexMap::from_iter([(
                        t_local!("a", t_global!("foo")),
                        ValueType::IntType,
                    )]),
                    return_type: ValueType::IntType,
                },
            ],
            global_types: TypeEnv::new(),
        };

        let result = GlobalizeIdentifiers.run_pass(program);
        let body = &result.functions[0].body;

        assert_eq!(
            body[0],
            Statement::Expr(Expr::Call(
                Box::new(Expr::GlobalSymbol(t_global!("foo"))),
                vec![Expr::Constant(Value::I64(1))],
            ))
        );
    }

    #[test]
    fn test_globalize_func_as_arg() {
        // Call(GlobalSymbol("bar"), [Id("foo")]) should globalize the arg Id("foo")
        // to GlobalSymbol("foo") when "foo" is a function
        let program = Program {
            functions: vec![
                Function {
                    name: t_global!(LABEL_MAIN),
                    body: vec![Statement::Expr(Expr::Call(
                        Box::new(Expr::Id(t_local!("bar", t_global!(LABEL_MAIN)))),
                        vec![Expr::Id(t_local!("foo", t_global!(LABEL_MAIN)))],
                    ))],
                    types: TypeEnv::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                },
                Function {
                    name: t_global!("foo"),
                    body: vec![],
                    types: TypeEnv::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                },
                Function {
                    name: t_global!("bar"),
                    body: vec![],
                    types: TypeEnv::new(),
                    params: IndexMap::from_iter([(
                        t_local!("a", t_global!("bar")),
                        ValueType::FunctionType(vec![], Box::new(ValueType::IntType)),
                    )]),
                    return_type: ValueType::NoneType,
                },
            ],
            global_types: TypeEnv::new(),
        };

        let result = GlobalizeIdentifiers.run_pass(program);
        let body = &result.functions[0].body;

        assert_eq!(
            body[0],
            Statement::Expr(Expr::Call(
                Box::new(Expr::GlobalSymbol(t_global!("bar"))),
                vec![Expr::GlobalSymbol(t_global!("foo"))],
            ))
        );
    }

    #[test]
    fn test_globalize_leaves_local_vars_alone() {
        // Call(Id("foo"), [Id("x")]) where "x" is NOT a function should leave "x" as Id
        let program = Program {
            functions: vec![
                Function {
                    name: t_global!(LABEL_MAIN),
                    body: vec![Statement::Expr(Expr::Call(
                        Box::new(Expr::Id(t_local!("foo", t_global!(LABEL_MAIN)))),
                        vec![Expr::Id(t_local!("x", t_global!(LABEL_MAIN)))],
                    ))],
                    types: TypeEnv::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                },
                Function {
                    name: t_global!("foo"),
                    body: vec![],
                    types: TypeEnv::new(),
                    params: IndexMap::from_iter([(
                        t_local!("a", t_global!("foo")),
                        ValueType::IntType,
                    )]),
                    return_type: ValueType::IntType,
                },
            ],
            global_types: TypeEnv::new(),
        };

        let result = GlobalizeIdentifiers.run_pass(program);
        let body = &result.functions[0].body;

        assert_eq!(
            body[0],
            Statement::Expr(Expr::Call(
                Box::new(Expr::GlobalSymbol(t_global!("foo"))),
                vec![Expr::Id(t_local!("x", t_global!(LABEL_MAIN)))], // x stays as Id since it's not a function
            ))
        );
    }
}
