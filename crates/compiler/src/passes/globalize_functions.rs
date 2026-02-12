use crate::{
    passes::ASTPass,
    syntax_trees::{ast::*, shared::*},
};

pub struct GlobalizeFunctions;

impl ASTPass for GlobalizeFunctions {
    fn run_pass(self, mut m: Program) -> Program {
        let func_types = &m.function_types;
        for f in m.functions.iter_mut() {
            for s in f.body.iter_mut() {
                globalize_for_statement(s, func_types);
            }
        }

        m
    }
}
fn globalize_for_statement(s: &mut Statement, func_types: &TypeEnv) {
    match s {
        Statement::Assign(_, expr) | Statement::Expr(expr) | Statement::Return(expr) => {
            globalize_for_expr(expr, func_types);
        }
        Statement::Conditional(cond, pos_body, neg_body) => {
            globalize_for_expr(cond, func_types);
            for s in pos_body {
                globalize_for_statement(s, func_types);
            }
            for s in neg_body {
                globalize_for_statement(s, func_types);
            }
        }
        Statement::WhileLoop(cond, body) => {
            globalize_for_expr(cond, func_types);
            for s in body {
                globalize_for_statement(s, func_types);
            }
        }
    }
}

fn globalize_for_expr(e: &mut Expr, func_types: &TypeEnv) {
    match e {
        Expr::Id(id) => {
            if func_types.contains_key(id) {
                *e = Expr::GlobalSymbol(id.clone());
            }
        }

        Expr::Call(func, args) => {
            for a in args.iter_mut() {
                globalize_for_expr(a, func_types);
            }

            globalize_for_expr(func, func_types);
        }
        Expr::BinaryOp(l, _, r) => {
            globalize_for_expr(l, func_types);
            globalize_for_expr(r, func_types);
        }
        Expr::UnaryOp(_, expr) => {
            globalize_for_expr(expr, func_types);
        }
        Expr::Ternary(cond, pos, neg) => {
            globalize_for_expr(cond, func_types);
            globalize_for_expr(pos, func_types);
            globalize_for_expr(neg, func_types);
        }
        Expr::StatementBlock(statements, expr) => {
            for s in statements {
                globalize_for_statement(s, func_types);
            }
            globalize_for_expr(expr, func_types);
        }
        Expr::Tuple(elems) => {
            for e in elems {
                globalize_for_expr(e, func_types);
            }
        }
        Expr::Subscript(expr, _) => {
            globalize_for_expr(expr, func_types);
        }

        Expr::Allocate(_, _) | Expr::Constant(_) | Expr::GlobalSymbol(_) => {}
    }
}

#[cfg(test)]
mod tests {
    use crate::utils::t_id;
    use indexmap::IndexMap;
    use test_support::compiler::{
        constants::LABEL_MAIN,
        passes::{ASTPass, GlobalizeFunctions},
        syntax_trees::{ast::*, shared::*},
    };

    #[test]
    fn test_globalize_call_func() {
        // Call(Id("foo"), [Const(1)]) should become Call(GlobalSymbol("foo"), [Const(1)])
        // when "foo" is in function_types
        let program = Program {
            functions: vec![Function {
                name: t_id!(LABEL_MAIN),
                body: vec![Statement::Expr(Expr::Call(
                    Box::new(Expr::Id(t_id!("foo"))),
                    vec![Expr::Constant(Value::I64(1))],
                ))],
                types: TypeEnv::new(),
                params: IndexMap::new(),
                return_type: ValueType::IntType,
            }],
            function_types: TypeEnv::from([(
                t_id!("foo"),
                ValueType::FunctionType(vec![ValueType::IntType], Box::new(ValueType::IntType)),
            )]),
        };

        let result = GlobalizeFunctions.run_pass(program);
        let body = &result.functions[0].body;

        assert_eq!(
            body[0],
            Statement::Expr(Expr::Call(
                Box::new(Expr::GlobalSymbol(t_id!("foo"))),
                vec![Expr::Constant(Value::I64(1))],
            ))
        );
    }

    #[test]
    fn test_globalize_func_as_arg() {
        // Call(GlobalSymbol("bar"), [Id("foo")]) should globalize the arg Id("foo")
        // to GlobalSymbol("foo") when "foo" is a function
        let program = Program {
            functions: vec![Function {
                name: t_id!(LABEL_MAIN),
                body: vec![Statement::Expr(Expr::Call(
                    Box::new(Expr::GlobalSymbol(t_id!("bar"))),
                    vec![Expr::Id(t_id!("foo"))],
                ))],
                types: TypeEnv::new(),
                params: IndexMap::new(),
                return_type: ValueType::IntType,
            }],
            function_types: TypeEnv::from([
                (
                    t_id!("foo"),
                    ValueType::FunctionType(vec![], Box::new(ValueType::IntType)),
                ),
                (
                    t_id!("bar"),
                    ValueType::FunctionType(
                        vec![ValueType::FunctionType(vec![], Box::new(ValueType::IntType))],
                        Box::new(ValueType::NoneType),
                    ),
                ),
            ]),
        };

        let result = GlobalizeFunctions.run_pass(program);
        let body = &result.functions[0].body;

        assert_eq!(
            body[0],
            Statement::Expr(Expr::Call(
                Box::new(Expr::GlobalSymbol(t_id!("bar"))),
                vec![Expr::GlobalSymbol(t_id!("foo"))],
            ))
        );
    }

    #[test]
    fn test_globalize_leaves_local_vars_alone() {
        // Call(Id("foo"), [Id("x")]) where "x" is NOT a function should leave "x" as Id
        let program = Program {
            functions: vec![Function {
                name: t_id!(LABEL_MAIN),
                body: vec![Statement::Expr(Expr::Call(
                    Box::new(Expr::Id(t_id!("foo"))),
                    vec![Expr::Id(t_id!("x"))],
                ))],
                types: TypeEnv::new(),
                params: IndexMap::new(),
                return_type: ValueType::IntType,
            }],
            function_types: TypeEnv::from([(
                t_id!("foo"),
                ValueType::FunctionType(vec![ValueType::IntType], Box::new(ValueType::IntType)),
            )]),
        };

        let result = GlobalizeFunctions.run_pass(program);
        let body = &result.functions[0].body;

        assert_eq!(
            body[0],
            Statement::Expr(Expr::Call(
                Box::new(Expr::GlobalSymbol(t_id!("foo"))),
                vec![Expr::Id(t_id!("x"))], // x stays as Id since it's not a function
            ))
        );
    }
}
