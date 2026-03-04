
use crate::{
    constants::EXTERNED_FUNCTIONS,
    passes::ASTPass,
    syntax_trees::{ast::*, shared::*},
    utils::closurize_type,
};

/// `ClosurizeFunctions` Pass
///
/// Prepares all named top-level functions for the uniform closure
/// calling convention by (1) inserting an empty captures-tuple as the
/// first parameter of every function, and (2) replacing any `Expr::Id`
/// or `Expr::GlobalSymbol` that refers to a named function with
/// `Expr::Closure`. Adjusts types of the corresponding identifiers as
/// well.
///
/// MUST run before `ClosurizeLambdas` to avoid double-adding captures
/// params.
///
/// Note: All operations exclude extern'ed functions
///
/// It is mandatory to run this pass
/// 
/// Pre-conditions:
/// - `TypeCheck` (`global_types` must be populated),
///
/// Post-conditions:
/// - All functions have a captures-tuple as their first parameter
/// - All first-class function references are Expr::Closure
/// - `TypeCheck` pass must be run again
#[derive(Debug)]
pub struct ClosurizeFunctions;

impl ASTPass for ClosurizeFunctions {
    fn run_pass(self, mut m: Program) -> Program {
        for f in m.functions.iter_mut() {
            // Insert a captures parameter to the function to unify the
            // calling procedure with capturing lambdas
            let closure_type = ValueType::TupleType(vec![ValueType::FunctionType(
                std::iter::once(ValueType::TupleType(vec![]))
                    .chain(f.params.iter().map(|(_, typ)| typ.clone()))
                    .collect(),
                Box::new(f.return_type.clone()),
            )]);
            f.params
                .insert_before(0, Identifier::new_ephemeral(), closure_type);

            for s in f.body.iter_mut() {
                closurize_func_references_for_statement(s, &m.global_types);
            }

            for (_, typ) in f.params.iter_mut() {
                closurize_type(typ);
            }
        }

        m
    }
}

fn closurize_func_references_for_statement(s: &mut Statement, func_env: &TypeEnv) {
    match s {
        Statement::Assign(_, expr, type_hint) => {
            closurize_func_references_for_expr(expr, func_env);

            // Closurize the type for any type elements of type-hints
            if let Some(vt) = type_hint {
                closurize_type(vt);
            }
        }
        Statement::Expr(expr) => closurize_func_references_for_expr(expr, func_env),
        Statement::Conditional(cond, pos_body, neg_body) => {
            closurize_func_references_for_expr(cond, func_env);
            for s in pos_body {
                closurize_func_references_for_statement(s, func_env);
            }
            for s in neg_body {
                closurize_func_references_for_statement(s, func_env);
            }
        }
        Statement::WhileLoop(cond, body) => {
            closurize_func_references_for_expr(cond, func_env);
            for s in body {
                closurize_func_references_for_statement(s, func_env);
            }
        }
        Statement::Return(expr) => {
            closurize_func_references_for_expr(expr, func_env);
        }
    }
}

fn closurize_func_references_for_expr(e: &mut Expr, func_env: &TypeEnv) {
    match e {
        Expr::Id(id) | Expr::GlobalSymbol(id) => {
            if func_env.contains_key(id) && !EXTERNED_FUNCTIONS.contains(id) {
                *e = Expr::Closure(id.clone(), vec![]);
            }
        }
        Expr::BinaryOp(l, _, r) => {
            closurize_func_references_for_expr(l, func_env);
            closurize_func_references_for_expr(r, func_env);
        }
        Expr::UnaryOp(_, expr) => closurize_func_references_for_expr(expr, func_env),
        Expr::Call(func, args) => {
            closurize_func_references_for_expr(func, func_env);
            for a in args {
                closurize_func_references_for_expr(a, func_env);
            }
        }
        Expr::Ternary(cond, pos, neg) => {
            closurize_func_references_for_expr(cond, func_env);
            closurize_func_references_for_expr(pos, func_env);
            closurize_func_references_for_expr(neg, func_env);
        }
        Expr::StatementBlock(statements, expr) => {
            for s in statements {
                closurize_func_references_for_statement(s, func_env);
            }
            closurize_func_references_for_expr(expr, func_env);
        }
        Expr::Tuple(exprs) | Expr::Array(exprs) => {
            for e in exprs {
                closurize_func_references_for_expr(e, func_env);
            }
        }
        Expr::Subscript(expr, _) => {
            closurize_func_references_for_expr(expr, func_env);
        }
        Expr::Constant(_) => {}
        Expr::Allocate(_, _) => {}
        Expr::Lambda(_) => {}
        Expr::Closure(..) => panic!("Closures should not exist yet"),
    }
}

#[cfg(test)]
mod tests {
    use crate::utils::{t_global, t_local};
    use indexmap::IndexMap;
    use test_support::compiler::{
        constants::LABEL_MAIN,
        passes::{ASTPass, ClosurizeFunctions},
        syntax_trees::{ast::*, shared::*},
    };

    macro_rules! main_local {
        ($name:expr) => {
            t_local!($name, t_global!(LABEL_MAIN))
        };
    }

    fn make_func_type(param_types: Vec<ValueType>, ret: ValueType) -> ValueType {
        ValueType::FunctionType(param_types, Box::new(ret))
    }

    #[test]
    fn test_captures_param_added_to_all_functions() {
        // Every function in the program gets an ephemeral TupleType([]) param
        // prepended, regardless of whether it actually captures anything.
        let program = Program {
            functions: vec![
                Function {
                    name: t_global!(LABEL_MAIN),
                    body: vec![],
                    types: TypeEnv::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                },
                Function {
                    name: t_global!("helper"),
                    body: vec![],
                    types: TypeEnv::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                },
            ],
            global_types: TypeEnv::new(),
        };

        let result = ClosurizeFunctions.run_pass(program);

        for f in &result.functions {
            assert_eq!(
                f.params.len(),
                1,
                "function {:?} should have exactly one captures param",
                f.name
            );
            let (param_id, _) = f.params.get_index(0).unwrap();
            assert!(
                matches!(param_id, Identifier::Ephemeral(_)),
                "captures param must be Ephemeral"
            );
        }
    }

    #[test]
    fn test_captures_param_inserted_at_front() {
        // When a function already has parameters, the captures-tuple is inserted
        // at index 0 and original params are shifted to index 1+.
        let x_param = t_local!("x", t_global!("helper"));
        let program = Program {
            functions: vec![Function {
                name: t_global!("helper"),
                body: vec![],
                types: TypeEnv::new(),
                params: IndexMap::from_iter([(x_param.clone(), ValueType::IntType)]),
                return_type: ValueType::IntType,
            }],
            global_types: TypeEnv::new(),
        };

        let result = ClosurizeFunctions.run_pass(program);
        let f = &result.functions[0];

        assert_eq!(f.params.len(), 2);
        let (cap_id, _) = f.params.get_index(0).unwrap();
        assert!(matches!(cap_id, Identifier::Ephemeral(_)));
        assert_eq!(*f.params.get_index(1).unwrap().0, x_param);
        assert_eq!(*f.params.get_index(1).unwrap().1, ValueType::IntType);
    }

    #[test]
    fn test_function_id_becomes_closure() {
        // An Expr::Id whose identifier is in global_types (i.e. a named function
        // used as a first-class value) is replaced with Expr::Closure(id, [], 0).
        let foo_name = t_global!("foo");
        let mut global_types = TypeEnv::new();
        global_types.insert(foo_name.clone(), make_func_type(vec![], ValueType::IntType));

        let program = Program {
            functions: vec![
                Function {
                    name: t_global!(LABEL_MAIN),
                    body: vec![Statement::Expr(Expr::Id(foo_name.clone()))],
                    types: TypeEnv::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                },
                Function {
                    name: foo_name.clone(),
                    body: vec![],
                    types: TypeEnv::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                },
            ],
            global_types,
        };

        let result = ClosurizeFunctions.run_pass(program);
        let main_func = result
            .functions
            .iter()
            .find(|f| f.name == t_global!(LABEL_MAIN))
            .unwrap();
        assert_eq!(
            main_func.body[0],
            Statement::Expr(Expr::Closure(foo_name, vec![]))
        );
    }

    #[test]
    fn test_non_function_id_unchanged() {
        // An Expr::Id for a local variable (not in global_types) is left alone.
        let program = Program {
            functions: vec![Function {
                name: t_global!(LABEL_MAIN),
                body: vec![Statement::Expr(Expr::Id(main_local!("x")))],
                types: TypeEnv::new(),
                params: IndexMap::new(),
                return_type: ValueType::IntType,
            }],
            global_types: TypeEnv::new(),
        };

        let result = ClosurizeFunctions.run_pass(program);
        assert_eq!(
            result.functions[0].body[0],
            Statement::Expr(Expr::Id(main_local!("x")))
        );
    }

    #[test]
    fn test_global_symbol_converted() {
        // Expr::GlobalSymbol whose identifier is in global_types is also converted to Closure.
        let foo_name = t_global!("foo");
        let mut global_types = TypeEnv::new();
        global_types.insert(foo_name.clone(), make_func_type(vec![], ValueType::IntType));

        let program = Program {
            functions: vec![Function {
                name: t_global!(LABEL_MAIN),
                body: vec![Statement::Expr(Expr::GlobalSymbol(foo_name.clone()))],
                types: TypeEnv::new(),
                params: IndexMap::new(),
                return_type: ValueType::IntType,
            }],
            global_types,
        };

        let result = ClosurizeFunctions.run_pass(program);
        assert_eq!(
            result.functions[0].body[0],
            Statement::Expr(Expr::Closure(foo_name, vec![]))
        );
    }

    #[test]
    fn test_function_reference_in_call_arg() {
        // A function name passed as a call argument is converted to Closure.
        // The callee itself (GlobalSymbol) is not converted.
        let foo_name = t_global!("foo");
        let mut global_types = TypeEnv::new();
        global_types.insert(foo_name.clone(), make_func_type(vec![], ValueType::IntType));

        let program = Program {
            functions: vec![Function {
                name: t_global!(LABEL_MAIN),
                body: vec![Statement::Expr(Expr::Call(
                    Box::new(Expr::GlobalSymbol(t_global!("apply"))),
                    vec![Expr::Id(foo_name.clone())],
                ))],
                types: TypeEnv::new(),
                params: IndexMap::new(),
                return_type: ValueType::IntType,
            }],
            global_types,
        };

        let result = ClosurizeFunctions.run_pass(program);
        match &result.functions[0].body[0] {
            Statement::Expr(Expr::Call(callee, args)) => {
                // The callee (GlobalSymbol) is unchanged
                assert_eq!(**callee, Expr::GlobalSymbol(t_global!("apply")));
                // The argument (Id referencing a function) is converted
                assert_eq!(args.get(0), Some(&Expr::Closure(foo_name, vec![])));
            }
            other => panic!("Expected Call, got: {other:?}"),
        }
    }

    #[test]
    fn test_function_reference_in_conditional() {
        // A function name inside a conditional branch body is converted.
        let foo_name = t_global!("foo");
        let mut global_types = TypeEnv::new();
        global_types.insert(foo_name.clone(), make_func_type(vec![], ValueType::IntType));

        let program = Program {
            functions: vec![Function {
                name: t_global!(LABEL_MAIN),
                body: vec![Statement::Conditional(
                    Expr::Constant(Value::Bool(true)),
                    vec![Statement::Expr(Expr::Id(foo_name.clone()))],
                    vec![],
                )],
                types: TypeEnv::new(),
                params: IndexMap::new(),
                return_type: ValueType::IntType,
            }],
            global_types,
        };

        let result = ClosurizeFunctions.run_pass(program);
        match &result.functions[0].body[0] {
            Statement::Conditional(_, pos_body, _) => {
                assert_eq!(
                    pos_body.get(0),
                    Some(&Statement::Expr(Expr::Closure(foo_name, vec![])))
                );
            }
            other => panic!("Expected Conditional, got: {other:?}"),
        }
    }

    #[test]
    fn test_function_reference_in_while_loop() {
        // A function name inside a while loop body is converted.
        let foo_name = t_global!("foo");
        let mut global_types = TypeEnv::new();
        global_types.insert(foo_name.clone(), make_func_type(vec![], ValueType::IntType));

        let program = Program {
            functions: vec![Function {
                name: t_global!(LABEL_MAIN),
                body: vec![Statement::WhileLoop(
                    Expr::Constant(Value::Bool(false)),
                    vec![Statement::Expr(Expr::Id(foo_name.clone()))],
                )],
                types: TypeEnv::new(),
                params: IndexMap::new(),
                return_type: ValueType::IntType,
            }],
            global_types,
        };

        let result = ClosurizeFunctions.run_pass(program);
        match &result.functions[0].body[0] {
            Statement::WhileLoop(_, loop_body) => {
                assert_eq!(
                    loop_body.get(0),
                    Some(&Statement::Expr(Expr::Closure(foo_name, vec![])))
                );
            }
            other => panic!("Expected WhileLoop, got: {other:?}"),
        }
    }
}
