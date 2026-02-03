use std::collections::HashMap;

use crate::{ast::{self, ValueType, Identifier}, x86_ast::{Block, Directive, Instr}};
use petgraph::graph::DiGraph;

pub fn x86_block_adj_graph<'a>(blocks: &'a [Block]) -> DiGraph<&'a Block, ()> {
    let mut block_graph = DiGraph::new();

    let label_node_map = {
        let mut map = HashMap::new();
        for b in blocks.iter() {
            if let Directive::Label(label) = &b.label {
                map.insert(label, block_graph.add_node(b));
            } else {
                panic!("block label was not a label");
            }
        }
        map
    };

    for idx in block_graph.node_indices() {
        let this_block = block_graph.node_weight(idx).unwrap();
        for i in this_block.instrs.iter() {
            match i {
                Instr::jmp(dest) | Instr::jmpcc(_, dest) => {
                    if let Some(dest_node) = label_node_map.get(dest) {
                        block_graph.add_edge(idx, *dest_node, ());
                    }
                }
                _ => {}
            }
        }
    }

    block_graph
}


pub fn type_check_ast_expr(e: &ast::Expr, env: &mut ast::TypeEnv) -> ValueType {
    use ast::Expr::*;

    match e {
        BinaryOp(left, op, right) => {
            let l_type = type_check_ast_expr(&*left, env);
            let r_type = type_check_ast_expr(&*right, env);

            let result_type = op.type_of(&l_type, &r_type).unwrap();
            result_type
        }
        UnaryOp(_op, exp) => {
            let exp_type = type_check_ast_expr(&*exp, env);
            assert_eq!(exp_type, ValueType::IntType);
            ValueType::IntType
        }
        Id(id) => env
            .get(&ast::AssignDest::Id(id.clone()))
            .expect(format!("Unknown Identifier: {id:?}").as_str())
            .clone(),
        Constant(v) => ValueType::from(v),
        Call(id, args) => match id {
            Identifier::Named(name) => {
                if name.as_ref() == "read_int" {
                    assert!(
                        args.is_empty(),
                        "Passed {} of args to {name}, expected 0",
                        args.len()
                    );
                    ValueType::IntType
                } else if name.as_ref() == "print_int" {
                    assert_eq!(
                        args.len(),
                        1,
                        "Passed {} args to {name}, expected 1",
                        args.len()
                    );
                    assert_eq!(
                        type_check_ast_expr(&args[0], env),
                        ValueType::IntType,
                        "Passed wrong arg type to {name}, expected I64"
                    );
                    ValueType::NoneType
                } else if name.as_ref() == "len" {
                    assert_eq!(
                        args.len(),
                        1,
                        "Passed {} args to {name}, expected 1",
                        args.len()
                    );
                    assert!(
                        matches!(type_check_ast_expr(&args[0], env), ValueType::TupleType(_)),
                        "Passed wrong arg type to {name}, expected tuple"
                    );
                    ValueType::IntType
                } else {
                    unimplemented!("Unknown function name")
                }
            }
            _ => unimplemented!("Unknown function name"),
        },
        Ternary(cond, pos, neg) => {
            let cond_type = type_check_ast_expr(&*cond, env);
            assert!([ValueType::BoolType, ValueType::IntType].contains(&cond_type));

            let pos_type = type_check_ast_expr(&*pos, env);
            let neg_type = type_check_ast_expr(&*neg, env);
            assert_eq!(
                pos_type, neg_type,
                "Both branches of a ternary must be the same type"
            );

            pos_type
        }
        StatementBlock(statements, expr) => {
            type_check_ast_statements(statements, env);

            type_check_ast_expr(expr, env)
        }
        Tuple(elements) => {
            let element_types: Vec<_> = elements.iter().map(|e| type_check_ast_expr(e, env)).collect();

            ValueType::TupleType(element_types)
        }
        Subscript(tup, idx) => {
            if let ValueType::TupleType(elems) = type_check_ast_expr(tup, env) {
                assert!(*idx >= 0 && *idx < elems.len() as i64, "Indexed tuple out of bounds");
                elems[*idx as usize].clone()
            } else {
                panic!("Subscripted a non-tuple")
            }
        }
        Allocate(_, value_type) => { ValueType::PointerType(Box::new(value_type.clone())) },
        GlobalSymbol(_) => { ValueType::IntType },
    }
}

pub fn type_check_ast_statements(statements: &[ast::Statement], env: &mut ast::TypeEnv) {
    use ast::Statement::*;

    if !statements.is_empty() {
        match &statements[0] {
            Assign(dest, e) => {
                let t = type_check_ast_expr(e, env);
                if env.contains_key(&dest) {
                    assert_eq!(env[&dest], t)
                } else {
                    env.insert(dest.clone(), t);
                }
                type_check_ast_statements(&statements[1..], env);
            }
            Expr(e) => {
                type_check_ast_expr(e, env);
                type_check_ast_statements(&statements[1..], env);
            }
            Conditional(cond, pos, neg) => {
                let cond_type = type_check_ast_expr(cond, env);
                assert!([ValueType::BoolType, ValueType::IntType].contains(&cond_type));

                type_check_ast_statements(pos, env);
                type_check_ast_statements(neg, env);
            }
            WhileLoop(cond, body) => {
                let cond_type = type_check_ast_expr(cond, env);
                assert!([ValueType::BoolType, ValueType::IntType].contains(&cond_type));

                type_check_ast_statements(body, env);
            }
        }
    }
}
