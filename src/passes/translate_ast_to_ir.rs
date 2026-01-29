use std::sync::Arc;

use crate::{
    ast,
    ir::{self, BlockMap, Identifier, Value},
    passes::ASTtoIRPass,
};

pub struct TranslateASTtoIR;

impl ASTtoIRPass for TranslateASTtoIR {
    fn run_pass(self, m: ast::Module) -> ir::IRProgram {
        let mut blocks = BlockMap::new();

        let mut main_body = ir::Block {
            statements: vec![ir::Statement::Return(ir::Atom::Constant(Value::I64(0)))],
        };

        let ast::Module::Body(ast_statements) = m;
        for s in ast_statements.iter().rev() {
            main_body.statements = generate_for_statement(s, main_body.statements, &mut blocks);
        }

        blocks.insert(Identifier::Named(Arc::from("entry")), main_body);

        ir::IRProgram { blocks }
    }
}

fn generate_for_statement(
    s: &ast::Statement,
    cont: Vec<ir::Statement>,
    blocks: &mut BlockMap,
) -> Vec<ir::Statement> {
    match s {
        ast::Statement::Assign(dest_id, expr) => {
            generate_for_assign(expr, dest_id.clone(), cont, blocks)
        }
        ast::Statement::Expr(expr) => generate_for_effect(expr, cont, blocks),
        ast::Statement::Conditional(cond, pos, neg) => {
            let cont_label = new_block(cont, blocks);

            let mut pos_ir = vec![ir::Statement::Goto(cont_label.clone())];
            for i in pos.iter().rev() {
                pos_ir = generate_for_statement(i, pos_ir, blocks);
            }

            let mut neg_ir = vec![ir::Statement::Goto(cont_label)];
            for i in neg.iter().rev() {
                neg_ir = generate_for_statement(i, neg_ir, blocks);
            }

            let pos_label = new_block(pos_ir, blocks);
            let neg_label = new_block(neg_ir, blocks);
            generate_for_predicate(cond, pos_label, neg_label, blocks)
        }
        ast::Statement::WhileLoop(cond, body) => {
            let cont_label = new_block(cont, blocks);

            let cond_label = new_block(vec![], blocks);

            let mut body_ir = vec![ir::Statement::Goto(cond_label.clone())];
            for i in body.iter().rev() {
                body_ir = generate_for_statement(i, body_ir, blocks);
            }

            let body_label = new_block(body_ir, blocks);

            let cond_ir = generate_for_predicate(cond, body_label.clone(), cont_label, blocks);
            let cond_block = blocks.get_mut(&cond_label).unwrap();
            cond_block.statements = cond_ir;

            vec![ir::Statement::Goto(cond_label)]
        }
    }
}

fn generate_for_effect(
    e: &ast::Expr,
    cont: Vec<ir::Statement>,
    blocks: &mut BlockMap,
) -> Vec<ir::Statement> {
    // Keep only the side effects of an expr statement, the result
    // doesn't matter
    match e {
        ast::Expr::Call(identifier, exprs) => {
            let mut ret = vec![ir::Statement::Expr(ir::Expr::Call(
                identifier.clone(),
                exprs.iter().map(expr_to_atom).collect(),
            ))];

            ret.extend(cont);
            ret
        }
        ast::Expr::Ternary(cond, pos, neg) => {
            let cont_label = new_block(cont, blocks);
            let pos_ir =
                generate_for_effect(pos, vec![ir::Statement::Goto(cont_label.clone())], blocks);
            let neg_ir = generate_for_effect(neg, vec![ir::Statement::Goto(cont_label)], blocks);

            let pos_label = new_block(pos_ir, blocks);
            let neg_label = new_block(neg_ir, blocks);
            generate_for_predicate(cond, pos_label, neg_label, blocks)
        }

        ast::Expr::StatementBlock(statements, expr) => {
            let mut ret = generate_for_effect(expr, cont, blocks);
            for s in statements.iter().rev() {
                ret = generate_for_statement(s, ret, blocks);
            }

            ret
        }

        ast::Expr::Constant(_)
        | ast::Expr::BinaryOp(_, _, _)
        | ast::Expr::UnaryOp(_, _)
        | ast::Expr::Id(_) => {
            // No side effects, disregard this expression
            cont
        }
    }
}

fn generate_for_assign(
    e: &ast::Expr,
    dest_id: Identifier,
    cont: Vec<ir::Statement>,
    blocks: &mut BlockMap,
) -> Vec<ir::Statement> {
    match e {
        ast::Expr::Constant(value) => {
            let mut ret = vec![ir::Statement::Assign(
                dest_id,
                ir::Expr::Atom(ir::Atom::Constant(*value)),
            )];
            ret.extend(cont);
            ret
        }
        ast::Expr::BinaryOp(left, op, right) => {
            let l_atom = expr_to_atom(&*left);
            let r_atom = expr_to_atom(&*right);

            let mut ret = vec![ir::Statement::Assign(
                dest_id,
                ir::Expr::BinaryOp(l_atom, *op, r_atom),
            )];
            ret.extend(cont);
            ret
        }
        ast::Expr::UnaryOp(op, expr) => {
            let atom = expr_to_atom(&*expr);

            let mut ret = vec![ir::Statement::Assign(dest_id, ir::Expr::UnaryOp(*op, atom))];
            ret.extend(cont);
            ret
        }
        ast::Expr::Call(func, exprs) => {
            let args = exprs.iter().map(expr_to_atom).collect();

            let mut ret = vec![ir::Statement::Assign(
                dest_id,
                ir::Expr::Call(func.clone(), args),
            )];
            ret.extend(cont);
            ret
        }
        ast::Expr::Id(src_id) => {
            let mut ret = vec![ir::Statement::Assign(
                dest_id,
                ir::Expr::Atom(ir::Atom::Variable(src_id.clone())),
            )];
            ret.extend(cont);
            ret
        }
        ast::Expr::Ternary(cond, pos, neg) => {
            let cont_label = new_block(cont, blocks);

            let pos_ir = generate_for_assign(
                pos,
                dest_id.clone(),
                vec![ir::Statement::Goto(cont_label.clone())],
                blocks,
            );
            let neg_ir =
                generate_for_assign(neg, dest_id, vec![ir::Statement::Goto(cont_label)], blocks);

            let pos_label = new_block(pos_ir, blocks);
            let neg_label = new_block(neg_ir, blocks);
            generate_for_predicate(&*cond, pos_label, neg_label, blocks)
        }
        ast::Expr::StatementBlock(statements, expr) => {
            let mut ret = generate_for_assign(expr, dest_id, cont, blocks);
            for s in statements.iter().rev() {
                ret = generate_for_statement(s, ret, blocks);
            }
            ret
        }
    }
}

fn generate_for_predicate(
    cond: &ast::Expr,
    pos_label: Identifier,
    neg_label: Identifier,
    blocks: &mut BlockMap,
) -> Vec<ir::Statement> {
    match cond {
        ast::Expr::BinaryOp(left, op, right) => {
            // TODO: This should be type-checked?

            let l_atom = expr_to_atom(&*left);
            let r_atom = expr_to_atom(&*right);

            vec![ir::Statement::If(
                ir::Expr::BinaryOp(l_atom, *op, r_atom),
                pos_label,
                neg_label,
            )]
        }
        ast::Expr::Constant(val) => {
            if (*val).into() {
                blocks[&pos_label].statements.clone()
            } else {
                blocks[&neg_label].statements.clone()
            }
        }
        ast::Expr::UnaryOp(op, val) => {
            // TODO: This should be type-checked?
            vec![ir::Statement::If(
                ir::Expr::UnaryOp(*op, expr_to_atom(&*val)),
                neg_label,
                pos_label,
            )]
        }
        ast::Expr::Ternary(sub_cond, sub_pos, sub_neg) => {
            let sub_pos_ir =
                generate_for_predicate(sub_pos, pos_label.clone(), neg_label.clone(), blocks);
            let sub_neg_ir = generate_for_predicate(sub_neg, pos_label, neg_label, blocks);

            let sub_pos_label = new_block(sub_pos_ir, blocks);
            let sub_neg_label = new_block(sub_neg_ir, blocks);
            generate_for_predicate(sub_cond, sub_pos_label, sub_neg_label, blocks)
        }
        ast::Expr::StatementBlock(statements, expr) => {
            let mut ret = vec![];
            for s in statements.iter().rev() {
                ret = generate_for_statement(s, ret, blocks);
            }

            ret.extend(generate_for_predicate(expr, pos_label, neg_label, blocks));

            ret
        }
        ast::Expr::Call(func_name, args) => {
            vec![ir::Statement::If(
                ir::Expr::Call(func_name.clone(), args.iter().map(expr_to_atom).collect()),
                pos_label,
                neg_label,
            )]
        }
        ast::Expr::Id(identifier) => {
            vec![ir::Statement::If(
                ir::Expr::Atom(ir::Atom::Variable(identifier.clone())),
                pos_label,
                neg_label,
            )]
        }
    }
}

fn expr_to_atom(e: &ast::Expr) -> ir::Atom {
    match e {
        ast::Expr::Constant(value) => ir::Atom::Constant(*value),
        ast::Expr::Id(id) => ir::Atom::Variable(id.clone()),
        _ => panic!("Expr `{e:?}` cannot be converted to atom"),
    }
}

fn new_block(statements: Vec<ir::Statement>, blocks: &mut BlockMap) -> Identifier {
    if statements.len() == 1
        && let ir::Statement::Goto(label) = &statements[0]
    {
        return label.clone();
    }

    let label = Identifier::new_ephemeral();
    blocks.insert(label.clone(), ir::Block { statements });
    label
}
