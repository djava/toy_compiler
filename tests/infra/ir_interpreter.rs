use std::collections::VecDeque;

use cs4999_compiler::{
    ast::{Identifier, Value},
    ir::*,
};

use crate::infra::{ValueEnv, interpreter_utils::*};

#[derive(Debug, Clone)]
enum Continuation {
    Next,
    Jump(Identifier),
    Return(Value),
    Exit,
}

fn interpret_atom(atom: &Atom, env: &mut ValueEnv) -> Value {
    match atom {
        Atom::Constant(value) => value.clone(),
        Atom::Variable(id) => env[id].clone(),
    }
}

fn interpret_expr(
    expr: &Expr,
    inputs: &mut VecDeque<i64>,
    outputs: &mut VecDeque<i64>,
    env: &mut ValueEnv,
) -> Value {
    match expr {
        Expr::Atom(atom) => interpret_atom(atom, env).clone(),
        Expr::UnaryOp(op, atom) => {
            let val = interpret_atom(&atom, env);
            op.try_eval(&val).unwrap()
        }
        Expr::BinaryOp(l_atom, op, r_atom) => {
            let l_val = interpret_atom(&l_atom, env);
            let r_val = interpret_atom(&r_atom, env);

            op.try_eval(&l_val, &r_val).unwrap()
        }
        Expr::Call(func_name, args) => {
            if func_name == &Identifier::from("print_int") {
                if args.len() != 1 {
                    panic!("Wrong number of arguments to print_int()");
                }

                if let Value::I64(val) = interpret_atom(&args[0], env) {
                    outputs.push_back(val.clone());
                } else {
                    panic!("Wrong argument type to print_int()");
                }

                Value::None
            } else if func_name == &Identifier::from("read_int") {
                if args.len() != 0 {
                    panic!("Wrong number of args to read_int()");
                }

                if let Some(val) = inputs.pop_front() {
                    Value::I64(val)
                } else {
                    panic!("Overflowed inputs");
                }
            } else {
                panic!("Unknown function name")
            }
        }
    }
}

fn interpret_statement(
    statement: &Statement,
    inputs: &mut VecDeque<i64>,
    outputs: &mut VecDeque<i64>,
    env: &mut ValueEnv,
) -> Continuation {
    match statement {
        Statement::Expr(expr) => {
            interpret_expr(expr, inputs, outputs, env);
            Continuation::Next
        }
        Statement::Assign(dest_id, expr) => {
            let value = interpret_expr(expr, inputs, outputs, env);
            env.insert(dest_id.clone(), value);
            Continuation::Next
        }
        Statement::Return(expr) => Continuation::Return(interpret_atom(expr, env)),
        Statement::Goto(label) => Continuation::Jump(label.clone()),
        Statement::If(cond, pos_label, neg_label) => {
            let result = interpret_expr(cond, inputs, outputs, env).expect_bool();
            if result {
                Continuation::Jump(pos_label.clone())
            } else {
                Continuation::Jump(neg_label.clone())
            }
        }
    }
}

fn interpret_block(
    block: &Block,
    inputs: &mut VecDeque<i64>,
    outputs: &mut VecDeque<i64>,
    env: &mut ValueEnv,
) -> Continuation {
    for s in &block.statements {
        match interpret_statement(s, inputs, outputs, env) {
            Continuation::Next => continue,
            c @ Continuation::Jump(_) => return c.clone(),
            c @ Continuation::Return(_) => return c.clone(),
            c @ Continuation::Exit => return c.clone(),
        }
    }
    Continuation::Exit
}

pub fn interpret_irprogram(p: &IRProgram, inputs: &mut VecDeque<i64>, outputs: &mut VecDeque<i64>) {
    let mut env = ValueEnv::new();
    dbg!(p);

    let mut block_idx = p
        .blocks
        .get_index_of(&Identifier::from("user_entry"))
        .unwrap();
    loop {
        println!("===Executing: {:?}", p.blocks.get_index(block_idx));
        match p
            .blocks
            .get_index(block_idx)
            .map(|(_label, b)| interpret_block(b, inputs, outputs, &mut env))
        {
            Some(Continuation::Jump(label)) => {
                block_idx = p.blocks.get_index_of(&label).unwrap();
            }
            Some(Continuation::Return(_val)) => {
                // TODO: Call stack? Or other way to implement this
                break;
            }
            Some(Continuation::Exit) | _ => {
                break;
            }
        }
    }
}
