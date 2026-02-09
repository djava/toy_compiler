use std::collections::VecDeque;

use compiler::{
    constants::LABEL_MAIN,
    syntax_trees::{ir::*, shared::*},
};

use crate::{ValueEnv, interpreter_utils::*};

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
        Atom::Variable(id) => env[&AssignDest::Id(id.clone())].clone(),
        Atom::GlobalSymbol(_) => Value::I64(0), // I'm not sure about this but it's what the textbook code does
    }
}

fn subscript_tuple(tup: &Value, idx: i64) -> Value {
    if let Value::Tuple(elems) = tup {
        elems[idx as usize].clone()
    } else {
        panic!("Non-tuple value passed to subscript_tuple")
    }
}

fn interpret_expr(
    expr: &Expr,
    inputs: &mut VecDeque<i64>,
    outputs: &mut VecDeque<i64>,
    val_env: &mut ValueEnv,
    func_env: &Vec<Function>,
) -> Value {
    match expr {
        Expr::Atom(atom) => interpret_atom(atom, val_env).clone(),
        Expr::UnaryOp(op, atom) => {
            let val = interpret_atom(&atom, val_env);
            op.try_eval(&val).unwrap()
        }
        Expr::BinaryOp(l_atom, op, r_atom) => {
            let l_val = interpret_atom(&l_atom, val_env);
            let r_val = interpret_atom(&r_atom, val_env);

            op.try_eval(&l_val, &r_val).unwrap()
        }
        Expr::Call(func_name, args) => {
            if func_name == &Atom::Variable(id!("print_int")) {
                if args.len() != 1 {
                    panic!("Wrong number of arguments to print_int()");
                }

                if let Value::I64(val) = interpret_atom(&args[0], val_env) {
                    outputs.push_back(val.clone());
                } else {
                    panic!("Wrong argument type to print_int()");
                }

                Value::None
            } else if func_name == &Atom::Variable(id!("read_int")) {
                if args.len() != 0 {
                    panic!("Wrong number of args to read_int()");
                }

                if let Some(val) = inputs.pop_front() {
                    Value::I64(val)
                } else {
                    panic!("Overflowed inputs");
                }
            } else {
                if let Some(func) = func_env.iter().find(|f| Atom::Variable(f.name.clone()) == *func_name) {
                    let arg_vals = args.iter().map(|a| interpret_atom(a, val_env)).collect();
                    return interpret_func(func, arg_vals, inputs, outputs, func_env)
                } else {
                    panic!();
                }
            }
        }
        Expr::Allocate(n, _) => Value::Tuple(vec![Value::None; *n]),
        Expr::Subscript(atom, idx) => {
            if let Value::Tuple(elems) = interpret_atom(atom, val_env) {
                elems[*idx as usize].clone()
            } else {
                panic!("Subscripted non-tuple")
            }
        }
    }
}

fn interpret_statement(
    statement: &Statement,
    inputs: &mut VecDeque<i64>,
    outputs: &mut VecDeque<i64>,
    val_env: &mut ValueEnv,
    func_env: &Vec<Function>,
) -> Continuation {
    match statement {
        Statement::Expr(expr) => {
            interpret_expr(expr, inputs, outputs, val_env, func_env);
            Continuation::Next
        }
        Statement::Assign(dest_id, expr) => {
            let value = interpret_expr(expr, inputs, outputs, val_env, func_env);
            val_env.insert(dest_id.clone(), value);
            Continuation::Next
        }
        Statement::Return(expr) => Continuation::Return(interpret_atom(expr, val_env)),
        Statement::Goto(label) => Continuation::Jump(label.clone()),
        Statement::If(cond, pos_label, neg_label) => {
            let result = interpret_expr(cond, inputs, outputs, val_env, func_env).expect_bool();
            if result {
                Continuation::Jump(pos_label.clone())
            } else {
                Continuation::Jump(neg_label.clone())
            }
        }
        Statement::TailCall(name, args) => {
            let ret_val = interpret_expr(
                &Expr::Call(Atom::Variable(name.clone()), args.clone()),
                inputs,
                outputs,
                val_env,
                func_env
            );
            Continuation::Return(ret_val)
        }
    }
}

fn interpret_block(
    block: &Block,
    inputs: &mut VecDeque<i64>,
    outputs: &mut VecDeque<i64>,
    val_env: &mut ValueEnv,
    func_env: &Vec<Function>,
) -> Continuation {
    for s in &block.statements {
        let c = interpret_statement(s, inputs, outputs, val_env, func_env);
        match c {
            Continuation::Next => continue,
            _ => {
                return c;
            }
        }
    }
    Continuation::Exit
}

pub fn interpret_func(
    f: &Function,
    args: Vec<Value>,
    inputs: &mut VecDeque<i64>,
    outputs: &mut VecDeque<i64>,
    func_env: &Vec<Function>,
) -> Value {
    let mut val_env = ValueEnv::new();

    assert!(
        f.params.len() == args.len(),
        "Passed wrong number of args to function: `{:?}` (got {}, expected {})",
        f.name,
        args.len(),
        f.params.len()
    );

    for ((id, typ), val) in f.params.iter().zip(args) {
        assert!(
            ValueType::from(&val) == *typ,
            "Passed wrong type of value to `{:?}` (Arg {:?} got {:?}, expected {:?})",
            f.name,
            id,
            ValueType::from(&val),
            typ
        );

        val_env.insert(AssignDest::Id(id.clone()), val);
    }

    let mut block_idx = f
        .blocks
        .get_index_of(&f.entry_block)
        .expect("Couldn't find entry block");

    loop {
        match f
            .blocks
            .get_index(block_idx)
            .map(|(_label, b)| interpret_block(b, inputs, outputs, &mut val_env, func_env))
        {
            Some(Continuation::Jump(label)) => {
                if let Some(idx) = f.blocks.get_index_of(&label) {
                    block_idx = idx;
                } else {
                    panic!("Didn't find jump target: {label:?}");
                }
            }
            Some(Continuation::Return(val)) => {
                return val;
            }
            Some(Continuation::Exit) | _ => {
                break;
            }
        }
    }
    Value::None
}

pub fn interpret_irprogram(p: &IRProgram, inputs: &mut VecDeque<i64>, outputs: &mut VecDeque<i64>) {
    let main_func = p
        .functions
        .iter()
        .find(|f| f.name == id!(LABEL_MAIN))
        .unwrap();

    interpret_func(main_func, vec![], inputs, outputs, &p.functions);
}
