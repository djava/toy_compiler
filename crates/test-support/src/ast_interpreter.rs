use crate::{ValueEnv, interpreter_utils::*};
use compiler::{
    constants::LABEL_MAIN,
    syntax_trees::{ast::*, shared::*},
};
use std::collections::VecDeque;

fn interpret_expr(
    e: &Expr,
    inputs: &mut VecDeque<i64>,
    outputs: &mut VecDeque<i64>,
    val_env: &mut ValueEnv,
    func_env: &Vec<Function>,
) -> Option<Value> {
    use Expr::*;

    match e {
        BinaryOp(left, op, right) => {
            if let Some(l_val) = interpret_expr(&*left, inputs, outputs, val_env, func_env)
                && let Some(r_val) = interpret_expr(&*right, inputs, outputs, val_env, func_env)
            {
                op.try_eval(&l_val, &r_val)
            } else {
                None
            }
        }
        UnaryOp(op, expr) => {
            if let Some(v) = interpret_expr(&*expr, inputs, outputs, val_env, func_env) {
                op.try_eval(&v)
            } else {
                None
            }
        }
        Constant(v) => Some(v.clone()),
        Call(func, args) => {
            if **func == GlobalSymbol(id!("read_int")) && args.is_empty() {
                Some(Value::I64(inputs.pop_front().expect("Ran out of inputs")))
            } else if **func == GlobalSymbol(id!("print_int")) && args.len() == 1 {
                let val = interpret_expr(&args[0], inputs, outputs, val_env, func_env).expect_int();
                outputs.push_back(val);

                Some(Value::None)
            } else {
                let func_val = interpret_expr(&**func, inputs, outputs, val_env, func_env);
                if let Some(Value::Function(name, _, _)) = func_val
                    && let Some(func) = func_env.iter().find(|f| f.name == name)
                {
                    let mut arg_vals = vec![];
                    for a in args {
                        let val = interpret_expr(a, inputs, outputs, val_env, func_env);
                        if val.is_none() {
                            panic!("Function argument did not evaluate to a value: `{e:?}`");
                        }

                        arg_vals.push(val.unwrap());
                    }

                    let mut func_val_env = ValueEnv::from_iter(
                        func.params
                            .iter()
                            .zip(arg_vals)
                            .map(|((id, _typ), arg)| (id.clone(), arg)),
                    );

                    interpret_statement_chain(
                        &mut func.body.iter(),
                        inputs,
                        outputs,
                        &mut func_val_env,
                        func_env,
                    )
                } else {
                    panic!()
                }
            }
        }
        Id(id) | GlobalSymbol(id) => {
            let val = val_env
                .get(id)
                .expect(format!("Unknown variable name: {id:?}").as_str())
                .clone();
            Some(val)
        }
        Ternary(cond, pos, neg) => {
            if let Some(Value::Bool(cond_val)) =
                interpret_expr(&*cond, inputs, outputs, val_env, func_env)
            {
                if cond_val {
                    interpret_expr(pos, inputs, outputs, val_env, func_env)
                } else {
                    interpret_expr(neg, inputs, outputs, val_env, func_env)
                }
            } else {
                None
            }
        }
        StatementBlock(statements, expr) => {
            if !statements.is_empty() {
                interpret_statement_chain(
                    &mut statements.iter(),
                    inputs,
                    outputs,
                    val_env,
                    func_env,
                );
            }

            interpret_expr(expr, inputs, outputs, val_env, func_env)
        }
        Tuple(_exprs) => todo!(),
        Subscript(_expr, _value) => todo!(),
        Allocate(_, _value_type) => todo!(),
    }
}

fn interpret_statement_chain<'a>(
    statements: &mut dyn Iterator<Item = &'a Statement>,
    inputs: &mut VecDeque<i64>,
    outputs: &mut VecDeque<i64>,
    val_env: &mut ValueEnv,
    func_env: &Vec<Function>,
) -> Option<Value> {
    let s = statements.next();
    if s.is_none() {
        return None;
    }

    match s.unwrap() {
        Statement::Expr(e) => {
            interpret_expr(e, inputs, outputs, val_env, func_env);
            return interpret_statement_chain(statements, inputs, outputs, val_env, func_env);
        }
        Statement::Assign(dest, e) => {
            let result = interpret_expr(e, inputs, outputs, val_env, func_env).unwrap();
            match dest {
                AssignDest::Id(id) => {
                    val_env.insert(id.clone(), result);
                }
                AssignDest::Subscript(id, index) => {
                    if let Some(Value::Tuple(elems)) = val_env.get_mut(id) {
                        elems[*index as usize] = result;
                    } else {
                        panic!("Assigned to invalid subscript assigndest: `{dest:?}`")
                    }
                }
            }

            return interpret_statement_chain(statements, inputs, outputs, val_env, func_env);
        }
        Statement::Conditional(cond, pos, neg) => {
            let result = interpret_expr(&cond, inputs, outputs, val_env, func_env).coerce_bool();

            let exec_next = if result { pos } else { neg };

            return interpret_statement_chain(
                &mut exec_next.into_iter().chain(statements),
                inputs,
                outputs,
                val_env,
                func_env,
            );
        }
        Statement::WhileLoop(cond, body) => {
            let mut iterations = 0;
            while interpret_expr(&cond, inputs, outputs, val_env, func_env).expect_bool() {
                if let Some(ret_val) =
                    interpret_statement_chain(&mut body.iter(), inputs, outputs, val_env, func_env)
                {
                    return Some(ret_val);
                }
                iterations += 1;
                if iterations > 1000 {
                    panic!("infinite loop? iterated 1000 times");
                }
            }

            return interpret_statement_chain(statements, inputs, outputs, val_env, func_env);
        }

        Statement::Return(e) => {
            return interpret_expr(&e, inputs, outputs, val_env, func_env);
        }
    };
}

pub fn interpret(m: &Program, inputs: &mut VecDeque<i64>, outputs: &mut VecDeque<i64>) {
    let main_function = m
        .functions
        .iter()
        .find(|f| f.name == id!(LABEL_MAIN))
        .expect("Couldn't find main function");

    if !main_function.body.is_empty() {
        let mut val_env = ValueEnv::new();

        // Add all the functions into val_env
        for f in &m.functions {
            val_env.insert(
                f.name.clone(),
                Value::Function(
                    f.name.clone(),
                    f.params.values().cloned().collect(),
                    f.return_type.clone(),
                ),
            );
        }

        interpret_statement_chain(
            &mut main_function.body.iter(),
            inputs,
            outputs,
            &mut val_env,
            &m.functions,
        );
    }
}
