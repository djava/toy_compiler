use crate::{ast::*, passes::ASTPass};

pub struct PartialEval;

impl ASTPass for PartialEval {
    fn run_pass(self, mut m: Module) -> Module {
        let mut new_body = Vec::new();
        for s in m.body {
            partial_eval_statement(s, &mut new_body);
        }

        m.body = new_body;
        m
    }
}

fn partial_eval_statement(s: Statement, new_statements: &mut Vec<Statement>) {
    match s {
        Statement::Assign(dest, mut e) => {
            partial_eval_expr(&mut e);
            new_statements.push(Statement::Assign(dest, e));
        }
        Statement::Expr(mut e) => {
            partial_eval_expr(&mut e);
            new_statements.push(Statement::Expr(e));
        }
        Statement::Conditional(mut cond, pos, neg) => {
            partial_eval_expr(&mut cond);

            if let Expr::Constant(val) = cond {
                if val.into() {
                    let mut pos_statements = Vec::new();
                    pos.into_iter()
                        .for_each(|s| partial_eval_statement(s, &mut pos_statements));
                    new_statements.extend(pos_statements);
                } else {
                    let mut neg_statements = Vec::new();
                    neg.into_iter()
                        .for_each(|s| partial_eval_statement(s, &mut neg_statements));
                    new_statements.extend(neg_statements);
                }
            } else {
                let mut pos_pe = Vec::new();
                pos.into_iter()
                    .for_each(|s| partial_eval_statement(s, &mut pos_pe));

                let mut neg_pe = Vec::new();
                neg.into_iter()
                    .for_each(|s| partial_eval_statement(s, &mut neg_pe));

                new_statements.push(Statement::Conditional(cond, pos_pe, neg_pe));
            }
        }
        Statement::WhileLoop(mut cond, body) => {
            partial_eval_expr(&mut cond);
            let mut body_pe = vec![];
            body.into_iter()
                .for_each(|s| partial_eval_statement(s, &mut body_pe));

            if let Expr::Constant(val) = cond {
                if val.into() {
                    println!(
                        "Contains an infinite loop - careful because I don't know if they'll respond to ctrl-c :)"
                    );
                } else {
                    // Cond is always false - don't even generate the loop
                }
            } else {
                new_statements.push(Statement::WhileLoop(cond, body_pe));
            }
        }
        Statement::AssignSubscript(identifier, idx, mut expr) => {
            partial_eval_expr(&mut expr);

            new_statements.push(Statement::AssignSubscript(identifier, idx, expr));
        },
    }
}

fn partial_eval_expr(e: &mut Expr) {
    use Expr::*;

    match e {
        BinaryOp(left, op, right) => {
            // Try and evaluate both operands recursively, then if
            // they're both constants we can evaluate the whole
            // expression
            partial_eval_expr(&mut *left);
            partial_eval_expr(&mut *right);

            if let Constant(l_val) = &**left
                && let Constant(r_val) = &**right
            {
                *e = Constant(op.eval(l_val, r_val))
            }
        }
        UnaryOp(op, expr) => {
            // Try and evaluate the operand recursively, then if its
            // constant we can evaluate the whole expression
            partial_eval_expr(&mut *expr);
            if let Constant(val) = &**expr {
                *e = Constant(op.eval(&val));
            }
        }
        Call(_name, args) => {
            // Can't evaluate through function calls right now, but try
            // to evaluate the arguments regardless
            for i in args {
                partial_eval_expr(i);
            }
        }
        StatementBlock(statements, expr) => {
            let mut new_statements = Vec::new();
            statements
                .iter()
                .for_each(|s| partial_eval_statement(s.clone(), &mut new_statements));
            *statements = new_statements;
            partial_eval_expr(expr);
        }
        Constant(_val) => {
            // Already a constant, nothing to evaluate
        }
        Id(_name) => {
            // Can't do anything
        }
        Ternary(cond, pos, neg) => {
            partial_eval_expr(&mut *cond);
            partial_eval_expr(&mut *pos);
            partial_eval_expr(&mut *neg);

            if let Constant(val) = cond.as_ref() {
                if val.into() {
                    *e = (**pos).clone();
                } else {
                    *e = (**neg).clone();
                }
            }
        }
        Tuple(elems) => elems.iter_mut().for_each(partial_eval_expr),
        Subscript(tup, idx) => {
            partial_eval_expr(tup.as_mut());
            if let Tuple(elems) = &**tup {
                if let Constant(elem_val) = &elems[*idx as usize] {
                    *e = Constant(elem_val.clone());
                }
            } else {
                panic!("Subscript had wrong argument types");
            }
        },
        GlobalSymbol(_) => {}
        Allocate(_, _) => panic!("This pass should've happened before any Allocate calls are injected")
    }
}

trait BinaryOperatorExt {
    fn eval(&self, l: &Value, r: &Value) -> Value;
}

impl BinaryOperatorExt for BinaryOperator {
    fn eval(&self, l: &Value, r: &Value) -> Value {
        use ValueType::*;

        match (ValueType::from(l), ValueType::from(r)) {
            (IntType, IntType) => {
                if let Value::I64(l_val) = l
                    && let Value::I64(r_val) = r
                {
                    match self {
                        BinaryOperator::Add => Value::I64(l_val + r_val),
                        BinaryOperator::Subtract => Value::I64(l_val - r_val),
                        BinaryOperator::Equals => Value::Bool(l_val == r_val),
                        BinaryOperator::NotEquals => Value::Bool(l_val != r_val),
                        BinaryOperator::Greater => Value::Bool(l_val > r_val),
                        BinaryOperator::GreaterEquals => Value::Bool(l_val >= r_val),
                        BinaryOperator::Less => Value::Bool(l_val < r_val),
                        BinaryOperator::LessEquals => Value::Bool(l_val <= r_val),
                        _ => panic!(
                            "Unsupported operand types (int, int) to BinaryOperator {self:?}"
                        ),
                    }
                } else {
                    panic!("ValueType didnt match Value variant - bug in ValueType::from()?");
                }
            }
            (BoolType, BoolType) => {
                if let Value::Bool(l_val) = l
                    && let Value::Bool(r_val) = r
                {
                    match self {
                        BinaryOperator::And => Value::Bool(*l_val && *r_val),
                        BinaryOperator::Or => Value::Bool(*l_val || *r_val),
                        BinaryOperator::Equals => Value::Bool(*l_val == *r_val),
                        BinaryOperator::NotEquals => Value::Bool(*l_val != *r_val),
                        _ => panic!(
                            "Unsupported operand types (bool, bool) to BinaryOperator {self:?}"
                        ),
                    }
                } else {
                    panic!("ValueType didnt match Value variant - bug in ValueType::from()?");
                }
            }
            _ => panic!(
                "Unsupported operand type ({:?}, {:?}) to UnaryOperator {self:?}",
                ValueType::from(l),
                ValueType::from(r)
            ),
        }
    }
}
trait UnaryOperatorExt {
    fn eval(&self, v: &Value) -> Value;
}

impl UnaryOperatorExt for UnaryOperator {
    fn eval(&self, v: &Value) -> Value {
        use ValueType::*;

        match ValueType::from(v) {
            IntType => {
                if let Value::I64(val) = v {
                    match self {
                        UnaryOperator::Minus => Value::I64(-val),
                        UnaryOperator::Plus => Value::I64(*val),
                        _ => panic!("Unsupported operand type (int) to UnaryOperator {self:?}"),
                    }
                } else {
                    panic!("ValueType didnt match Value variant - bug in ValueType::from()?");
                }
            }
            BoolType => {
                if let Value::Bool(val) = v {
                    match self {
                        UnaryOperator::Not => Value::Bool(!val),
                        _ => panic!("Unsupported operand type bool to UnaryOperator {self:?}"),
                    }
                } else {
                    panic!("ValueType didnt match Value variant - bug in ValueType::from()?");
                }
            }
            _ => panic!(
                "Unsupported operand type ({:?}) to UnaryOperator {self:?}",
                ValueType::from(v)
            ),
        }
    }
}
