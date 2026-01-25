use crate::infra::TypeEnv;
use cs4999_compiler::ast::*;

fn type_check_expr(e: &Expr, env: &mut TypeEnv) -> ValueType {
    use Expr::*;

    match e {
        BinaryOp(left, _op, right) => {
            let l_type = type_check_expr(&*left, env);
            assert_eq!(l_type, ValueType::IntType);
            let r_type = type_check_expr(&*right, env);
            assert_eq!(r_type, ValueType::IntType);
            ValueType::IntType
        }
        UnaryOp(_op, exp) => {
            let exp_type = type_check_expr(&*exp, env);
            assert_eq!(exp_type, ValueType::IntType);
            ValueType::IntType
        }
        Id(id) => *env
            .get(id)
            .expect(format!("Unknown Identifier: {id:?}").as_str()),
        Constant(v) => match v {
            Value::I64(_) => ValueType::IntType,
        },
        Call(id, _args) => {
            // TODO: Lookup function name to check arg types
            match id {
                Identifier::Named(name) => {
                    if name.as_ref() == "read_int" {
                        ValueType::IntType
                    } else if name.as_ref() == "print_int" {
                        ValueType::None
                    } else {
                        unimplemented!("Unknown function name")
                    }
                }
                _ => unimplemented!("Unknown function name"),
            }
        }
    }
}

fn type_check_statements(statements: &[Statement], env: &mut TypeEnv) {
    use Statement::*;

    if !statements.is_empty() {
        match &statements[0] {
            Assign(dest, e) => {
                let t = type_check_expr(e, env);
                if env.contains_key(&dest) {
                    assert_eq!(env[&dest], t)
                } else {
                    env.insert(dest.clone(), t);
                }
                type_check_statements(&statements[1..], env);
            }
            Expr(e) => {
                type_check_expr(e, env);
                type_check_statements(&statements[1..], env);
            }
        }
    }
}

pub fn type_check(m: &Module) {
    let Module::Body(statements) = m;

    let mut env = TypeEnv::new();
    type_check_statements(&statements[..], &mut env);
}
