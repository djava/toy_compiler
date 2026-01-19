use crate::{ast::*, passes::IRToX86Pass, x86_ast::*};

pub struct SelectInstructions;

impl IRToX86Pass for SelectInstructions {
    fn run_pass(m: Module) -> X86Program {
        let Module::Body(statements) = m;

        let mut program_instrs = vec![];
        for s in statements {
            program_instrs.extend(sel_for_statement(s));
        }

        X86Program {
            functions: vec![(Directive::Label(String::from("main")), program_instrs)],
            stack_size: 0,
        }
    }
}

fn sel_for_statement(s: Statement) -> Vec<Instr> {
    match s {
        Statement::Assign(dest_id, e) => match e {
            Expr::BinaryOp(l, BinaryOperator::Add, r) => sel_for_add(dest_id, l, r),
            Expr::BinaryOp(l, BinaryOperator::Subtract, r) => sel_for_sub(dest_id, l, r),
            Expr::UnaryOp(UnaryOperator::Plus, val) => sel_for_unary_plus(dest_id, val),
            Expr::UnaryOp(UnaryOperator::Minus, val) => sel_for_unary_minus(dest_id, val),
            Expr::Constant(Value::I64(val)) => vec![Instr::movq(
                Arg::Immediate(val as _),
                Arg::Variable(dest_id),
            )],
            Expr::Id(id) => vec![Instr::movq(Arg::Variable(id), Arg::Variable(dest_id))],
            Expr::Call(func_id, args) => sel_for_call(Some(dest_id), func_id, args),
        },
        Statement::Expr(e) => match e {
            Expr::Call(func_id, args) => sel_for_call(None, func_id, args),
            _ => vec![], // If a statement is just an Expr but not a call, it's always a no-op
        },
    }
}

fn sel_for_add(dest_id: Identifier, left: Box<Expr>, right: Box<Expr>) -> Vec<Instr> {
    if let Expr::Id(left_id) = &*left
        && left_id == &dest_id
    {
        // Expression can be calculated in 1 instr, dest
        // is the same as the left arg (x = x + 1)
        vec![Instr::addq(atom_to_arg(*right), Arg::Variable(dest_id))]
    } else if let Expr::Id(right_id) = &*right
        && right_id == &dest_id
    {
        // Expression can be calculated in 1 instr, dest
        // is the same as the right arg (x = 1 + x)
        vec![Instr::addq(atom_to_arg(*left), Arg::Variable(dest_id))]
    } else {
        // Expression requires two instructions - load
        // left into dest, then add right into dest
        vec![
            Instr::movq(atom_to_arg(*left), Arg::Variable(dest_id.clone())),
            Instr::addq(atom_to_arg(*right), Arg::Variable(dest_id)),
        ]
    }
}

fn sel_for_sub(dest_id: Identifier, left: Box<Expr>, right: Box<Expr>) -> Vec<Instr> {
    if let Expr::Id(left_id) = &*left
        && left_id == &dest_id
    {
        // Expression can be calculated in 1 instr, dest
        // is the same as the left arg (x = x - 1)
        vec![Instr::subq(atom_to_arg(*right), Arg::Variable(dest_id))]
    } else {
        // Expression requires two instructions - load
        // left into dest, then subtract right from dest
        vec![
            Instr::movq(atom_to_arg(*left), Arg::Variable(dest_id.clone())),
            Instr::subq(atom_to_arg(*right), Arg::Variable(dest_id)),
        ]
    }
}

fn sel_for_unary_plus(dest_id: Identifier, val: Box<Expr>) -> Vec<Instr> {
    if let Expr::Id(val_id) = &*val
        && val_id == &dest_id
    {
        // Plus on itself? No-op! (x = +x)
        vec![]
    } else {
        // Just a mov? (x = +y)
        vec![Instr::movq(atom_to_arg(*val), Arg::Variable(dest_id))]
    }
}

fn sel_for_unary_minus(dest_id: Identifier, val: Box<Expr>) -> Vec<Instr> {
    if let Expr::Id(val_id) = &*val
        && val_id == &dest_id
    {
        // Minus on itself: 1 instr (x = -x)
        vec![Instr::negq(Arg::Variable(dest_id))]
    } else {
        // x = -y
        vec![
            Instr::movq(atom_to_arg(*val), Arg::Variable(dest_id.clone())),
            Instr::negq(Arg::Variable(dest_id)),
        ]
    }
}

fn sel_for_call(dest_id: Option<Identifier>, func_id: Identifier, args: Vec<Expr>) -> Vec<Instr> {
    if args.len() > 6 {
        unimplemented!("Only register arg passing is implemented, max of 6 args");
    }
    let func_name = if let Identifier::Named(f) = func_id {
        f
    } else {
        unimplemented!("Ephemeral function names are not implmented");
    };

    // As per the calling convention, the first 6 args are passed in registers, in this order
    const CALLING_CONV_ARG_PASSING_REG_MAP: [Arg; 6] = [
        Arg::Reg(Register::rdi),
        Arg::Reg(Register::rsi),
        Arg::Reg(Register::rdx),
        Arg::Reg(Register::rcx),
        Arg::Reg(Register::r8),
        Arg::Reg(Register::r9),
    ];
    // As per the calling convention, the return value will be in this register
    const CALLING_CONV_RETURN_REG: Arg = Arg::Reg(Register::rax);

    let mut instrs = vec![];

    let num_args = args.len();
    // Push the args to the correct registers
    for (arg_expr, reg) in args.into_iter().zip(CALLING_CONV_ARG_PASSING_REG_MAP) {
        instrs.push(Instr::movq(atom_to_arg(arg_expr), reg));
    }

    instrs.push(Instr::callq(func_name, num_args as _));

    if let Some(dest_id) = dest_id {
        instrs.push(Instr::movq(CALLING_CONV_RETURN_REG, Arg::Variable(dest_id)));
    }

    instrs
}

fn atom_to_arg(e: Expr) -> Arg {
    // Atomic values are either constants or variables, all others are non-atomic
    match e {
        Expr::Constant(Value::I64(val)) => Arg::Immediate(val),
        Expr::Id(id) => Arg::Variable(id),
        _ => panic!("Non-Atom `{e:?}` passed to atom_to_arg"),
    }
}
