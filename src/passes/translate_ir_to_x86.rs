
use crate::{
    ir::{self, BinaryOperator, IRProgram, Identifier, UnaryOperator},
    passes::IRtoX86Pass,
    x86_ast::{self as x86, Instr, Register, X86Program},
};

pub struct TranslateIRtoX86;

impl IRtoX86Pass for TranslateIRtoX86 {
    fn run_pass(self, m: IRProgram) -> X86Program {
        let mut x86_blocks = vec![];
        for (id, block) in m.blocks {
            x86_blocks.push(translate_block(id, block));
        }

        X86Program {
            header: vec![],
            blocks: x86_blocks,
            stack_size: 0,
        }
    }
}

fn translate_block(label: Identifier, b: ir::Block) -> x86::Block {
    let mut instrs = vec![];

    for s in b.statements {
        instrs.extend(translate_statement(s));
    }

    x86::Block {
        label: x86::Directive::Label(label),
        instrs,
    }
}

fn translate_statement(s: ir::Statement) -> Vec<Instr> {
    match s {
        ir::Statement::Expr(expr) => {
            match expr {
                ir::Expr::Call(func_id, args) => translate_call(None, func_id, args),
                // If a statement is just an Expr but not a call, it's
                // always a no-op. Any sub-exprs are necessarily either Id's
                // or constants because of remove_complex_operands - if this
                // isn't true, atom_to_arg() will panic.
                _ => vec![],
            }
        }
        ir::Statement::Assign(dest_id, expr) => translate_assign(dest_id, expr),
        ir::Statement::Return(atom) => vec![
            Instr::movq(atom_to_arg(atom), x86::Arg::Reg(Register::rax)),
            Instr::jmp(Identifier::from("exit")),
        ],
        ir::Statement::Goto(label) => vec![Instr::jmp(label)],
        ir::Statement::If(cond, pos_label, neg_label) => {
            translate_conditional(cond, pos_label, neg_label)
        }
    }
}

fn translate_assign(dest_id: Identifier, expr: ir::Expr) -> Vec<Instr> {
    match expr {
        ir::Expr::Atom(atom) => vec![Instr::movq(atom_to_arg(atom), x86::Arg::Variable(dest_id))],
        ir::Expr::UnaryOp(UnaryOperator::Plus, val) => translate_unary_plus(dest_id, val),
        ir::Expr::UnaryOp(UnaryOperator::Minus, val) => translate_unary_minus(dest_id, val),
        ir::Expr::UnaryOp(UnaryOperator::Not, val) => translate_not(dest_id, val),
        ir::Expr::BinaryOp(l, BinaryOperator::Add, r) => translate_add(dest_id, l, r),
        ir::Expr::BinaryOp(l, BinaryOperator::Subtract, r) => translate_subtract(dest_id, l, r),
        ir::Expr::BinaryOp(l, cmp_op, r) => translate_comparison(dest_id, cmp_op, l, r),
        ir::Expr::Call(func_id, args) => translate_call(Some(dest_id), func_id, args),
    }
}

fn translate_conditional(
    cond: ir::Expr,
    pos_label: Identifier,
    neg_label: Identifier,
) -> Vec<Instr> {
    let mut instrs = match cond {
        ir::Expr::Atom(atom)
        | ir::Expr::UnaryOp(UnaryOperator::Plus, atom)
        | ir::Expr::UnaryOp(UnaryOperator::Minus, atom) => {
            vec![
                Instr::cmpq(x86::Arg::Immediate(0), atom_to_arg(atom)),
                Instr::jmpcc(x86::Comparison::NotEquals, pos_label),
            ]
        }
        ir::Expr::UnaryOp(UnaryOperator::Not, atom) => vec![
            Instr::cmpq(x86::Arg::Immediate(0), atom_to_arg(atom)),
            Instr::jmpcc(x86::Comparison::Equals, pos_label),
        ],
        ir::Expr::BinaryOp(left, op, right) => {
            if let Some(cc) = try_binop_to_cc(op) {
                vec![
                    Instr::cmpq(atom_to_arg(right), atom_to_arg(left)),
                    Instr::jmpcc(cc, pos_label),
                ]
            } else {
                let temp_id = Identifier::new_ephemeral();
                let cond = ir::Expr::BinaryOp(left, op, right);
                let mut asgn_instrs = translate_assign(temp_id.clone(), cond);
                asgn_instrs.extend([
                    Instr::cmpq(x86::Arg::Immediate(0), x86::Arg::Variable(temp_id)),
                    Instr::jmpcc(x86::Comparison::NotEquals, pos_label),
                ]);
                asgn_instrs
            }
        }
        ir::Expr::Call(func_id, args) => {
            let mut call_instrs = translate_call(None, func_id, args);
            call_instrs.extend([
                Instr::cmpq(x86::Arg::Immediate(0), x86::Arg::Reg(x86::Register::rax)),
                Instr::jmpcc(x86::Comparison::NotEquals, pos_label)
            ]);
            call_instrs
        },
    };

    instrs.push(Instr::jmp(neg_label));
    instrs
}

fn translate_comparison(
    dest_id: Identifier,
    cmp_op: BinaryOperator,
    l: ir::Atom,
    r: ir::Atom,
) -> Vec<Instr> {
    let cc = if let Some(c) = try_binop_to_cc(cmp_op) {
        c
    } else {
        panic!(
            "Tried to convert {cmp_op:?} to x86_ast::Comparison - Missing case in translate_expr()?"
        );
    };

    vec![
        Instr::cmpq(atom_to_arg(r), atom_to_arg(l)),
        Instr::set(cc, x86::ByteReg::al),
        Instr::movzbq(
            x86::ByteReg::al,
            x86::Arg::Variable(dest_id),
        ),
    ]
}

fn translate_add(dest_id: Identifier, left: ir::Atom, right: ir::Atom) -> Vec<Instr> {
    if let ir::Atom::Variable(left_id) = &left
        && left_id == &dest_id
    {
        // Expression can be calculated in 1 instr, dest
        // is the same as the left arg (x = x + 1)
        vec![Instr::addq(atom_to_arg(right), x86::Arg::Variable(dest_id))]
    } else if let ir::Atom::Variable(right_id) = &right
        && right_id == &dest_id
    {
        // Expression can be calculated in 1 instr, dest
        // is the same as the right arg (x = 1 + x)
        vec![Instr::addq(atom_to_arg(left), x86::Arg::Variable(dest_id))]
    } else {
        // Expression requires two instructions - load
        // left into dest, then add right into dest
        vec![
            Instr::movq(atom_to_arg(left), x86::Arg::Variable(dest_id.clone())),
            Instr::addq(atom_to_arg(right), x86::Arg::Variable(dest_id)),
        ]
    }
}

fn translate_subtract(dest_id: Identifier, left: ir::Atom, right: ir::Atom) -> Vec<Instr> {
    if let ir::Atom::Variable(left_id) = &left
        && left_id == &dest_id
    {
        // Expression can be calculated in 1 instr, dest
        // is the same as the left arg (x = x - 1)
        vec![Instr::subq(atom_to_arg(right), x86::Arg::Variable(dest_id))]
    } else {
        // Expression requires two instructions - load
        // left into dest, then subtract right from dest
        vec![
            Instr::movq(atom_to_arg(left), x86::Arg::Variable(dest_id.clone())),
            Instr::subq(atom_to_arg(right), x86::Arg::Variable(dest_id)),
        ]
    }
}

fn translate_not(dest_id: Identifier, atom: ir::Atom) -> Vec<Instr> {
    if let ir::Atom::Variable(val_id) = &atom
        && val_id == &dest_id
    {
        // Not on itself: 1 instr (x = !x)
        vec![Instr::xorq(
            x86::Arg::Immediate(1),
            x86::Arg::Variable(dest_id),
        )]
    } else {
        // x = !y
        vec![
            Instr::movq(atom_to_arg(atom), x86::Arg::Variable(dest_id.clone())),
            Instr::xorq(x86::Arg::Immediate(1), x86::Arg::Variable(dest_id)),
        ]
    }
}

fn translate_unary_minus(dest_id: Identifier, atom: ir::Atom) -> Vec<Instr> {
    if let ir::Atom::Variable(val_id) = &atom
        && val_id == &dest_id
    {
        // Minus on itself: 1 instr (x = -x)
        vec![Instr::negq(x86::Arg::Variable(dest_id))]
    } else {
        // x = -y
        vec![
            Instr::movq(atom_to_arg(atom), x86::Arg::Variable(dest_id.clone())),
            Instr::negq(x86::Arg::Variable(dest_id)),
        ]
    }
}

fn translate_unary_plus(dest_id: Identifier, atom: ir::Atom) -> Vec<Instr> {
    if let ir::Atom::Variable(val_id) = &atom
        && val_id == &dest_id
    {
        // Plus on itself? No-op! (x = +x)
        vec![]
    } else {
        // Just a mov? (x = +y)
        vec![Instr::movq(atom_to_arg(atom), x86::Arg::Variable(dest_id))]
    }
}

fn translate_call(
    dest_id: Option<Identifier>,
    func_id: Identifier,
    args: Vec<ir::Atom>,
) -> Vec<Instr> {
    if args.len() > 6 {
        unimplemented!("Only register arg passing is implemented, max of 6 args");
    }

    // As per the calling convention, the first 6 args are passed in registers, in this order
    const CALLING_CONV_ARG_PASSING_REG_MAP: [x86::Arg; 6] = [
        x86::Arg::Reg(Register::rdi),
        x86::Arg::Reg(Register::rsi),
        x86::Arg::Reg(Register::rdx),
        x86::Arg::Reg(Register::rcx),
        x86::Arg::Reg(Register::r8),
        x86::Arg::Reg(Register::r9),
    ];
    // As per the calling convention, the return value will be in this register
    const CALLING_CONV_RETURN_REG: x86::Arg = x86::Arg::Reg(Register::rax);

    let mut instrs = vec![];

    let num_args = args.len();
    // Push the args to the correct registers
    for (arg_expr, reg) in args.into_iter().zip(CALLING_CONV_ARG_PASSING_REG_MAP) {
        instrs.push(Instr::movq(atom_to_arg(arg_expr), reg));
    }

    instrs.push(Instr::callq(func_id.clone(), num_args as _));

    if let Some(dest_id) = dest_id {
        instrs.push(Instr::movq(
            CALLING_CONV_RETURN_REG,
            x86::Arg::Variable(dest_id),
        ));
    }

    instrs
}

fn atom_to_arg(a: ir::Atom) -> x86::Arg {
    match a {
        ir::Atom::Constant(value) => x86::Arg::Immediate(value.into()),
        ir::Atom::Variable(id) => x86::Arg::Variable(id),
    }
}

fn try_binop_to_cc(op: BinaryOperator) -> Option<x86::Comparison> {
    match op {
        BinaryOperator::Equals => Some(x86::Comparison::Equals),
        BinaryOperator::NotEquals => Some(x86::Comparison::NotEquals),
        BinaryOperator::Greater => Some(x86::Comparison::Greater),
        BinaryOperator::GreaterEquals => Some(x86::Comparison::GreaterEquals),
        BinaryOperator::Less => Some(x86::Comparison::Less),
        BinaryOperator::LessEquals => Some(x86::Comparison::LessEquals),
        BinaryOperator::Add
        | BinaryOperator::Subtract
        | BinaryOperator::And
        | BinaryOperator::Or => None,
    }
}
