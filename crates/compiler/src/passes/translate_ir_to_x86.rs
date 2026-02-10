use indexmap::IndexMap;

use crate::{
    constants::*,
    passes::IRtoX86Pass,
    syntax_trees::{
        ir,
        shared::*,
        x86::{self, Instr, Register, X86Program},
    },
    utils::id,
};

pub struct TranslateIRtoX86;

impl IRtoX86Pass for TranslateIRtoX86 {
    fn run_pass(self, m: ir::IRProgram) -> X86Program {
        let mut x86_functions = vec![];
        for f in m.functions {
            let mut x86_blocks = vec![];

            let entry_block = if f.params.is_empty() {
                f.entry_block
            } else {
                let block = translate_arg_passing(f.params, f.entry_block);
                let label = block.label.clone();
                x86_blocks.push(block);

                if let x86::Directive::Label(name) = label {
                    name
                } else {
                    unreachable!()
                }
            };

            for (id, block) in f.blocks {
                x86_blocks.push(translate_block(id, block, &f.exit_block));
            }

            x86_functions.push(x86::Function {
                name: f.name,
                blocks: x86_blocks,
                entry_block,
                exit_block: f.exit_block,
                stack_size: 0,
                gc_stack_size: 0,
                types: f.types,
                callee_saved_used: vec![],
                header: vec![]
            });
        }
        X86Program {
            header: vec![],
            functions: x86_functions,
        }
    }
}

fn translate_block(label: Identifier, b: ir::Block, exit_block: &Identifier) -> x86::Block {
    let mut instrs = vec![];

    for s in b.statements {
        instrs.extend(translate_statement(s, exit_block));
    }

    x86::Block {
        label: x86::Directive::Label(label),
        instrs,
    }
}

fn translate_statement(s: ir::Statement, exit_block: &Identifier) -> Vec<Instr> {
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
            Instr::jmp(exit_block.clone()),
        ],
        ir::Statement::Goto(label) => vec![Instr::jmp(label)],
        ir::Statement::If(cond, pos_label, neg_label) => {
            translate_conditional(cond, pos_label, neg_label)
        }
        ir::Statement::TailCall(func_id, args) => {
            if args.len() > MAX_REGISTER_ARGS {
                unimplemented!(
                    "Only register arg passing is implemented, max of {MAX_REGISTER_ARGS} args"
                );
            }

            if SPECIAL_FUNCTIONS
                .iter()
                .map(|(name, _, _)| name)
                .find(|n| id!(**n) == func_id)
                .is_some()
            {
                return translate_call(None, ir::Atom::Variable(func_id), args);
            }

            let mut instrs = vec![];

            let num_args = args.len();
            // Push the args to the correct registers
            for (arg_expr, reg) in args.into_iter().zip(CALL_ARG_REGISTERS) {
                instrs.push(Instr::movq(atom_to_arg(arg_expr), x86::Arg::Reg(reg)));
            }

            // Jump to the function
            instrs.push(Instr::leaq(
                x86::Arg::Global(func_id),
                x86::Arg::Reg(Register::rax),
            ));
            instrs.push(Instr::jmp_tail(x86::Arg::Reg(Register::rax), num_args as _));
            instrs
        }
    }
}

fn translate_assign(dest: AssignDest, expr: ir::Expr) -> Vec<Instr> {
    let mut ret = vec![];

    ret.extend(match expr {
        ir::Expr::Atom(atom) => translate_atom(dest, atom),
        ir::Expr::UnaryOp(UnaryOperator::Plus, val) => translate_unary_plus(dest, val),
        ir::Expr::UnaryOp(UnaryOperator::Minus, val) => translate_unary_minus(dest, val),
        ir::Expr::UnaryOp(UnaryOperator::Not, val) => translate_not(dest, val),
        ir::Expr::BinaryOp(l, BinaryOperator::Add, r) => translate_add(dest, l, r),
        ir::Expr::BinaryOp(l, BinaryOperator::Subtract, r) => translate_subtract(dest, l, r),
        ir::Expr::BinaryOp(l, BinaryOperator::Multiply, r) => translate_multiply(dest, l, r),
        ir::Expr::BinaryOp(l, cmp_op, r) => translate_comparison(dest, cmp_op, l, r),
        ir::Expr::Call(func_id, args) => translate_call(Some(dest), func_id, args),
        ir::Expr::Allocate(bytes, value_type) => translate_allocation(dest, bytes, value_type),
        ir::Expr::Subscript(atom, idx) => translate_subscript(dest, atom, idx),
    });

    ret
}

fn translate_subscript(dest: AssignDest, atom: ir::Atom, idx: i64) -> Vec<Instr> {
    let mut ret = vec![];
    if let AssignDest::Subscript(id, _idx) = &dest {
        ret.push(Instr::movq(
            x86::Arg::Variable(id.clone()),
            x86::Arg::Reg(x86::Register::r11),
        ));
    }

    ret.extend([
        Instr::movq(atom_to_arg(atom), x86::Arg::Reg(Register::rax)),
        Instr::movq(
            x86::Arg::Deref(Register::rax, (WORD_SIZE + (WORD_SIZE * idx)) as i32),
            assigndest_to_arg(dest),
        ),
    ]);

    ret
}

fn translate_atom(dest: AssignDest, atom: ir::Atom) -> Vec<Instr> {
    if let AssignDest::Subscript(id, _idx) = &dest {
        vec![
            Instr::movq(
                x86::Arg::Variable(id.clone()),
                x86::Arg::Reg(x86::Register::r11),
            ),
            Instr::movq(atom_to_arg(atom), assigndest_to_arg(dest)),
        ]
    } else {
        vec![Instr::movq(atom_to_arg(atom), assigndest_to_arg(dest))]
    }
}

fn translate_allocation(dest: AssignDest, bytes: usize, value_type: ValueType) -> Vec<Instr> {
    let tag = i64::from_ne_bytes(make_tuple_tag(value_type).to_ne_bytes());

    // Bump allocator pointer, write tag. pointer is in r11
    let mut ret = vec![
        Instr::movq(
            x86::Arg::Global(id!(GC_FREE_PTR)),
            x86::Arg::Reg(Register::r11),
        ),
        Instr::addq(
            x86::Arg::Immediate(bytes as i64),
            x86::Arg::Global(id!(GC_FREE_PTR)),
        ),
        Instr::movq(x86::Arg::Immediate(tag), x86::Arg::Deref(Register::r11, 0)),
    ];

    if let AssignDest::Subscript(id, idx) = &dest {
        ret.extend([
            Instr::movq(
                x86::Arg::Variable(id.clone()),
                x86::Arg::Reg(x86::Register::rax),
            ),
            Instr::movq(
                x86::Arg::Reg(x86::Register::r11),
                x86::Arg::Deref(
                    Register::rax,
                    (WORD_SIZE + (idx * WORD_SIZE)).try_into().unwrap(),
                ),
            ),
        ]);
    } else {
        ret.push(Instr::movq(
            x86::Arg::Reg(Register::r11),
            assigndest_to_arg(dest),
        ));
    }

    ret
}

fn make_tuple_tag(value_type: ValueType) -> u64 {
    if let ValueType::TupleType(elems) = value_type {
        if elems.len() > MAX_TUPLE_ELEMENTS {
            unimplemented!("The compiler has a max of {MAX_TUPLE_ELEMENTS} tuple elements")
        }

        let pointer_mask = elems
            .iter()
            .map(|e| matches!(e, ValueType::PointerType(_)))
            .fold(0u64, |acc, e| (acc << 1) | (e as u64));

        TupleTag::new()
            .with_forwarding(false)
            .with_length(elems.len() as u8)
            .with_pointer_mask(pointer_mask)
            .into_bits()
    } else {
        panic!("Passed non-tuple ValueType to make_tuple_tag")
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
                let mut asgn_instrs = translate_assign(AssignDest::Id(temp_id.clone()), cond);
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
                Instr::jmpcc(x86::Comparison::NotEquals, pos_label),
            ]);
            call_instrs
        }
        ir::Expr::Subscript(atom, idx) => {
            if let ir::Atom::Variable(var) = atom {
                vec![
                    Instr::movq(x86::Arg::Variable(var), x86::Arg::Reg(x86::Register::rax)),
                    Instr::cmpq(
                        x86::Arg::Immediate(0),
                        x86::Arg::Deref(Register::rax, (WORD_SIZE + (WORD_SIZE * idx)) as i32),
                    ),
                    Instr::jmpcc(x86::Comparison::NotEquals, pos_label),
                ]
            } else {
                panic!("Doesn't make sense to subscript a constant or global")
            }
        }
        ir::Expr::Allocate(_, _) => panic!("Doesn't make sense for Allocate to be a predicate"),
    };

    instrs.push(Instr::jmp(neg_label));
    instrs
}

fn translate_comparison(
    dest: AssignDest,
    cmp_op: BinaryOperator,
    l: ir::Atom,
    r: ir::Atom,
) -> Vec<Instr> {
    let cc = if let Some(c) = try_binop_to_cc(cmp_op) {
        c
    } else {
        panic!(
            "Tried to convert {cmp_op:?} to x86::Comparison - Missing case in translate_expr()?"
        );
    };

    let mut ret = vec![];
    if let AssignDest::Subscript(id, _idx) = &dest {
        ret.push(Instr::movq(
            x86::Arg::Variable(id.clone()),
            x86::Arg::Reg(x86::Register::r11),
        ));
    }

    ret.extend([
        Instr::cmpq(atom_to_arg(r), atom_to_arg(l)),
        Instr::set(cc, x86::ByteReg::al),
        Instr::movzbq(x86::ByteReg::al, assigndest_to_arg(dest)),
    ]);
    ret
}

fn translate_add(dest: AssignDest, left: ir::Atom, right: ir::Atom) -> Vec<Instr> {
    let mut ret = vec![];
    if let AssignDest::Subscript(id, _idx) = &dest {
        ret.push(Instr::movq(
            x86::Arg::Variable(id.clone()),
            x86::Arg::Reg(x86::Register::r11),
        ));
    }

    if let ir::Atom::Variable(left_id) = &left
        && let AssignDest::Id(dest_id) = &dest
        && left_id == dest_id
    {
        // Expression can be calculated in 1 instr, dest
        // is the same as the left arg (x = x + 1)
        ret.push(Instr::addq(atom_to_arg(right), assigndest_to_arg(dest)));
    } else if let ir::Atom::Variable(right_id) = &right
        && let AssignDest::Id(dest_id) = &dest
        && right_id == dest_id
    {
        // Expression can be calculated in 1 instr, dest
        // is the same as the right arg (x = 1 + x)
        ret.push(Instr::addq(atom_to_arg(left), assigndest_to_arg(dest)));
    } else {
        // Expression requires two instructions - load
        // left into dest, then add right into dest
        ret.extend([
            Instr::movq(atom_to_arg(left), assigndest_to_arg(dest.clone())),
            Instr::addq(atom_to_arg(right), assigndest_to_arg(dest)),
        ]);
    }

    ret
}

fn translate_subtract(dest: AssignDest, left: ir::Atom, right: ir::Atom) -> Vec<Instr> {
    let mut ret = vec![];
    if let AssignDest::Subscript(id, _idx) = &dest {
        ret.push(Instr::movq(
            x86::Arg::Variable(id.clone()),
            x86::Arg::Reg(x86::Register::r11),
        ));
    }

    if let ir::Atom::Variable(left_id) = &left
        && let AssignDest::Id(dest_id) = &dest
        && left_id == dest_id
    {
        // Expression can be calculated in 1 instr, dest
        // is the same as the left arg (x = x - 1)
        ret.push(Instr::subq(atom_to_arg(right), assigndest_to_arg(dest)));
    } else {
        // Expression requires two instructions - load
        // left into dest, then subtract right from dest
        ret.extend([
            Instr::movq(atom_to_arg(left), assigndest_to_arg(dest.clone())),
            Instr::subq(atom_to_arg(right), assigndest_to_arg(dest)),
        ]);
    }

    ret
}

fn translate_multiply(dest: AssignDest, left: ir::Atom, right: ir::Atom) -> Vec<Instr> {
    let mut ret = vec![];
    if let AssignDest::Subscript(id, _idx) = &dest {
        ret.push(Instr::movq(
            x86::Arg::Variable(id.clone()),
            x86::Arg::Reg(x86::Register::r11),
        ));
    }

    if let ir::Atom::Variable(left_id) = &left
        && let AssignDest::Id(dest_id) = &dest
        && left_id == dest_id
    {
        // Expression can be calculated in 1 instr, dest
        // is the same as the left arg (x = x * y)
        ret.push(Instr::imulq(atom_to_arg(right), assigndest_to_arg(dest)));
    } else if let ir::Atom::Variable(right_id) = &right
        && let AssignDest::Id(dest_id) = &dest
        && right_id == dest_id
    {
        // Expression can be calculated in 1 instr, dest
        // is the same as the right arg (x = 1 + x)
        ret.push(Instr::imulq(atom_to_arg(left), assigndest_to_arg(dest)));
    } else {
        // Expression requires two instructions - load
        // left into dest, then add right into dest
        ret.extend([
            Instr::movq(atom_to_arg(left), assigndest_to_arg(dest.clone())),
            Instr::imulq(atom_to_arg(right), assigndest_to_arg(dest)),
        ]);
    }

    ret
}

fn translate_not(dest: AssignDest, atom: ir::Atom) -> Vec<Instr> {
    let mut ret = vec![];
    if let AssignDest::Subscript(id, _idx) = &dest {
        ret.push(Instr::movq(
            x86::Arg::Variable(id.clone()),
            x86::Arg::Reg(x86::Register::r11),
        ));
    }

    if let ir::Atom::Variable(val_id) = &atom
        && let AssignDest::Id(dest_id) = &dest
        && val_id == dest_id
    {
        // Not on itself: 1 instr (x = !x)
        ret.push(Instr::xorq(x86::Arg::Immediate(1), assigndest_to_arg(dest)));
    } else {
        // x = !y
        ret.extend([
            Instr::movq(atom_to_arg(atom), assigndest_to_arg(dest.clone())),
            Instr::xorq(x86::Arg::Immediate(1), assigndest_to_arg(dest)),
        ]);
    }

    ret
}

fn translate_unary_minus(dest: AssignDest, atom: ir::Atom) -> Vec<Instr> {
    let mut ret = vec![];
    if let AssignDest::Subscript(id, _idx) = &dest {
        ret.push(Instr::movq(
            x86::Arg::Variable(id.clone()),
            x86::Arg::Reg(x86::Register::r11),
        ));
    }

    if let ir::Atom::Variable(val_id) = &atom
        && let AssignDest::Id(dest_id) = &dest
        && val_id == dest_id
    {
        // Minus on itself: 1 instr (x = -x)
        ret.push(Instr::negq(assigndest_to_arg(dest)));
    } else {
        // x = -y
        ret.extend([
            Instr::movq(atom_to_arg(atom), assigndest_to_arg(dest.clone())),
            Instr::negq(assigndest_to_arg(dest)),
        ]);
    }

    ret
}

fn translate_unary_plus(dest: AssignDest, atom: ir::Atom) -> Vec<Instr> {
    if let ir::Atom::Variable(val_id) = &atom
        && let AssignDest::Id(dest_id) = &dest
        && val_id == dest_id
    {
        // Plus on itself? No-op! (x = +x)
        vec![]
    } else {
        // Just a mov? (x = +y)
        let mut ret = vec![];
        if let AssignDest::Subscript(id, _idx) = &dest {
            ret.push(Instr::movq(
                x86::Arg::Variable(id.clone()),
                x86::Arg::Reg(x86::Register::r11),
            ));
        }

        ret.push(Instr::movq(atom_to_arg(atom), assigndest_to_arg(dest)));
        ret
    }
}

const SPECIAL_FUNCTIONS: [(
    &'static str,
    usize,
    fn(Vec<ir::Atom>, Option<AssignDest>) -> Vec<Instr>,
); 2] = [
    (GC_COLLECT, 1, |mut args, _dest| {
        assert!(_dest.is_none());
        vec![
            Instr::movq(x86::Arg::Reg(Register::r15), x86::Arg::Reg(Register::rdi)),
            Instr::movq(atom_to_arg(args.remove(0)), x86::Arg::Reg(Register::rsi)),
            Instr::callq(id!(GC_COLLECT), 2),
        ]
    }),
    (FN_LEN, 1, |mut args, dest_opt| {
        if let Some(dest) = dest_opt {
            vec![
                Instr::movq(atom_to_arg(args.remove(0)), x86::Arg::Reg(Register::rax)),
                Instr::movq(
                    x86::Arg::Deref(Register::rax, 0),
                    x86::Arg::Reg(Register::rax),
                ),
                // Shift and mask out the length field of the tuple tag
                Instr::sarq(
                    x86::Arg::Immediate(TUPLE_LENGTH_TAG_SHIFT),
                    x86::Arg::Reg(Register::rax),
                ),
                Instr::andq(
                    x86::Arg::Immediate(TUPLE_LENGTH_TAG_MASK),
                    x86::Arg::Reg(Register::rax),
                ),
                Instr::movq(x86::Arg::Reg(Register::rax), assigndest_to_arg(dest)),
            ]
        } else {
            // No dest - no-op
            vec![]
        }
    }),
];

fn translate_call(dest_opt: Option<AssignDest>, func: ir::Atom, args: Vec<ir::Atom>) -> Vec<Instr> {
    if args.len() > MAX_REGISTER_ARGS {
        unimplemented!("Only register arg passing is implemented, max of {MAX_REGISTER_ARGS} args");
    }

    for (name, num_args, instr_fn) in SPECIAL_FUNCTIONS {
        if func == ir::Atom::Variable(id!(name)) {
            if args.len() != num_args {
                panic!(
                    "Wrong number of args to special function `{name}` (Expected {num_args}, Got {}",
                    args.len()
                );
            }
            return instr_fn(args, dest_opt);
        }
    }

    let mut instrs = vec![];

    let num_args = args.len();
    // Push the args to the correct registers
    for (arg_expr, reg) in args.into_iter().zip(CALL_ARG_REGISTERS) {
        instrs.push(Instr::movq(atom_to_arg(arg_expr), x86::Arg::Reg(reg)));
    }

    match func {
        ir::Atom::Variable(func_name) => {
            let addr_var = Identifier::new_ephemeral();
            instrs.push(Instr::leaq(
                x86::Arg::Global(func_name),
                x86::Arg::Variable(addr_var.clone()),
            ));
            instrs.push(Instr::callq_ind(
                x86::Arg::Variable(addr_var.clone()),
                num_args as u16,
            ));
        }

        ir::Atom::GlobalSymbol(name) => {
            instrs.push(Instr::callq(name, num_args as _));
        }

        ir::Atom::Constant(_) => panic!("Tried to call a non-function value?"),
    }

    if let Some(dest) = dest_opt {
        if let AssignDest::Subscript(id, _idx) = &dest {
            instrs.push(Instr::movq(
                x86::Arg::Variable(id.clone()),
                x86::Arg::Reg(x86::Register::r11),
            ));
        }

        instrs.push(Instr::movq(
            x86::Arg::Reg(CALL_RETURN_REGISTER),
            assigndest_to_arg(dest),
        ));
    }

    instrs
}

fn translate_arg_passing(
    args: IndexMap<Identifier, ValueType>,
    old_entry_point: Identifier,
) -> x86::Block {
    if args.len() > MAX_REGISTER_ARGS {
        panic!("Should have a max of {MAX_REGISTER_ARGS} - bug in tupleize_excess_args?");
    }

    let mut instrs = vec![];
    for ((id, _), reg) in args.into_iter().zip(CALL_ARG_REGISTERS) {
        instrs.push(Instr::movq(x86::Arg::Reg(reg), x86::Arg::Variable(id)))
    }

    instrs.push(Instr::jmp(old_entry_point));

    x86::Block {
        label: x86::Directive::Label(Identifier::new_ephemeral()),
        instrs,
    }
}

fn atom_to_arg(a: ir::Atom) -> x86::Arg {
    match a {
        ir::Atom::Constant(value) => x86::Arg::Immediate(value.into()),
        ir::Atom::Variable(id) => x86::Arg::Variable(id),
        ir::Atom::GlobalSymbol(id) => x86::Arg::Global(id),
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
        BinaryOperator::Is => Some(x86::Comparison::Equals),
        BinaryOperator::Add
        | BinaryOperator::Subtract
        | BinaryOperator::Multiply
        | BinaryOperator::And
        | BinaryOperator::Or => None,
    }
}

fn assigndest_to_arg(dest: AssignDest) -> x86::Arg {
    match dest {
        AssignDest::Id(id) => x86::Arg::Variable(id),
        AssignDest::Subscript(_, offset) => {
            // Assumes that the id-ptr has already been moved into r11,
            // as is convention
            x86::Arg::Deref(
                Register::r11,
                (WORD_SIZE + (offset * WORD_SIZE)).try_into().unwrap(),
            )
        }
    }
}
