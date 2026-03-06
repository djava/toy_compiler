use indexmap::IndexMap;

use crate::{
    constants::*,
    passes::IRtoX86Pass,
    syntax_trees::{
        ir,
        shared::*,
        x86::{self, Instr, Register, X86Program},
    },
    utils::global,
};

/// `TranslateIRtoX86` Pass
///
/// Lowers IR instructions to `x86_64` assembly instructions, selecting
/// instruction sequences for each IR operation (arithmetic,
/// comparisons, memory allocation, function calls, tail calls,
/// subscripts). Arguments are passed in registers per the calling
/// convention; variables remain as `Arg::Variable` pseudo-registers
/// pending register allocation.
///
/// It is mandatory to run this pass
///
/// Pre-conditions:
/// - `TranslateASTtoIR`
///
/// Post-conditions:
/// - An `X86Program` with `Arg::Variable` pseudo-registers that have
///   not yet been assigned to physical registers or stack slots
#[derive(Debug)]
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

                label
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
                header: vec![],
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

    x86::Block { label, instrs }
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
        ir::Statement::TailCall(func, args) => {
            if args.len() > MAX_REGISTER_ARGS {
                unimplemented!(
                    "Only register arg passing is implemented, max of {MAX_REGISTER_ARGS} args"
                );
            }

            if SPECIAL_FUNCTIONS
                .iter()
                .map(|(name, _, _)| name)
                .find(|n| &ir::Atom::GlobalSymbol(global!(**n)) == &func)
                .is_some()
            {
                return translate_call(None, func, args);
            }

            let mut instrs = vec![];

            let num_args = args.len();
            // Push the args to the correct registers
            for (arg_expr, reg) in args.into_iter().zip(CALL_ARG_REGISTERS) {
                instrs.push(Instr::movq(atom_to_arg(arg_expr), x86::Arg::Reg(reg)));
            }

            // Jump to the function
            match func {
                ir::Atom::Variable(_) => {
                    instrs.extend([
                        Instr::movq(atom_to_arg(func), x86::Arg::Reg(Register::rax)),
                        Instr::jmp_tail(x86::Arg::Reg(Register::rax), num_args as _),
                    ]);
                }
                ir::Atom::GlobalSymbol(id) => {
                    instrs.push(Instr::jmp_tail(x86::Arg::Global(id), num_args as _));
                }
                ir::Atom::Constant(_) => panic!("func was Atom::Constant??"),
            }
            instrs
        }
    }
}

fn translate_assign(dest: AssignDest<ir::Atom>, expr: ir::Expr) -> Vec<Instr> {
    let mut ret = vec![];

    ret.extend(match expr {
        ir::Expr::Atom(atom) => translate_atom(dest, atom),
        ir::Expr::UnaryOp(UnaryOperator::Plus, val) => translate_unary_plus(dest, val),
        ir::Expr::UnaryOp(UnaryOperator::Minus, val) => translate_unary_minus(dest, val),
        ir::Expr::UnaryOp(UnaryOperator::Not, val) => translate_not(dest, val),
        ir::Expr::BinaryOp(l, BinaryOperator::Add, r) => translate_add(dest, l, r),
        ir::Expr::BinaryOp(l, BinaryOperator::Subtract, r) => translate_subtract(dest, l, r),
        ir::Expr::BinaryOp(l, BinaryOperator::Multiply, r) => translate_multiply(dest, l, r),
        ir::Expr::BinaryOp(l, BinaryOperator::LeftShift, r) => {
            translate_bitshift(BitshiftDirection::Left, dest, l, r)
        }
        ir::Expr::BinaryOp(l, BinaryOperator::RightShift, r) => {
            translate_bitshift(BitshiftDirection::Right, dest, l, r)
        }
        ir::Expr::BinaryOp(l, cmp_op, r) => translate_comparison(dest, cmp_op, l, r),
        ir::Expr::Call(func_id, args) => translate_call(Some(dest), func_id, args),
        ir::Expr::Allocate(bytes, value_type) => translate_allocation(dest, bytes, value_type),
        ir::Expr::TupleSubscript(atom, idx) => translate_subscript(dest, atom, idx),
    });

    ret
}

fn translate_subscript(dest: AssignDest<ir::Atom>, atom: ir::Atom, idx: i64) -> Vec<Instr> {
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

fn translate_atom(dest: AssignDest<ir::Atom>, atom: ir::Atom) -> Vec<Instr> {
    let mut ret = vec![];
    if let AssignDest::Subscript(id, _idx) = &dest {
        ret.push(Instr::movq(
            x86::Arg::Variable(id.clone()),
            x86::Arg::Reg(x86::Register::r11),
        ));
    }

    if let ir::Atom::GlobalSymbol(_) = &atom {
        // Global symbols (functions) must be leaq'd, not just mov'd from
        ret.push(Instr::leaq(atom_to_arg(atom), assigndest_to_arg(dest)));
    } else {
        ret.push(Instr::movq(atom_to_arg(atom), assigndest_to_arg(dest)));
    }

    ret
}

fn translate_allocation(
    dest: AssignDest<ir::Atom>,
    bytes: usize,
    value_type: ValueType,
) -> Vec<Instr> {
    let tag = i64::from_ne_bytes(make_allocation_tag(value_type).to_ne_bytes());

    // Bump allocator pointer, write tag. pointer is in r11
    let mut ret = vec![
        Instr::movq(
            x86::Arg::Global(global!(GC_FREE_PTR)),
            x86::Arg::Reg(Register::r11),
        ),
        Instr::addq(
            x86::Arg::Immediate(bytes as i64),
            x86::Arg::Global(global!(GC_FREE_PTR)),
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

fn make_allocation_tag(value_type: ValueType) -> u64 {
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
    } else if let ValueType::ArrayType(elems, len) = value_type {
        let pointer_mask = matches!(*elems, ValueType::PointerType(_));

        ArrayTag::new()
            .with_forwarding(false)
            .with_length(len as u64)
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
        ir::Expr::TupleSubscript(atom, idx) => {
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
    dest: AssignDest<ir::Atom>,
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

fn translate_add(dest: AssignDest<ir::Atom>, left: ir::Atom, right: ir::Atom) -> Vec<Instr> {
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

fn translate_subtract(dest: AssignDest<ir::Atom>, left: ir::Atom, right: ir::Atom) -> Vec<Instr> {
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

fn translate_multiply(dest: AssignDest<ir::Atom>, left: ir::Atom, right: ir::Atom) -> Vec<Instr> {
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

fn translate_not(dest: AssignDest<ir::Atom>, atom: ir::Atom) -> Vec<Instr> {
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

fn translate_unary_minus(dest: AssignDest<ir::Atom>, atom: ir::Atom) -> Vec<Instr> {
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

fn translate_unary_plus(dest: AssignDest<ir::Atom>, atom: ir::Atom) -> Vec<Instr> {
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

#[derive(Debug, Clone, Copy, PartialEq)]
enum BitshiftDirection {
    Left,
    Right,
}

fn translate_bitshift(
    dir: BitshiftDirection,
    dest: AssignDest<ir::Atom>,
    left: ir::Atom,
    right: ir::Atom,
) -> Vec<Instr> {
    let mut ret = vec![];
    if let AssignDest::Subscript(id, _idx) = &dest {
        ret.push(Instr::movq(
            x86::Arg::Variable(id.clone()),
            x86::Arg::Reg(x86::Register::r11),
        ));
    }

    let shift_arg = {
        if let ir::Atom::Constant(_) = right {
            // Shift by immediate, can just do the instruction
            atom_to_arg(right)
        } else {
            // Shift by non-constant, need to move RHS into %cl first,
            // and need to convert it a into bytereg first
            ret.push(Instr::mov(atom_to_arg(right), x86::ByteReg::cl));
            x86::Arg::ByteReg(x86::ByteReg::cl)
        }
    };

    if let ir::Atom::Variable(left_id) = &left
        && let AssignDest::Id(dest_id) = &dest
        && left_id == dest_id
    {
        // Expression can be calculated in 1 instr, dest
        // is the same as the left arg (x = x << y)
        let shift_instr = match dir {
            BitshiftDirection::Left => Instr::salq(shift_arg, atom_to_arg(left)),
            BitshiftDirection::Right => Instr::sarq(shift_arg, atom_to_arg(left)),
        };
        ret.push(shift_instr);
    } else {
        // Expression requires two instructions - load
        // left into dest, then add right into dest
        let shift_instr = match dir {
            BitshiftDirection::Left => {
                Instr::salq(shift_arg, assigndest_to_arg(dest.clone()))
            }
            BitshiftDirection::Right => {
                Instr::sarq(shift_arg, assigndest_to_arg(dest.clone()))
            }
        };

        ret.extend([
            Instr::movq(atom_to_arg(left), assigndest_to_arg(dest.clone())),
            shift_instr,
        ]);
    }

    ret
}

const SPECIAL_FUNCTIONS: [(
    &'static str,
    usize,
    fn(Vec<ir::Atom>, Option<AssignDest<ir::Atom>>) -> Vec<Instr>,
); 1] = [(FN_GC_COLLECT, 1, |mut args, _dest| {
    assert!(_dest.is_none());
    vec![
        Instr::movq(x86::Arg::Reg(Register::r15), x86::Arg::Reg(Register::rdi)),
        Instr::movq(atom_to_arg(args.remove(0)), x86::Arg::Reg(Register::rsi)),
        Instr::callq(x86::Arg::Global(global!(FN_GC_COLLECT)), 2),
    ]
})];

fn translate_call(
    dest_opt: Option<AssignDest<ir::Atom>>,
    func: ir::Atom,
    args: Vec<ir::Atom>,
) -> Vec<Instr> {
    if args.len() > MAX_REGISTER_ARGS {
        unimplemented!("Only register arg passing is implemented, max of {MAX_REGISTER_ARGS} args");
    }

    for (name, num_args, instr_fn) in SPECIAL_FUNCTIONS {
        if func == ir::Atom::Variable(global!(name)) {
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

    // Jump to the function
    match func {
        ir::Atom::Variable(_) => {
            instrs.extend([
                Instr::movq(atom_to_arg(func), x86::Arg::Reg(Register::rax)),
                Instr::callq(x86::Arg::Reg(Register::rax), num_args as _),
            ]);
        }
        ir::Atom::GlobalSymbol(id) => {
            instrs.push(Instr::callq(x86::Arg::Global(id), num_args as _));
        }
        ir::Atom::Constant(_) => panic!("func was Atom::Constant??"),
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
        label: Identifier::new_ephemeral(),
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
        | BinaryOperator::Or
        | BinaryOperator::LeftShift
        | BinaryOperator::RightShift => None,
    }
}

fn assigndest_to_arg(dest: AssignDest<ir::Atom>) -> x86::Arg {
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
        AssignDest::ComplexSubscript(_) => {
            panic!("Should've been removed by DisambiguateSubscript")
        }
    }
}
