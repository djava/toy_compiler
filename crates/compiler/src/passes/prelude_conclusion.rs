use crate::{constants::*, passes::X86Pass, syntax_trees::x86::*, utils::id};

pub struct PreludeConclusion;

impl X86Pass for PreludeConclusion {
    fn run_pass(self, mut m: X86Program) -> X86Program {
        let header_directives: [Directive; 1] = [Directive::AttSyntax];
        m.header = header_directives.into();

        for f in m.functions.iter_mut() {
            expand_tail_calls(f);
            add_prelude_conclusion(f);
        }

        m
    }
}

fn add_prelude_conclusion(f: &mut Function) {
    f.header.push(Directive::Align(WORD_SIZE as u8));
    if f.name == id!(LABEL_MAIN) {
        f.header.push(Directive::Globl(id!(LABEL_MAIN)));
    }

    let entry_block = Block {
        label: Directive::Label(f.name.clone()),
        instrs: generate_prelude(f),
    };
    f.blocks.insert(0, entry_block);
    f.entry_block = f.name.clone();

    let exit_conclusion_instrs =
        generate_conclusion(&f.stack_size, &f.gc_stack_size, &f.callee_saved_used, None);

    if let Some(exit_block) = f
        .blocks
        .iter_mut()
        .find(|b| b.label == Directive::Label(f.exit_block.clone()))
    {
        exit_block.instrs.extend(exit_conclusion_instrs);
    } else {
        panic!("Couldn't find exit block in function")
    }
}

fn expand_tail_calls(f: &mut Function) {
    for b in f.blocks.iter_mut() {
        if let Some(tail_call_instr) = b.instrs.pop_if(|i| matches!(i, Instr::jmp_tail(_, _))) {
            let tail_call_conclusion = generate_conclusion(
                &f.stack_size,
                &f.gc_stack_size,
                &f.callee_saved_used,
                Some(tail_call_instr),
            );
            b.instrs.extend(tail_call_conclusion);
        }
    }
}

fn generate_prelude(f: &mut Function) -> Vec<Instr> {
    let mut prelude_instrs = vec![];

    if f.stack_size > 0 {
        // Initialize stack
        prelude_instrs.push(Instr::pushq(Arg::Reg(Register::rbp)));
    }

    // Push any used callee-saved registers onto the stack
    prelude_instrs.extend(
        f.callee_saved_used
            .iter()
            .map(|reg| Instr::pushq(Arg::Reg(*reg))),
    );

    // Setup stack for function
    if f.stack_size > 0 {
        prelude_instrs.push(Instr::movq(
            Arg::Reg(Register::rsp),
            Arg::Reg(Register::rbp),
        ));
        prelude_instrs.push(Instr::subq(
            Arg::Immediate(f.stack_size as _),
            Arg::Reg(Register::rsp),
        ));
    }

    if f.name == id!(LABEL_MAIN) {
        // In main, we also have to initialize GC Stack/Heap
        prelude_instrs.extend([
            Instr::movq(Arg::Immediate(GC_STACK_SIZE), Arg::Reg(Register::rdi)),
            Instr::movq(Arg::Immediate(GC_HEAP_SIZE), Arg::Reg(Register::rsi)),
            Instr::callq(id!(GC_INITIALIZE), 2),
            Instr::movq(
                Arg::Global(id!(GC_ROOTSTACK_BEGIN)),
                Arg::Reg(Register::r15),
            ),
        ]);
    }

    // Allocate space for GC stack
    if f.gc_stack_size > 0 {
        prelude_instrs.push(Instr::addq(
            Arg::Immediate(f.gc_stack_size as _),
            Arg::Reg(Register::r15),
        ));
    }

    // Zero out GC Stack
    prelude_instrs.extend((0..(f.gc_stack_size / WORD_SIZE as usize) as _).map(|i| {
        Instr::movq(
            Arg::Immediate(0),
            Arg::Deref(Register::r15, (WORD_SIZE * i) as i32),
        )
    }));

    // Jump to function entry point
    prelude_instrs.push(Instr::jmp(f.entry_block.clone()));

    prelude_instrs
}

fn generate_conclusion(
    stack_size: &usize,
    gc_stack_size: &usize,
    callee_saved_used: &Vec<Register>,
    opt_tail_call_instr: Option<Instr>,
) -> Vec<Instr> {
    let mut conclusion_instrs = vec![];

    // Move the stack pointer back up above this frame
    if *stack_size > 0 {
        conclusion_instrs.push(Instr::addq(
            Arg::Immediate(*stack_size as _),
            Arg::Reg(Register::rsp),
        ));
    }

    // Pop any used callee-saved registers from the stack, in reverse order
    conclusion_instrs.extend(
        callee_saved_used
            .iter()
            .rev()
            .map(|reg| Instr::popq(Arg::Reg(*reg))),
    );

    // Move the GC-stack pointer back down
    if *gc_stack_size > 0 {
        conclusion_instrs.push(Instr::subq(
            Arg::Immediate(*gc_stack_size as _),
            Arg::Reg(Register::r15),
        ));
    }

    // Restore rbp
    if *stack_size > 0 {
        conclusion_instrs.push(Instr::popq(Arg::Reg(Register::rbp)));
    }

    // Handle tail-call: If the exit block ends with a jmp_tail then we
    // can do just do a tail call instead of retq. Otherwise, retq normally.
    conclusion_instrs.push(opt_tail_call_instr.unwrap_or(Instr::retq));

    conclusion_instrs
}
