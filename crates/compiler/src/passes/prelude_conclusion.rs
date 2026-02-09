use crate::{
    constants::*,
    passes::X86Pass,
    syntax_trees::x86::*,
    utils::id,
};

pub struct PreludeConclusion;

impl X86Pass for PreludeConclusion {
    fn run_pass(self, mut m: X86Program) -> X86Program {
        let header_directives: [Directive; 2] =
            [Directive::AttSyntax, Directive::Globl(id!(LABEL_MAIN))];
        m.header = header_directives.into();

        for f in m.functions.iter_mut() {
            add_prelude_conclusion(f);
        }

        m
    }
}

fn add_prelude_conclusion(f: &mut Function) {
    let entry_block = Block {
        label: Directive::Label(f.name.clone()),
        instrs: generate_prelude(f),
    };
    f.blocks.insert(0, entry_block);
    f.entry_block = f.name.clone();

    let conclusion_instrs = generate_conclusion(f);

    if let Some(exit_block) = f
        .blocks
        .iter_mut()
        .find(|b| b.label == Directive::Label(f.exit_block.clone()))
    {
        exit_block.instrs = conclusion_instrs;
    } else {
        panic!("Couldn't find exit block in function")
    }
}

fn generate_prelude(f: &mut Function) -> Vec<Instr> {
    let mut prelude_instrs = vec![
        // Initialize stack
        Instr::pushq(Arg::Reg(Register::rbp)),
    ];

    // Push any used callee-saved registers onto the stack
    prelude_instrs.extend(
        f.callee_saved_used
            .iter()
            .map(|reg| Instr::pushq(Arg::Reg(*reg))),
    );

    // Setup stack for function
    prelude_instrs.extend([
        Instr::movq(Arg::Reg(Register::rsp), Arg::Reg(Register::rbp)),
        Instr::subq(Arg::Immediate(f.stack_size as _), Arg::Reg(Register::rsp)),
    ]);

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
    prelude_instrs.push(Instr::addq(
        Arg::Immediate(f.gc_stack_size as _),
        Arg::Reg(Register::r15),
    ));

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

fn generate_conclusion(f: &mut Function) -> Vec<Instr> {
    let mut conclusion_instrs = vec![
        // Move the stack pointer back up above this frame
        Instr::addq(Arg::Immediate(f.stack_size as _), Arg::Reg(Register::rsp)),
    ];

    // Pop any used callee-saved registers from the stack, in reverse order
    conclusion_instrs.extend(
        f.callee_saved_used
            .iter()
            .rev()
            .map(|reg| Instr::popq(Arg::Reg(*reg))),
    );

    conclusion_instrs.extend([
        // Move the GC-stack pointer back down
        Instr::subq(
            Arg::Immediate(f.gc_stack_size as _),
            Arg::Reg(Register::r15),
        ),
        // Restore rbp
        Instr::popq(Arg::Reg(Register::rbp)),
        // Return
        Instr::retq,
    ]);

    conclusion_instrs
}
