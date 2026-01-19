use crate::ast::Identifier;

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum Register {
    rax = 0u8,
    rbx = 1u8,
    rcx = 2u8,
    rdx = 3u8,
    rsi = 4u8,
    rdi = 5u8,
    rbp = 6u8,
    rsp = 7u8,
    r8 = 8u8,
    r9 = 9u8,
    r10 = 10u8,
    r11 = 11u8,
    r12 = 12u8,
    r13 = 13u8,
    r14 = 14u8,
    r15 = 15u8,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Arg {
    Intermediate(i32),
    Reg(Register),
    Deref(Register, i32),
    Variable(Identifier),
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instr {
    addq(Arg, Arg),
    subq(Arg, Arg),
    negq(Arg),
    movq(Arg, Arg),
    pushq(Arg),
    popq(Arg),
    callq(String, u16),
    retq,
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum X86Program {
    Body(Vec<Instr>),
}
