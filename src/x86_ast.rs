use std::fmt::Display;

pub use crate::ast::Identifier;

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

pub const STACK_ALIGNMENT: i32 = 16;

pub const CALLEE_SAVED_REGISTERS: [Register; 7] = [
    Register::rsp,
    Register::rbp,
    Register::rbx,
    Register::r12,
    Register::r13,
    Register::r14,
    Register::r15,
];

pub const CALLER_SAVED_REGISTERS: [Register; 9] = [
    Register::rax,
    Register::rcx,
    Register::rdx,
    Register::rsi,
    Register::rdi,
    Register::r8,
    Register::r9,
    Register::r10,
    Register::r11,
];

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ByteReg {
    ah,
    al,
    bh,
    bl,
    ch,
    cl,
    dh,
    dl
}

impl ByteReg {
    pub fn to_underlying(&self) -> Register {
        match self {
            ByteReg::ah | ByteReg::al => Register::rax,
            ByteReg::bh | ByteReg::bl => Register::rbx,
            ByteReg::ch | ByteReg::cl => Register::rcx,
            ByteReg::dh | ByteReg::dl => Register::rdx,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Arg {
    Immediate(i64),
    Reg(Register),
    ByteReg(ByteReg),
    Deref(Register, i32),
    Variable(Identifier),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Comparison {
    Equals,
    NotEquals,
    Less,
    LessEquals,
    Greater,
    GreaterEquals
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
    callq(Identifier, u16),
    retq,
    xorq(Arg, Arg),
    cmpq(Arg, Arg),
    set(Comparison, ByteReg),
    movzbq(ByteReg, Arg),
    jmp(Identifier),
    jmpcc(Comparison, Identifier)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Directive {
    Label(Identifier),
    Globl(Identifier),
    AttSyntax,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    pub label: Directive,
    pub instrs: Vec<Instr>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct X86Program {
    pub header: Vec<Directive>,
    pub blocks: Vec<Block>,
    pub(crate) stack_size: usize,
}

impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Register::rax => write!(f, "%rax"),
            Register::rbx => write!(f, "%rbx"),
            Register::rcx => write!(f, "%rcx"),
            Register::rdx => write!(f, "%rdx"),
            Register::rsi => write!(f, "%rsi"),
            Register::rdi => write!(f, "%rdi"),
            Register::rbp => write!(f, "%rbp"),
            Register::rsp => write!(f, "%rsp"),
            Register::r8 => write!(f, "%r8"),
            Register::r9 => write!(f, "%r9"),
            Register::r10 => write!(f, "%r10"),
            Register::r11 => write!(f, "%r11"),
            Register::r12 => write!(f, "%r12"),
            Register::r13 => write!(f, "%r13"),
            Register::r14 => write!(f, "%r14"),
            Register::r15 => write!(f, "%r15"),
        }
    }
}

impl Display for ByteReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ByteReg::ah => write!(f, "%ah"),
            ByteReg::al => write!(f, "%al"),
            ByteReg::bh => write!(f, "%bh"),
            ByteReg::bl => write!(f, "%bl"),
            ByteReg::ch => write!(f, "%ch"),
            ByteReg::cl => write!(f, "%cl"),
            ByteReg::dh => write!(f, "%dh"),
            ByteReg::dl => write!(f, "%dl"),
        }
    }
}

impl Display for Arg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Arg::Immediate(val) => write!(f, "${}", val),
            Arg::Reg(reg) => reg.fmt(f),
            Arg::ByteReg(reg) => reg.fmt(f),
            Arg::Deref(reg, offset) => write!(f, "{offset}({reg})"),
            Arg::Variable(id) => match id {
                Identifier::Named(name) => write!(f, "@{name}"),
                Identifier::Ephemeral(id) => write!(f, "@EE#{id}"),
            },
        }
    }
}

impl Display for Comparison {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Comparison::Equals => write!(f, "e"),
            Comparison::NotEquals => write!(f, "ne"),
            Comparison::Less => write!(f, "l"),
            Comparison::LessEquals => write!(f, "le"),
            Comparison::Greater => write!(f, "g"),
            Comparison::GreaterEquals => write!(f, "ge"),
        }
    }
}

fn fmt_label(label: &Identifier) -> String {
    match label {
        Identifier::Named(name) => name.to_string(),
        Identifier::Ephemeral(id) => format!(".EE_{id}"),
    }
}

impl Display for Instr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instr::addq(arg, arg1) => write!(f, "addq {arg}, {arg1}"),
            Instr::subq(arg, arg1) => write!(f, "subq {arg}, {arg1}"),
            Instr::negq(arg) => write!(f, "negq {arg}"),
            Instr::movq(arg, arg1) => write!(f, "movq {arg}, {arg1}"),
            Instr::pushq(arg) => write!(f, "pushq {arg}"),
            Instr::popq(arg) => write!(f, "popq {arg}"),
            Instr::callq(label, _) => write!(f, "callq {}", fmt_label(label)),
            Instr::retq => write!(f, "retq"),
            Instr::xorq(arg, arg1) => write!(f, "xorq {arg}, {arg1}"),
            Instr::cmpq(arg, arg1) => write!(f, "cmpq {arg}, {arg1}"),
            Instr::set(cmp, arg) => write!(f, "set{cmp} {arg}"),
            Instr::movzbq(arg, arg1) => write!(f, "movzbq {arg}, {arg1}"),
            Instr::jmp(label) => write!(f, "jmp {}", fmt_label(label)),
            Instr::jmpcc(cmp, label) => write!(f, "j{cmp} {}", fmt_label(label)),
        }
    }
}

impl Display for Directive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Label(label) => write!(f, "{}:", fmt_label(label)),
            Self::Globl(label) => write!(f, "\t.globl {}", fmt_label(label)),
            Self::AttSyntax => write!(f, "\t.att_syntax"),
        }
    }
}

impl Display for X86Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for block in &self.blocks {
            writeln!(f, "{}", block.label)?;
            for i in block.instrs.iter() {
                writeln!(f, "\t{i}")?;
            }
        }
        Ok(())
    }
}
