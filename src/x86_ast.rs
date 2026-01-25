use std::fmt::Display;
use std::sync::Arc;

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Arg {
    Immediate(i64),
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
    callq(Arc<str>, u16),
    retq,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Directive {
    Label(Arc<str>),
    Globl(Arc<str>),
    AttSyntax,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct X86Program {
    pub functions: Vec<(Directive, Vec<Instr>)>,
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

impl Display for Arg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Arg::Immediate(val) => write!(f, "${}", val),
            Arg::Reg(reg) => write!(f, "{reg}"),
            Arg::Deref(reg, offset) => write!(f, "{offset}({reg})"),
            Arg::Variable(id) => match id {
                // write!(f, "@{id}"),
                Identifier::Named(name) => write!(f, "@{name}"),
                Identifier::Ephemeral(id) => write!(f, "@EE#{id}"),
            },
        }
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
            Instr::callq(arg, _) => write!(f, "callq {arg}"),
            Instr::retq => write!(f, "retq"),
        }
    }
}

impl Display for Directive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Label(name) => write!(f, "{name}:"),
            Self::Globl(name) => write!(f, "\t.globl {name}"),
            Self::AttSyntax => write!(f, "\t.att_syntax"),
        }
    }
}

impl Display for X86Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (dir, instrs) in &self.functions {
            writeln!(f, "{dir}")?;
            for i in instrs {
                writeln!(f, "\t{i}")?;
            }
        }
        Ok(())
    }
}
