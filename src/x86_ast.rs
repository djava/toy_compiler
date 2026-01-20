use std::fmt::Display;

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

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Arg<'a> {
    Immediate(i64),
    Reg(Register),
    Deref(Register, i32),
    Variable(Identifier<'a>),
}

#[allow(non_camel_case_types)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Instr<'a> {
    addq(Arg<'a>, Arg<'a>),
    subq(Arg<'a>, Arg<'a>),
    negq(Arg<'a>),
    movq(Arg<'a>, Arg<'a>),
    pushq(Arg<'a>),
    popq(Arg<'a>),
    callq(&'a str, u16),
    retq,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Directive<'a> {
    Label(&'a str),
    Globl(&'a str),
    AttSyntax,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct X86Program<'a> {
    pub functions: Vec<(Directive<'a>, Vec<Instr<'a>>)>,
    pub(crate) stack_size: usize
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

impl<'a> Display for Arg<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Arg::Immediate(val) => write!(f, "${}", val),
            Arg::Reg(reg) => write!(f, "{reg}"),
            Arg::Deref(reg, offset) => write!(f, "{offset}({reg})"),
            Arg::Variable(id) => match id {
                // write!(f, "@{id}"),
                Identifier::Named(name) => write!(f, "@{name}"),
                Identifier::Ephemeral(id) => write!(f, "@EE#{id}"),
            }
        }
    }
}

impl<'a> Display for Instr<'a> {
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

impl<'a> Display for Directive<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Label(name) => write!(f, "{name}:"),
            Self::Globl(name) => write!(f, "\t.globl {name}"),
            Self::AttSyntax => write!(f, "\t.att_syntax"),
        }
    }
}

impl<'a> Display for X86Program<'a> {
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