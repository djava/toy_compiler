use std::fmt::Display;

use super::shared::*;

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Register {
    // 64 bit registers
    rax,
    rbx,
    rcx,
    rdx,
    rsi,
    rdi,
    rbp,
    rsp,
    r8,
    r9,
    r10,
    r11,
    r12,
    r13,
    r14,
    r15,
    // 32 bit registers
    eax,
    ebx,
    ecx,
    edx,
    esi,
    edi,
    esp,
    ebp,
    r8d,
    r9d,
    r10d,
    r11d,
    r12d,
    r13d,
    r14d,
    r15d,
    // 16 bit registers
    ax,
    bx,
    cx,
    dx,
    si,
    di,
    sp,
    bp,
    r8w,
    r9w,
    r10w,
    r11w,
    r12w,
    r13w,
    r14w,
    r15w,
    // 8 bit registers
    al,
    ah,
    bl,
    bh,
    cl,
    ch,
    dl,
    dh,
    sil,
    dil,
    spl,
    bpl,
    r8b,
    r9b,
    r10b,
    r11b,
    r12b,
    r13b,
    r14b,
    r15b,
}

impl Register {
    pub fn to_quad(&self) -> Self {
        use Register::*;
        match self {
            rax | eax | ax | ah | al => rax,
            rbx | ebx | bx | bh | bl => rbx,
            rcx | ecx | cx | ch | cl => rcx,
            rdx | edx | dx | dh | dl => rdx,
            rsi | esi | si | sil => rsi,
            rdi | edi | di | dil => rdi,
            rsp | esp | sp | spl => rsp,
            rbp | ebp | bp | bpl => rbp,
            r8 | r8d | r8w | r8b => r8,
            r9 | r9d | r9w | r9b => r9,
            r10 | r10d | r10w | r10b => r10,
            r11 | r11d | r11w | r11b => r11,
            r12 | r12d | r12w | r12b => r12,
            r13 | r13d | r13w | r13b => r13,
            r14 | r14d | r14w | r14b => r14,
            r15 | r15d | r15w | r15b => r15,
        }
    }

    pub fn to_double(&self) -> Self {
        use Register::*;
        match self {
            rax | eax | ax | ah | al => eax,
            rbx | ebx | bx | bh | bl => ebx,
            rcx | ecx | cx | ch | cl => ecx,
            rdx | edx | dx | dh | dl => edx,
            rsi | esi | si | sil => esi,
            rdi | edi | di | dil => edi,
            rsp | esp | sp | spl => esp,
            rbp | ebp | bp | bpl => ebp,
            r8 | r8d | r8w | r8b => r8d,
            r9 | r9d | r9w | r9b => r9d,
            r10 | r10d | r10w | r10b => r10d,
            r11 | r11d | r11w | r11b => r11d,
            r12 | r12d | r12w | r12b => r12d,
            r13 | r13d | r13w | r13b => r13d,
            r14 | r14d | r14w | r14b => r14d,
            r15 | r15d | r15w | r15b => r15d,
        }
    }

    pub fn to_word(&self) -> Self {
        use Register::*;
        match self {
            rax | eax | ax | ah | al => ax,
            rbx | ebx | bx | bh | bl => bx,
            rcx | ecx | cx | ch | cl => cx,
            rdx | edx | dx | dh | dl => dx,
            rsi | esi | si | sil => si,
            rdi | edi | di | dil => di,
            rsp | esp | sp | spl => sp,
            rbp | ebp | bp | bpl => bp,
            r8 | r8d | r8w | r8b => r8w,
            r9 | r9d | r9w | r9b => r9w,
            r10 | r10d | r10w | r10b => r10w,
            r11 | r11d | r11w | r11b => r11w,
            r12 | r12d | r12w | r12b => r12w,
            r13 | r13d | r13w | r13b => r13w,
            r14 | r14d | r14w | r14b => r14w,
            r15 | r15d | r15w | r15b => r15w,
        }
    }

    pub fn to_byte_low(&self) -> Self {
        use Register::*;
        match self {
            rax | eax | ax | ah | al => al,
            rbx | ebx | bx | bh | bl => bl,
            rcx | ecx | cx | ch | cl => cl,
            rdx | edx | dx | dh | dl => dl,
            rsi | esi | si | sil => sil,
            rdi | edi | di | dil => dil,
            rsp | esp | sp | spl => spl,
            rbp | ebp | bp | bpl => bpl,
            r8 | r8d | r8w | r8b => r8b,
            r9 | r9d | r9w | r9b => r9b,
            r10 | r10d | r10w | r10b => r10b,
            r11 | r11d | r11w | r11b => r11b,
            r12 | r12d | r12w | r12b => r12b,
            r13 | r13d | r13w | r13b => r13b,
            r14 | r14d | r14w | r14b => r14b,
            r15 | r15d | r15w | r15b => r15b,
        }
    }

    pub fn try_to_byte_high(&self) -> Option<Self> {
        use Register::*;
        match self {
            rax | eax | ax => Some(ah),
            rbx | ebx | bx => Some(bh),
            rcx | ecx | cx => Some(ch),
            rdx | edx | dx => Some(dh),
            _ => None,
        }
    }

    pub fn width(&self) -> Width {
        use Register::*;
        match self {
            rax | rbx | rcx | rdx | rsi | rdi | rbp | rsp | r8 | r9 | r10 | r11 | r12 | r13
            | r14 | r15 => Width::Quad,
            eax | ebx | ecx | edx | esi | edi | esp | ebp | r8d | r9d | r10d | r11d | r12d
            | r13d | r14d | r15d => Width::Double,
            ax | bx | cx | dx | si | di | sp | bp | r8w | r9w | r10w | r11w | r12w | r13w
            | r14w | r15w => Width::Word,
            al | ah | bl | bh | cl | ch | dl | dh | sil | dil | spl | bpl | r8b | r9b | r10b
            | r11b | r12b | r13b | r14b | r15b => Width::Byte,
        }
    }

    pub fn convert_width(&self, width: &Width) -> Self {
        match width {
            Width::Quad => self.to_quad(),
            Width::Double => self.to_double(),
            Width::Word => self.to_word(),
            Width::Byte => self.to_byte_low(),
        }
    }

    pub fn is_high_byte(&self) -> bool {
        use Register::*;
        match self {
            ah | bh | ch | dh => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd)]
pub enum Width {
    Byte,
    Word,
    Double,
    Quad,
}

impl Width {
    pub fn mask(&self) -> i64 {
        match self {
            Width::Quad => !0,
            Width::Double => 0x0000_0000_FFFF_FFFFi64,
            Width::Word => 0x0000_0000_0000_FFFFi64,
            Width::Byte => 0x0000_0000_0000_00FFi64,
        }
    }

    pub fn bytes(&self) -> usize {
        match self {
            Width::Quad => 8,
            Width::Double => 4,
            Width::Word => 2,
            Width::Byte => 1,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ArgValue {
    Immediate(i64),
    Reg(Register),
    Deref(Register, i32),
    Variable(Identifier),
    Global(Identifier),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Arg {
    pub value: ArgValue,
    pub width: Width,
}

impl Arg {
    pub fn new_imm(n: i64) -> Self {
        Self {
            value: ArgValue::Immediate(n),
            width: Width::Quad, // TODO: pass width
        }
    }

    pub fn new_reg(r: Register) -> Self {
        Self {
            value: ArgValue::Reg(r),
            width: r.width(),
        }
    }

    pub fn new_deref(r: Register, offset: i32) -> Self {
        Self {
            value: ArgValue::Deref(r, offset),
            width: Width::Quad, // TODO: pass width
        }
    }

    pub fn new_variable(id: Identifier) -> Self {
        Self {
            value: ArgValue::Variable(id),
            width: Width::Quad, // TODO: pass width
        }
    }

    pub fn new_global(id: Identifier) -> Self {
        Self {
            value: ArgValue::Global(id),
            width: Width::Quad, // TODO: pass width
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Comparison {
    Equals,
    NotEquals,
    Less,
    LessEquals,
    Greater,
    GreaterEquals,
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Instr {
    add(Arg, Arg),
    sub(Arg, Arg),
    neg(Arg),
    mov(Arg, Arg),
    push(Arg),
    pop(Arg),
    call(Arg, u16),
    ret,
    xor(Arg, Arg),
    cmp(Arg, Arg),
    set(Comparison, Arg),
    movzx(Arg, Arg),
    movsx(Arg, Arg),
    jmp(Identifier),
    jmpcc(Comparison, Identifier),
    sar(Arg, Arg),
    sal(Arg, Arg),
    and(Arg, Arg),
    imul(Arg, Arg),
    lea(Arg, Arg),
    call_ind(Arg, u16),
    jmp_tail(Arg, u16),
    idiv(Arg),
    cqto,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Directive {
    Label(Identifier),
    Globl(Identifier),
    AttSyntax,
    Align(u8),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Block {
    pub label: Identifier,
    pub instrs: Vec<Instr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub header: Vec<Directive>,
    pub name: Identifier,
    pub blocks: Vec<Block>,
    pub entry_block: Identifier,
    pub exit_block: Identifier,
    pub stack_size: usize,
    pub gc_stack_size: usize,
    pub types: TypeEnv,
    pub callee_saved_used: Vec<Register>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct X86Program {
    pub header: Vec<Directive>,
    pub functions: Vec<Function>,
}

impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{:?}", self)
    }
}

impl Display for Arg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.value {
            ArgValue::Immediate(val) => write!(f, "${}", val),
            ArgValue::Reg(reg) => reg.fmt(f),
            ArgValue::Deref(reg, offset) => write!(f, "{offset}({reg})"),
            ArgValue::Variable(id) => match id {
                Identifier::Global(name) => write!(f, "{name}"),
                Identifier::Ephemeral(id) => write!(f, "@EE#{id}"),
                Identifier::Local(id, func) => match &**func {
                    Identifier::Ephemeral(func_num) => write!(f, "@EE#{func_num}::{id}"),
                    Identifier::Global(func_name) => write!(f, "@{func_name}::{id}"),
                    Identifier::Local(func_name, func_owner) => {
                        write!(f, "@{{{func_owner:?}::{func_name}}}::{id}")
                    }
                },
            },
            ArgValue::Global(id) => match id {
                Identifier::Global(name) => write!(f, "{name}(%rip)"),
                Identifier::Ephemeral(id) => write!(f, "__EE_{id}(%rip)"),
                Identifier::Local(..) => panic!("A local cannot be a global"),
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
        Identifier::Global(name) => name.to_string(),
        Identifier::Ephemeral(id) => format!(".EE_{id}"),
        Identifier::Local(id, owner) => format!("{}{id}", fmt_label(owner)),
    }
}

fn fmt_arg_for_jmp_call(arg: &Arg) -> String {
    match &arg.value {
        ArgValue::Global(id) => fmt_label(id),
        ArgValue::Reg(reg) => format!("*{reg}"),
        _ => panic!("Invalid arg for jmp/callq: {arg}"),
    }
}

impl Display for Instr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instr::add(arg, arg1) => write!(f, "addq {arg}, {arg1}"),
            Instr::sub(arg, arg1) => write!(f, "subq {arg}, {arg1}"),
            Instr::neg(arg) => write!(f, "negq {arg}"),
            Instr::mov(arg, arg1) => write!(f, "movq {arg}, {arg1}"),
            Instr::push(arg) => write!(f, "pushq {arg}"),
            Instr::pop(arg) => write!(f, "popq {arg}"),
            Instr::call(arg, _) => write!(f, "callq {}", fmt_arg_for_jmp_call(arg)),
            Instr::ret => write!(f, "retq"),
            Instr::xor(arg, arg1) => write!(f, "xorq {arg}, {arg1}"),
            Instr::cmp(arg, arg1) => write!(f, "cmpq {arg}, {arg1}"),
            Instr::set(cmp, arg) => write!(f, "set{cmp} {arg}"),
            Instr::movzx(arg, arg1) => write!(f, "movzxq {arg}, {arg1}"),
            Instr::jmp(label) => write!(f, "jmp {}", fmt_label(label)),
            Instr::jmpcc(cmp, label) => write!(f, "j{cmp} {}", fmt_label(label)),
            Instr::sar(arg, arg1) => write!(f, "sarq {arg}, {arg1}"),
            Instr::sal(arg, arg1) => write!(f, "salq {arg}, {arg1}"),
            Instr::and(arg, arg1) => write!(f, "andq {arg}, {arg1}"),
            Instr::imul(arg, arg1) => write!(f, "imulq {arg}, {arg1}"),
            Instr::lea(arg, arg1) => write!(f, "leaq {arg}, {arg1}"),
            Instr::call_ind(arg, _) => write!(f, "callq {}", fmt_arg_for_jmp_call(arg)),
            Instr::jmp_tail(arg, _) => write!(f, "jmp {}", fmt_arg_for_jmp_call(arg)),
            Instr::idiv(arg) => write!(f, "idivq {arg}"),
            Instr::cqto => write!(f, "cqto"),
        }
    }
}

impl Display for Directive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Label(label) => write!(f, "{}:", fmt_label(label)),
            Self::Globl(label) => write!(f, "\t.globl {}", fmt_label(label)),
            Self::AttSyntax => write!(f, "\t.att_syntax"),
            Self::Align(n) => write!(f, "\t.align {n}"),
        }
    }
}

impl Display for X86Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for dir in &self.header {
            writeln!(f, "{dir}")?;
        }
        for func in &self.functions {
            for dir in &func.header {
                writeln!(f, "{dir}")?;
            }
            for block in &func.blocks {
                writeln!(f, "{}", Directive::Label(block.label.clone()))?;
                for i in block.instrs.iter() {
                    writeln!(f, "\t{i}")?;
                }
            }
            writeln!(f)?;
        }
        writeln!(f, "\t.section .note.GNU-stack,\"\",@progbits")?;
        Ok(())
    }
}
