use crate::syntax_trees::{shared::*, x86::*};
use std::sync::LazyLock;
use std::{collections::HashMap, fmt::Display};

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

fn get_instr_mnemonic(instr: &Instr) -> String {
    const SUFFIX_MAP: LazyLock<HashMap<Width, &str>> = LazyLock::new(|| {
        HashMap::from([
            (Width::Byte, "b"),
            (Width::Word, "w"),
            (Width::Double, "d"),
            (Width::Quad, "q"),
        ])
    });

    use Instr as I;
    let width = match instr {
        I::add(arg, _)
        | I::sub(arg, _)
        | I::mov(arg, _)
        | I::xor(arg, _)
        | I::cmp(arg, _)
        | I::sar(arg, _)
        | I::sal(arg, _)
        | I::and(arg, _)
        | I::imul(arg, _)
        | I::lea(arg, _)
        | I::neg(arg)
        | I::push(arg)
        | I::pop(arg)
        | I::call_ind(arg, _)
        | I::idiv(arg)
        | I::jmp_tail(arg, _)
        | I::set(_, arg)
        | I::call(arg, _) => Some(arg.width),

        I::ret => Some(Width::Quad),
        I::cqto => Some(Width::Quad),
        I::jmp(_) => None,
        I::jmpcc(_, _) => None,
        I::movzx(_, _) => None,
        I::movsx(_, _) => None,
    };

    let width_suffix = width.map_or("", |w| SUFFIX_MAP[&w]);

    match instr {
        I::set(cc, _) => format!("set{cc}"),
        I::jmpcc(cc, _) => format!("j{cc}"),
        I::call_ind(..) => format!("call{width_suffix}"),
        I::jmp_tail(..) => format!("jmp"),
        I::cqto => match width {
            Some(Width::Quad) => format!("cqto"),
            Some(Width::Double) => format!("cdtq"),
            Some(Width::Word) => format!("cwtd"),
            Some(Width::Byte) | None => panic!("Invalid width for Instr::cqto: {width:?}"),
        },
        I::movzx(s, d) => {
            format!("movz{}{}", SUFFIX_MAP[&s.width], SUFFIX_MAP[&d.width])
        }
        I::movsx(s, d) => {
            format!("movs{}{}", SUFFIX_MAP[&s.width], SUFFIX_MAP[&d.width])
        }
        I::add(..) => format!("add{width_suffix}"),
        I::sub(..) => format!("sub{width_suffix}"),
        I::neg(..) => format!("neg{width_suffix}"),
        I::mov(..) => format!("mov{width_suffix}"),
        I::push(..) => format!("push{width_suffix}"),
        I::pop(..) => format!("pop{width_suffix}"),
        I::call(..) => format!("call{width_suffix}"),
        I::xor(..) => format!("xor{width_suffix}"),
        I::cmp(..) => format!("cmp{width_suffix}"),
        I::jmp(..) => format!("jmp{width_suffix}"),
        I::sar(..) => format!("sar{width_suffix}"),
        I::sal(..) => format!("sal{width_suffix}"),
        I::and(..) => format!("and{width_suffix}"),
        I::imul(..) => format!("imul{width_suffix}"),
        I::lea(..) => format!("lea{width_suffix}"),
        I::idiv(..) => format!("idiv{width_suffix}"),
        I::ret => format!("ret{width_suffix}"),
    }
}

impl Display for Instr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instr::add(arg, arg1)
            | Instr::sub(arg, arg1)
            | Instr::mov(arg, arg1)
            | Instr::xor(arg, arg1)
            | Instr::cmp(arg, arg1)
            | Instr::movzx(arg, arg1)
            | Instr::movsx(arg, arg1)
            | Instr::sal(arg, arg1)
            | Instr::and(arg, arg1)
            | Instr::imul(arg, arg1)
            | Instr::lea(arg, arg1)
            | Instr::sar(arg, arg1) => write!(f, "{} {arg}, {arg1}", get_instr_mnemonic(self)),

            Instr::neg(arg)
            | Instr::push(arg)
            | Instr::pop(arg)
            | Instr::idiv(arg)
            | Instr::set(_, arg) => {
                write!(f, "{} {arg}", get_instr_mnemonic(self))
            }

            Instr::call(arg, _) | Instr::call_ind(arg, _) | Instr::jmp_tail(arg, _) => write!(
                f,
                "{} {}",
                get_instr_mnemonic(self),
                fmt_arg_for_jmp_call(arg)
            ),

            Instr::jmp(label) | Instr::jmpcc(_, label) => {
                write!(f, "{} {}", get_instr_mnemonic(self), fmt_label(label))
            }

            Instr::ret | Instr::cqto => write!(f, "{}", get_instr_mnemonic(self)),
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
