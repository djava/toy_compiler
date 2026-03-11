use crate::{
    passes::X86Pass,
    syntax_trees::{x86::*},
};

/// `ResolveWidth` Pass
///
/// Reconciles mismatched argument widths within each instruction by
/// widening narrower arguments to match their partner and inserting
/// sign- or zero-extension instructions (`movsx`/`movzx`) when
/// required. Also truncates immediate values to fit their target width.
/// This is the final pass before optional jump cleanup, and must run
/// after `PreludeConclusion` so that all physical register assignments
/// are in place.
///
/// It is mandatory to run this pass
///
/// Pre-conditions:
/// - `RegisterAllocation` (all `Arg::Variable` resolved)
/// - `PreludeConclusion` (frame fully constructed)
///
/// Post-conditions:
/// - All instruction operands have matching widths
/// - Any required sign- or zero-extension is explicit
#[derive(Debug)]
pub struct ResolveWidth;

impl X86Pass for ResolveWidth {
    fn run_pass(self, mut m: X86Program) -> X86Program {
        for f in m.functions.iter_mut() {
            for b in f.blocks.iter_mut() {
                let old_instrs = std::mem::replace(&mut b.instrs, vec![]);
                for i in old_instrs {
                    resolve_instr_width(i, &mut b.instrs);
                }
            }
        }

        m
    }
}

macro_rules! binary_instr {
    ($instr:expr, $ext:expr, $s:expr, $d:expr, $new_instrs:expr) => {
        {
            let final_width = std::cmp::max($s.width, $d.width);
            let s_converted = convert_arg_width($s, final_width, $ext, $new_instrs);
            let d_converted = convert_arg_width($d, final_width, $ext, $new_instrs);
    
            $new_instrs.push($instr(s_converted, d_converted));
        }
    }
}

macro_rules! unary_instr {
    ($instr:expr, $arg:expr, $new_instrs:expr) => {
        {
            let width = $arg.width;
            let arg_converted = convert_arg_width($arg, width, Extension::None, $new_instrs);
            $new_instrs.push($instr(arg_converted));
        }
    };
}

fn resolve_instr_width(i: Instr, new_instrs: &mut Vec<Instr>) {
    match i {
        Instr::add(s, d) => binary_instr!(Instr::add, Extension::Signed, s, d, new_instrs),
        Instr::sub(s, d) => binary_instr!(Instr::sub, Extension::Signed, s, d, new_instrs),
        Instr::neg(arg) => unary_instr!(Instr::neg, arg, new_instrs),
        Instr::mov(s, d) => binary_instr!(Instr::mov, Extension::Zero, s, d, new_instrs),
        Instr::push(arg) => unary_instr!(Instr::push, arg, new_instrs),
        Instr::pop(arg) => unary_instr!(Instr::pop, arg, new_instrs),
        Instr::xor(s, d) => binary_instr!(Instr::xor, Extension::Zero, s, d, new_instrs),
        Instr::cmp(s, d) => binary_instr!(Instr::cmp, Extension::Zero, s, d, new_instrs),
        Instr::sar(s, d) => binary_instr!(Instr::sar, Extension::Signed, s, d, new_instrs),
        Instr::sal(s, d) => binary_instr!(Instr::sal, Extension::Signed, s, d, new_instrs),
        Instr::and(s, d) => binary_instr!(Instr::and, Extension::Signed, s, d, new_instrs),
        Instr::imul(s, d) => binary_instr!(Instr::imul, Extension::Signed, s, d, new_instrs),
        Instr::lea(s, d) => binary_instr!(Instr::lea, Extension::Signed, s, d, new_instrs),
        Instr::idiv(arg) => unary_instr!(Instr::idiv, arg, new_instrs),
        
        Instr::call(arg, arity) => {
            let width = arg.width;
            let arg_converted = convert_arg_width(arg, width, Extension::None, new_instrs);
            new_instrs.push(Instr::call(arg_converted, arity));
        }
        Instr::call_ind(arg, arity) => {
            let width = arg.width;
            let arg_converted = convert_arg_width(arg, width, Extension::None, new_instrs);
            new_instrs.push(Instr::call_ind(arg_converted, arity));
        }
        Instr::set(comparison, arg) => {
            let arg_converted = convert_arg_width(arg, Width::Byte, Extension::None, new_instrs);
            new_instrs.push(Instr::set(comparison, arg_converted));
        },
        Instr::movsx(s, d) => {
            // Both args are not the same width
            let s_width = s.width;
            let s_converted = convert_arg_width(s, s_width, Extension::None, new_instrs);
            let d_width = d.width;
            let d_converted = convert_arg_width(d, d_width, Extension::None, new_instrs);
    
            new_instrs.push(Instr::movsx(s_converted, d_converted));
        },
        Instr::movzx(s, d) => {
            // Both args are not the same width
            let s_width = s.width;
            let s_converted = convert_arg_width(s, s_width, Extension::None, new_instrs);
            let d_width = d.width;
            let d_converted = convert_arg_width(d, d_width, Extension::None, new_instrs);
    
            new_instrs.push(Instr::movzx(s_converted, d_converted));
        },
        Instr::cqto => {new_instrs.push(Instr::cqto)},  // TODO: width in cqto?
        Instr::ret => { new_instrs.push(i); },
        Instr::jmp(_) => { new_instrs.push(i); },
        Instr::jmpcc(_, _) =>  { new_instrs.push(i); },
        Instr::jmp_tail(_, _) =>  { new_instrs.push(i); },
    }
}

enum Extension {
    Signed,
    Zero,
    None,
}

fn convert_arg_width(
    mut arg: Arg,
    width: Width,
    extension: Extension,
    new_instrs: &mut Vec<Instr>,
) -> Arg {
    match &mut arg.value {
        ArgValue::Immediate(imm) => {
            *imm &= width.mask();
        }
        ArgValue::Reg(reg) => {
            let new_reg = reg.convert_width(width);
            if width > reg.width() {
                // Requires explicit extension instruction
                let extended_arg = Arg::new_reg(new_reg);
                match extension {
                    Extension::Zero => {
                        // Double->Quad zero extension happens implicitly
                        if !(reg.width() == Width::Double && width == Width::Quad) {
                            new_instrs.push(Instr::movzx(Arg::new_reg(*reg), extended_arg));
                        }
                    }
                    Extension::Signed => {
                        new_instrs.push(Instr::movsx(Arg::new_reg(*reg), extended_arg));
                    }
                    Extension::None => {
                        // Implies that there isn't actually any
                        // width coercion happening, ie unary
                    }
                }
            }
            *reg = new_reg;
        }
        ArgValue::Deref(_, _) | ArgValue::Variable(_) | ArgValue::Global(_) => {}
    }
    arg.width = width;
    arg
}
