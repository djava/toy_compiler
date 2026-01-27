use std::collections::VecDeque;
use std::sync::Arc;

use crate::infra::ValueEnv;
use cs4999_compiler::{ast::Value, ir::Identifier, x86_ast::*};

#[derive(Debug, Default)]
struct Eflags {
    pub zero: bool,     // ZF
    pub carry: bool,    // CF
    pub negative: bool, // SF
    pub overflow: bool, // OF
}

#[derive(Debug)]
struct X86Env {
    vars: ValueEnv,
    regs: [i64; 16],
    memory: [u8; 2048],
    eflags: Eflags,
}

impl X86Env {
    fn new() -> Self {
        let mut ret = Self {
            vars: ValueEnv::new(),
            regs: [0; 16],
            memory: [0; 2048],
            eflags: Eflags::default(),
        };

        ret.regs[Register::rsp as usize] = 2048;
        ret.regs[Register::rbp as usize] = 2048;

        ret
    }

    fn write_high_byte(&mut self, reg: Register, value: i64) {
        self.regs[reg as usize] &= 0xFFFF_FFFF_FFFF_00FFu64 as i64; // Zero the "Ah" byte
        self.regs[Register::rax as usize] |= (value & 0x0000_0000_0000_00FFu64 as i64) << 8;
    }

    fn write_low_byte(&mut self, reg: Register, value: i64) {
        self.regs[reg as usize] &= 0xFFFF_FFFF_FFFF_FF00u64 as i64; // Zero the "Ah" byte
        self.regs[Register::rax as usize] |= value & 0x0000_0000_0000_00FFu64 as i64;
    }

    fn write_arg(&mut self, arg: &Arg, value: i64) {
        match arg {
            Arg::Reg(n) => {
                self.regs[*n as usize] = value;
            }
            Arg::Variable(id) => {
                self.vars.insert(id.clone(), Value::I64(value));
            }
            Arg::Deref(reg, offset) => {
                let base = self.regs[*reg as usize];
                let addr: usize = (base + (*offset as i64)).try_into().unwrap();

                let bytes = value.to_le_bytes();
                for (idx, byte) in bytes.iter().enumerate() {
                    self.memory[addr + idx] = *byte;
                }
            }
            Arg::ByteReg(reg) => match reg {
                ByteReg::ah => {
                    self.write_high_byte(Register::rax, value);
                }
                ByteReg::al => {
                    self.write_low_byte(Register::rax, value);
                }
                ByteReg::bh => {
                    self.write_high_byte(Register::rbx, value);
                }
                ByteReg::bl => {
                    self.write_low_byte(Register::rbx, value);
                }
                ByteReg::ch => {
                    self.write_high_byte(Register::rcx, value);
                }
                ByteReg::cl => {
                    self.write_low_byte(Register::rcx, value);
                }
                ByteReg::dh => {
                    self.write_high_byte(Register::rdx, value);
                }
                ByteReg::dl => {
                    self.write_low_byte(Register::rdx, value);
                }
            },
            Arg::Immediate(_) => panic!("Can't write to an intermediate"),
        }
    }

    fn read_arg(&self, arg: &Arg) -> i64 {
        match arg {
            Arg::Reg(n) => self.regs[*n as usize],
            Arg::Variable(id) => self
                .vars
                .get(id)
                .map(|val| i64::from(val))
                .expect(format!("Unknown x86var identifier: {id:?}").as_str()),
            Arg::Deref(reg, offset) => {
                let base = self.regs[*reg as usize];
                let addr: usize = (base + (*offset as i64)).try_into().unwrap();

                let mut bytes = [0xFF; 8];
                for (idx, byte) in bytes.iter_mut().enumerate() {
                    *byte = self.memory[addr + idx];
                }
                i64::from_le_bytes(bytes)
            }
            Arg::Immediate(val) => *val as i64,
            Arg::ByteReg(reg) => match reg {
                ByteReg::ah => {
                    (self.regs[Register::rax as usize] & 0x0000_0000_0000_FF00u64 as i64) >> 8
                }
                ByteReg::al => {
                    (self.regs[Register::rax as usize] & 0x0000_0000_0000_00FFu64 as i64) >> 0
                }
                ByteReg::bh => {
                    (self.regs[Register::rbx as usize] & 0x0000_0000_0000_FF00u64 as i64) >> 8
                }
                ByteReg::bl => {
                    (self.regs[Register::rbx as usize] & 0x0000_0000_0000_00FFu64 as i64) >> 0
                }
                ByteReg::ch => {
                    (self.regs[Register::rcx as usize] & 0x0000_0000_0000_FF00u64 as i64) >> 8
                }
                ByteReg::cl => {
                    (self.regs[Register::rcx as usize] & 0x0000_0000_0000_00FFu64 as i64) >> 0
                }
                ByteReg::dh => {
                    (self.regs[Register::rdx as usize] & 0x0000_0000_0000_FF00u64 as i64) >> 8
                }
                ByteReg::dl => {
                    (self.regs[Register::rdx as usize] & 0x0000_0000_0000_00FFu64 as i64) >> 0
                }
            },
        }
    }

    fn compare(&mut self, s: i64, d: i64) {
        let val = d.wrapping_sub(s);
        self.eflags = Eflags {
            zero: val == 0,
            negative: val < 0,
            carry: (d as u64) < (s as u64),
            overflow: ((d ^ s) < 0) && ((d ^ val) < 0),
        }
    }

    fn get_comparison(&self, cc: &Comparison) -> bool {
        match cc {
            Comparison::Equals => self.eflags.zero,
            Comparison::NotEquals => !self.eflags.zero,
            Comparison::Less => self.eflags.negative != self.eflags.overflow,
            Comparison::LessEquals => {
                (self.eflags.negative != self.eflags.overflow) || self.eflags.zero
            }
            Comparison::Greater => {
                !(self.eflags.negative != self.eflags.overflow) && !self.eflags.zero
            }
            Comparison::GreaterEquals => !(self.eflags.negative != self.eflags.overflow),
        }
    }
}

fn execute_runtime_calls(
    label: &Identifier,
    inputs: &mut VecDeque<i64>,
    outputs: &mut VecDeque<i64>,
    env: &mut X86Env,
) -> bool {
    if label == &Identifier::Named(Arc::from("print_int")) {
        let int = env.read_arg(&Arg::Reg(Register::rdi));
        outputs.push_back(int);
        return true;
    } else if label == &Identifier::Named(Arc::from("read_int")) {
        let int = inputs.pop_front().expect("Overflowed input values");
        env.write_arg(&Arg::Reg(Register::rax), int);
        return true;
    } else {
        // No match found, must be another call
        return false;
    }
}

enum Continuation {
    Next,
    Jump(Identifier),
    Exit,
}

fn run_instr(
    instr: &Instr,
    inputs: &mut VecDeque<i64>,
    outputs: &mut VecDeque<i64>,
    env: &mut X86Env,
) -> Continuation {
    match instr {
        Instr::addq(s, d) => {
            env.write_arg(d, env.read_arg(s) + env.read_arg(d));
            Continuation::Next
        }
        Instr::subq(s, d) => {
            env.write_arg(d, env.read_arg(d) - env.read_arg(s));
            Continuation::Next
        }
        Instr::negq(d) => {
            env.write_arg(d, -env.read_arg(d));
            Continuation::Next
        }
        Instr::movq(s, d) => {
            env.write_arg(d, env.read_arg(s));
            Continuation::Next
        }
        Instr::pushq(s) => {
            env.write_arg(
                &Arg::Reg(Register::rsp),
                env.read_arg(&Arg::Reg(Register::rsp)) - 8,
            );
            env.write_arg(&Arg::Deref(Register::rsp, 0), env.read_arg(s));
            Continuation::Next
        }
        Instr::popq(d) => {
            env.write_arg(d, env.read_arg(&Arg::Deref(Register::rsp, 0)));
            env.write_arg(
                &Arg::Reg(Register::rsp),
                env.read_arg(&Arg::Reg(Register::rsp)) + 8,
            );
            Continuation::Next
        }
        Instr::callq(label, _num_args) => {
            if !execute_runtime_calls(label, inputs, outputs, env) {
                unimplemented!("User-defined function calls not implemented");
            }
            Continuation::Next
        }
        Instr::retq => Continuation::Exit,
        Instr::xorq(s, d) => {
            env.write_arg(d, env.read_arg(s) ^ env.read_arg(d));
            Continuation::Next
        }
        Instr::cmpq(s, d) => {
            env.compare(env.read_arg(s), env.read_arg(d));
            Continuation::Next
        }
        Instr::set(cc, d) => {
            env.write_arg(&Arg::ByteReg(*d), env.get_comparison(cc) as i64);
            Continuation::Next
        }
        Instr::movzbq(s, d) => {
            assert!(matches!(s, Arg::ByteReg(_)));
            assert!(matches!(d, Arg::Reg(_)));
            env.write_arg(d, env.read_arg(s));
            Continuation::Next
        }
        Instr::jmp(label) => Continuation::Jump(label.clone()),
        Instr::jmpcc(cc, label) => {
            if env.get_comparison(cc) {
                Continuation::Jump(label.clone())
            } else {
                Continuation::Next
            }
        }
    }
}

pub fn interpret_x86(m: &X86Program, inputs: &mut VecDeque<i64>, outputs: &mut VecDeque<i64>) {
    let mut env = X86Env::new();

    let main_instrs = &m
        .blocks
        .iter()
        .find(|block| block.label == Directive::Label(Identifier::Named(Arc::from("main"))))
        .unwrap();

    let mut curr_instr_iter = main_instrs.instrs.iter();
    loop {
        let curr_instr = curr_instr_iter
            .next()
            .unwrap_or_else(|| panic!("Ran out of instrs without a non-Next continuation"));

        match run_instr(curr_instr, inputs, outputs, &mut env) {
            Continuation::Next => {}
            Continuation::Jump(label) => {
                let new_block = &m
                    .blocks
                    .iter()
                    .find(|b| b.label == Directive::Label(label.clone()))
                    .unwrap_or_else(|| panic!("Couldn't find label: {label:?}"));
                curr_instr_iter = new_block.instrs.iter();
            }
            Continuation::Exit => { break; }
        }
    }
}
