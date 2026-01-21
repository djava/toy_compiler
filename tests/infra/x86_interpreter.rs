use std::collections::VecDeque;

use crate::infra::ValueEnv;
use cs4999_compiler::{ast::Value, x86_ast::*};

struct X86Env<'a> {
    vars: ValueEnv<'a>,
    regs: [i64; 16],
    memory: [u8; 2048],
}

impl<'a> X86Env<'a> {
    fn new() -> Self {
        let mut ret = Self {
            vars: ValueEnv::new(),
            regs: [0; 16],
            memory: [0; 2048],
        };

        ret.regs[Register::rsp as usize] = 2048;
        ret.regs[Register::rbp as usize] = 2048;

        ret
    }

    fn write_arg(&mut self, arg: &Arg<'a>, value: i64) {
        match arg {
            Arg::Reg(n) => {
                self.regs[*n as usize] = value;
            }
            Arg::Variable(id) => {
                self.vars.insert(*id, Value::I64(value));
            }
            Arg::Deref(reg, offset) => {
                let base = self.regs[*reg as usize];
                let addr: usize = (base + (*offset as i64)).try_into().unwrap();

                let bytes = value.to_le_bytes();
                for (idx, byte) in bytes.iter().enumerate() {
                    self.memory[addr + idx] = *byte;
                }
            }
            Arg::Immediate(_) => panic!("Can't write to an intermediate"),
        }
    }

    fn read_arg(&self, arg: &Arg) -> i64 {
        match arg {
            Arg::Reg(n) => self.regs[*n as usize],
            Arg::Variable(id) => *self
                .vars
                .get(id)
                .map(|Value::I64(val)| val)
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
        }
    }
}

fn execute_runtime_calls(
    label: &str,
    inputs: &mut VecDeque<i64>,
    outputs: &mut VecDeque<i64>,
    env: &mut X86Env,
) -> bool {
    if label == "print_int" {
        let int = env.read_arg(&Arg::Reg(Register::rdi));
        outputs.push_back(int);
        return true;
    } else if label == "read_int" {
        let int = inputs.pop_front().expect("Overflowed input values");
        env.write_arg(&Arg::Reg(Register::rax), int);
        return true;
    } else {
        // No match found, must be another call
        return false;
    }
}

fn run_instr<'a>(
    instr: &Instr<'a>,
    inputs: &mut VecDeque<i64>,
    outputs: &mut VecDeque<i64>,
    env: &mut X86Env<'a>,
) {
    match instr {
        Instr::addq(s, d) => {
            env.write_arg(d, env.read_arg(s) + env.read_arg(d));
        }
        Instr::subq(s, d) => {
            env.write_arg(d, env.read_arg(d) - env.read_arg(s));
        }
        Instr::negq(d) => {
            env.write_arg(d, -env.read_arg(d));
        }
        Instr::movq(s, d) => {
            env.write_arg(d, env.read_arg(s));
        }
        Instr::pushq(s) => {
            env.write_arg(
                &Arg::Reg(Register::rsp),
                env.read_arg(&Arg::Reg(Register::rsp)) - 8,
            );
            env.write_arg(&Arg::Deref(Register::rsp, 0), env.read_arg(s));
        }
        Instr::popq(d) => {
            env.write_arg(d, env.read_arg(&Arg::Deref(Register::rsp, 0)));
            env.write_arg(
                &Arg::Reg(Register::rsp),
                env.read_arg(&Arg::Reg(Register::rsp)) + 8,
            );
        }
        Instr::callq(label, _num_args) => {
            if !execute_runtime_calls(label, inputs, outputs, env) {
                unimplemented!("User-defined function calls not implemented");
            }
        }
        Instr::retq => {
            unimplemented!("User-defined function calls not implemented");
        }
    }
}

pub fn interpret_x86(m: &X86Program, inputs: &mut VecDeque<i64>, outputs: &mut VecDeque<i64>) {
    let mut env = X86Env::new();

    let main_instrs = &m
        .functions
        .iter()
        .find(|(d, _)| d == &Directive::Label("main"))
        .expect("Didn't find a main function").1;
    // TODO: This is not remotely sufficient for a program with actual
    // control flow - Need to follow %rip instead...
    for i in main_instrs {
        run_instr(i, inputs, outputs, &mut env);
    }
}
