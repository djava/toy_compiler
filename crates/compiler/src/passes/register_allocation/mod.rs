mod dataflow_analysis;
mod graph_coloring;

use std::collections::HashMap;
use std::mem::size_of;

use crate::{
    ast::{AssignDest, Identifier, TypeEnv, ValueType},
    passes::{X86Pass, register_allocation::graph_coloring::COLOR_TO_REG_STORAGE},
    x86_ast,
};
use dataflow_analysis::LivenessMap;
use graph_coloring::color_location_graph;
use x86_ast::*;

pub struct RegisterAllocation;

impl X86Pass for RegisterAllocation {
    fn run_pass(self, mut m: X86Program) -> X86Program {
        let liveness = LivenessMap::from_program(&m);

        let AllocateStorageResult {
            id_to_storage,
            stack_size,
            gc_stack_size,
        } = allocate_storage(&liveness, &m.types);

        let callee_saved_used: Vec<_> = id_to_storage
            .values()
            .filter(|stg| matches!(stg, Storage::Reg(reg) if CALLEE_SAVED_REGISTERS.contains(reg)))
            .collect();
        let callee_offset = -((callee_saved_used.len() * size_of::<i64>()) as i32);

        for b in m.blocks.iter_mut() {
            run_for_block(&mut b.instrs, &id_to_storage, callee_offset);
        }

        if let Some(user_entry) = m
            .blocks
            .iter_mut()
            .find(|b| b.label == Directive::Label(Identifier::from("user_entry")))
        {
            let callee_pushqs = callee_saved_used.iter().filter_map(|loc| {
                if let Storage::Reg(reg) = loc {
                    Some(Instr::pushq(Arg::Reg(*reg)))
                } else {
                    None
                }
            });

            user_entry.instrs.splice(0..0, callee_pushqs);
        }

        if let Some(user_exit) = m
            .blocks
            .iter_mut()
            .find(|b| b.label == Directive::Label(Identifier::from("user_exit")))
        {
            let callee_popqs = callee_saved_used.iter().rev().filter_map(|loc| {
                if let Storage::Reg(reg) = loc {
                    Some(Instr::popq(Arg::Reg(*reg)))
                } else {
                    None
                }
            });

            let len = user_exit.instrs.len();
            user_exit.instrs.splice((len - 2)..=(len - 2), callee_popqs);
        }

        let used_stack = -callee_offset + stack_size;
        let aligned_stack_size = if used_stack % STACK_ALIGNMENT == 0 {
            used_stack
        } else {
            used_stack + (STACK_ALIGNMENT - (used_stack % STACK_ALIGNMENT))
        };

        m.stack_size = aligned_stack_size as usize;
        m.gc_stack_size = gc_stack_size as usize;

        m
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
enum Location {
    Id(Identifier),
    Reg(Register),
}

impl Location {
    fn try_from_arg(arg: &Arg) -> Option<Self> {
        match arg {
            Arg::Deref(reg, _) => Some(Location::Reg(*reg)),
            Arg::Variable(id) => Some(Location::Id(id.clone())),
            Arg::Reg(reg) => Some(Location::Reg(*reg)),
            Arg::ByteReg(bytereg) => Some(Location::Reg(bytereg.to_underlying())),
            Arg::Immediate(_) => None,
            Arg::Global(_) => None,
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
enum Storage {
    Stack(i32),
    GCStack(i32),
    Reg(Register),
}

impl Storage {
    pub fn to_arg(self) -> Arg {
        match self {
            Storage::Stack(offset) => Arg::Deref(Register::rbp, offset),
            Storage::GCStack(offset) => Arg::Deref(Register::r15, offset),
            Storage::Reg(reg) => Arg::Reg(reg),
        }
    }

    pub fn with_stack_offset(self, off: i32) -> Self {
        if let Storage::Stack(offset) = self {
            Storage::Stack(offset + off)
        } else {
            self
        }
    }
}

fn run_for_block(
    instrs: &mut Vec<Instr>,
    id_to_storage: &HashMap<Identifier, Storage>,
    stack_offset: i32,
) {
    for i in instrs.iter_mut() {
        match i {
            Instr::addq(s, d)
            | Instr::subq(s, d)
            | Instr::movq(s, d)
            | Instr::xorq(s, d)
            | Instr::cmpq(s, d)
            | Instr::andq(s, d)
            | Instr::sarq(s, d)
            | Instr::salq(s, d) => {
                replace_arg_with_allocated(s, id_to_storage, stack_offset);
                replace_arg_with_allocated(d, id_to_storage, stack_offset);
            }
            Instr::negq(a) | Instr::pushq(a) | Instr::popq(a) | Instr::movzbq(_, a) => {
                replace_arg_with_allocated(a, id_to_storage, stack_offset);
            }
            Instr::callq(_, _)
            | Instr::retq
            | Instr::jmpcc(_, _)
            | Instr::jmp(_)
            | Instr::set(_, _) => {
                // No real args to replace
            }
        }
    }
}

struct AllocateStorageResult {
    id_to_storage: HashMap<Identifier, Storage>,
    stack_size: i32,
    gc_stack_size: i32,
}

fn allocate_storage<'a>(liveness: &'a LivenessMap, types: &TypeEnv) -> AllocateStorageResult {
    let mut curr_stack_offset = 0i32;
    let mut curr_gc_stack_offset = 0i32;

    let graph_colors = color_location_graph(&liveness.interference_graph);
    let mut id_to_storage = HashMap::new();
    let mut color_to_storage = HashMap::from(COLOR_TO_REG_STORAGE);

    for (location, color) in &graph_colors {
        if let Location::Id(id) = location {
            let storage = if let Some(storage) = color_to_storage.get(color) {
                // If this color was already allocated a storage, use that
                *storage
            } else {
                // Color hasn't been seen before - has to be past the end of
                // the register colors because we prefill the map with
                // register colors. Allocate it a new stack storage (or
                // GC stack storage if its a tuple)
                let s =
                    if let Some(ValueType::TupleType(_)) = types.get(&AssignDest::Id(id.clone())) {
                        let stg = Storage::GCStack(curr_gc_stack_offset);
                        curr_gc_stack_offset += 8;
                        stg
                    } else {
                        curr_stack_offset -= 8;
                        Storage::Stack(curr_stack_offset)
                    };

                color_to_storage.insert(*color, s);
                s
            };

            id_to_storage.insert(id.clone(), storage);
        } else if let Location::Reg(r) = location
            && CALLEE_SAVED_REGISTERS.contains(r)
        {
            // Silly hack to make the callee-saved registers work
            // properly if they're used but not assigned to a
            // variable... TODO: Fix this..
            id_to_storage.insert(Identifier::new_ephemeral(), Storage::Reg(*r));
        }
    }

    // Offset is negative because stack grows down. We negate it to get
    // the stack size.
    let stack_size = -curr_stack_offset;
    let gc_stack_size = curr_gc_stack_offset;
    AllocateStorageResult {
        id_to_storage,
        stack_size,
        gc_stack_size,
    }
}

fn replace_arg_with_allocated(
    arg: &mut Arg,
    id_to_stg: &HashMap<Identifier, Storage>,
    stack_offset: i32,
) {
    match arg {
        Arg::Variable(id) => {
            if let Some(storage) = id_to_stg.get(id) {
                *arg = storage.with_stack_offset(stack_offset).to_arg()
            } else {
                panic!("No storage found for variable: {id:?}");
            }
        }
        Arg::Immediate(_) | Arg::Reg(_) | Arg::ByteReg(_) | Arg::Global(_) | Arg::Deref(_, _) => {
            // All of these argument forms should've been *unchanged* by
            // register allocation, as they all refer to absolute
            // things. They do *participate* in regalloc for graph
            // coloring purposes, but they should NOT have been
            // reassigned to a different location
        }
    }
}
