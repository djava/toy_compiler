use crate::syntax_trees::{x86::Register};

// Block labels
pub const LABEL_USER_ENTRY: &str = "user_entry";
pub const LABEL_USER_EXIT: &str = "user_exit";
pub const LABEL_MAIN: &str = "main";
pub const LABEL_EXIT: &str = "exit";

// GC runtime symbols
pub const GC_COLLECT: &str = "__gc_collect";
pub const GC_FREE_PTR: &str = "__gc_free_ptr";
pub const GC_FROMSPACE_BEGIN: &str = "__gc_fromspace_begin";
pub const GC_FROMSPACE_END: &str = "__gc_fromspace_end";
pub const GC_ROOTSTACK_BEGIN: &str = "__gc_rootstack_begin";
pub const GC_ROOTSTACK_END: &str = "__gc_rootstack_end";
pub const GC_INITIALIZE: &str = "__gc_initialize";

// Built-in function names
pub const FN_PRINT_INT: &str = "print_int";
pub const FN_READ_INT: &str = "read_int";
pub const FN_LEN: &str = "len";

// Size of a machine word in bytes (= size of i64)
pub const WORD_SIZE: i64 = 8;

// Maximum number of elements in a tuple.
// Also determines the width of the pointer_mask field in TupleTag.
// NOTE: syntax_trees/shared.rs TupleTag bitfield and parser/parse_tree.rs PEG grammar
// use this value as a literal because their macro systems require it.
pub const MAX_TUPLE_ELEMENTS: usize = 50;

// Tuple tag field extraction constants (see TupleTag in syntax_trees/shared.rs)
pub const TUPLE_LENGTH_TAG_MASK: i64 = 0x03F;
pub const TUPLE_LENGTH_TAG_SHIFT: i64 = 1; // skip the forwarding bit

// Maximum number of function arguments passed in registers (System V AMD64 ABI)
pub const MAX_REGISTER_ARGS: usize = 6;

// GC configuration
pub const GC_STACK_SIZE: i64 = 0x8000;
pub const GC_HEAP_SIZE: i64 = 0x8000;

// In x86, %rsp must always be 16-byte-aligned
pub const STACK_ALIGNMENT: i32 = 16;

// As per the calling convention, these registers are saved + restored
// by the callee if they are changed
pub const CALLEE_SAVED_REGISTERS: [Register; 7] = [
    Register::rsp,
    Register::rbp,
    Register::rbx,
    Register::r12,
    Register::r13,
    Register::r14,
    Register::r15,
];

// As per the calling convention, these registers are saved + restored
// by the caller if they are changed
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

// As per the calling convention, arguments to a function are passed in
// these registers first, in this order
pub const CALL_ARG_REGISTERS: [Register; MAX_REGISTER_ARGS] = [
    Register::rdi,
    Register::rsi,
    Register::rdx,
    Register::rcx,
    Register::r8,
    Register::r9,
];

// As per the calling convention, functions will return values through
// this register
pub const CALL_RETURN_REGISTER: Register = Register::rax;