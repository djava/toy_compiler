# `bobc`

Compiler for a small statically-typed language, built for WPI's CS4533 (Programming Language Translation) as taught by Matthew Ahrens. The compiler is based on the textbook *Essentials of Compilation: An Incremental Approach in Python* by Jeremy G. Siek, but written from scratch in Rust rather than using the provided Python infrastructure.

## The Language

Source files use the `.bob` extension. The language contains: functions, variables, arithmetic, conditionals, loops, first-class functions, closures, tuples, and arrays. Types are `int`, `bool`, `tuple<...>`, `array<T, N>`, and `Callable<[args], ret>`. A short example:

```
fn factorial(n: int) -> int {
    result = 1
    while n > 0 {
        result = result * n
        n = n - 1
    }
    return result
}

fn main() -> int {
    print_int(factorial(read_int()))
}
```

Built-in functions are `read_int()`, `print_int(x)`, and `len(x)` (which takes array or tuple). There's no `print_bool` or string type. The runtime is a C library linked alongside the compiled output.

## Compilation Pipeline

The compiler works in three stages: AST passes, a translation to a flat IR, then x86 code generation and cleanup. Each stage is a sequence of individual passes, and the pipeline can be run with or without optional optimization passes.

### AST Passes

1. **GlobalizeIdentifiers** — Determines which identifiers should be treated as globals rather than locals, and makes them so. 

2. **TypeCheck** — Walks the AST and infers/checks types. Runs after each pass which adds identifiers or changes their types (after GlobalizeIdentifiers, ClosurizeFunctions and RemoveComplexOperands). Uses a type environment that maps identifiers to `ValueType`.

3. **ClosurizeFunctions** — Prepares every named top-level function for the uniform closure calling convention. Inserts an empty captures tuple as the first argument and replaces references to functions with `Expr::Closure` nodes.

4. **ClosurizeLambdas** — Converts lambda expressions into top-level functions that close over their free variables. The captured variables get bundled into a tuple that becomes the closure's first argument.

5. **DisambiguateSubscript** — Rewrites `x[i]` expressions to distinguish between tuple subscripts (which have statically-known integer indices) and array subscripts (which are dynamic). This affects how later passes generate allocation and access code.

6. **ShortCircuiting** — Rewrites `&&` and `||` into nested ternary expressions so that evaluation is lazy.

7. **PartialEval** *(optional, only in `-O` mode)* — Constant folding and dead code elimination. Folds operations on known constants, applies algebraic identities like `x * 0 = 0` and `x + 0 = x`, and eliminates branches with statically-known conditions.

8. **TupleizeExcessArgs** — The ABI passes the first six arguments in registers. Functions with more than six parameters get their excess arguments packed into a tuple, which is passed as the sixth argument. This is used instead of the typical stack slots for argument passing for simplicity.

9. **RemoveComplexOperands** — Ensures every operand to every operation is "atomic" — either a variable or a constant. Any complex sub-expression gets lifted into a fresh temporary assignment before the containing statement.

10. **InjectAllocations** — Replaces tuple, array, and closure construction with explicit heap allocation sequences: check `free_ptr` against `fromspace_end`, call `gc_collect` if needed, bump the allocation pointer, then initialize each element by subscript assignment.

11. **DeclosurizeCalls** — Rewrites all call expressions to extract the function pointer from the closure tuple and pass the closure as the first argument, implementing the actual calling convention.

### IR Translation

1. **TranslateASTtoIR** — Converts the AST to a flat, block-structured IR. Control flow (if/while) becomes explicit jumps between labeled basic blocks. The IR has no nested expressions; everything is either an atom (variable or constant) or a single binary/unary operation.

### x86 Code Generation

1. **TranslateIRtoX86** — Converts IR instructions to x86-64 instructions using abstract `Variable` arguments instead of real registers or stack slots. Multiplication uses `imulq`, division uses `cqto` + `idivq` (with the dividend sign-extended into `rdx:rax`), and shifts use `salq`/`sarq` with the shift amount in `%cl`.

### x86 Cleanup Passes

1. **RegisterAllocation** — Replaces every `Variable` argument with a real register or stack slot. First runs liveness analysis (backward dataflow over the CFG) to determine which variables have interfering lifetimes. Then builds an interference graph and colors it greedily, biasing moves so that move-related variables end up in the same location when possible. Overflows to the stack when registers run out. Tuple pointers are kept on a separate GC root stack so the garbage collector can find them across function calls.

2. **PatchInstructions** — Fixes up instruction formats that x86 doesn't actually support, usually by adding a `mov` to a temporary register before/after the instruction (ie. an instruction with two memory dereference arguments gets split into a `mov` then the instruction). Also elides trivial moves.

3. **PreludeConclusion** — Wraps each function with its stack frame setup and teardown: saving callee-saved registers, allocating stack space, initializing the GC root stack on entry to `main`, and restoring everything on exit.

4. **ResolveWidth** — Reconciles mismatched argument widths within each instruction, inserting `movsx`/`movzx` extension instructions where needed and truncating immediates to fit their target width.

5. **RemoveJumps** *(optional)* — Removes unconditional jumps to the immediately following block.

6. **OptimizeFallthrough** *(optional)* — Reorders basic blocks so that the most common fall-through path doesn't need a jump at all.

## Runtime

The runtime is a C library in `runtime/`. It provides `read_int`, `print_int`, and the garbage collector. The GC is a semi-space (Cheney's algorithm) copying collector. On each collection, it copies all live objects from fromspace into tospace, updating all pointers on the root stack. Heap and root stack sizes are both 32KB by default. The runtime is almost entirely taken from Siek's textbook supporting materials

## Building and Running

```sh
# Compile a .bob file to x86-64 assembly
cargo run input.bob -o output.s

# With optimization
cargo run input.bob -o output.s -O

# Inspect intermediate representations
cargo run --emit-ast input.bob
cargo run --emit-ir input.bob

# Assemble and link (Linux, with runtime)
gcc output.s runtime/runtime.c -o program
./program
```

## Tests

Tests are in `crates/compiler/tests/`. Each test compiles a source snippet, runs it through an x86 interpreter (in `crates/test-support/`), and checks the output against expected values. Every test case runs both with and without optimization to verify both pipelines produce the same result.

```sh
cargo test
```
